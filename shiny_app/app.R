##############################
# ------ Env Set Up ------
##############################

library(shiny)
library(fpp3)      # brings in tsibble, feasts, fable, etc.
library(tidyverse)
library(gt)
library(here)
library(patchwork) # For arranging multiple plots
library(bslib)     # For collapsible sidebar layouts
library(bsicons)   # For tooltip icons

##############################
# ---- Data Import -----
##############################

aus_wine <- read_csv(
    here::here("data/AustralianWines.csv"),
    na = "*",
    col_types = cols(Rose = col_number()),
    show_col_types = FALSE
) |>
    fill(Rose, .direction = "down") |>
    mutate(Month = mdy(str_replace(Month, "-", "-01-")) |> yearmonth()) |>
    rename(Sparkling = sparkling)

wine_ts <- aus_wine |>
    pivot_longer(
        cols = c(Fortified:`Dry white`),
        names_to = "Varietal",
        values_to = "Sales"
    ) |>
    as_tsibble(
        index = Month,
        key = Varietal
    )

varietal_choices <- wine_ts |> distinct(Varietal) |> arrange(Varietal) |> pull(Varietal)
min_month <- min(wine_ts$Month)
max_month <- max(wine_ts$Month)
year_choices <- wine_ts |> distinct(year = year(Month)) |> arrange(year) |> pull(year)

##############################
# ---- UI ----
##############################

ui <- fluidPage(
    # Upgrade to Bootstrap 5 to support bslib components like layout_sidebar
    theme = bs_theme(version = 5),
    
    titlePanel("Australian Wine Sales: Explore, Model, Forecast"),
    
    tabsetPanel(
        
        ##############################
        # ----- Tab 1: Explore ------
        ##############################
        tabPanel(
            "Explore",
            
            layout_sidebar(
                fillable = FALSE,
                sidebar = sidebar(
                    title = "Explore Options",
                    open = "open",
                    
                    h4("Explore time series"),
                    
                    checkboxGroupInput(
                        inputId = "explore_varietals",
                        label = "Select varietals",
                        choices = varietal_choices,
                        selected = varietal_choices
                    ),
                    
                    dateRangeInput(
                        inputId = "explore_daterange",
                        label = "Select date range",
                        start = as.Date(yearmonth(min_month)),
                        end   = as.Date(yearmonth(max_month)),
                        min   = as.Date(yearmonth(min_month)),
                        max   = as.Date(yearmonth(max_month))
                    ),
                    
                    radioButtons(
                        inputId = "explore_view",
                        label = "View type",
                        choices = c("Time series" = "ts", "STL decomposition" = "stl"),
                        selected = "ts"
                    ),
                    
                    # Dynamic UI for STL selection
                    uiOutput("explore_stl_ui")
                ),
                
                # --- Main Content ---
                h3("Time Series & Decomposition"),
                plotOutput("explore_plot", height = "500px"),
                hr(),
                h3("Summary statistics"),
                gt_output("explore_summary")
            )
        ),
        
        ##############################
        # ----- Tab 2: Modeling -----
        ##############################
        tabPanel(
            "Model Building",
            
            layout_sidebar(
                fillable = FALSE,
                sidebar = sidebar(
                    title = "Model Options",
                    open = "open",
                    
                    h4("Model setup"),
                    
                    selectInput(
                        inputId = "model_varietal",
                        label = "Select varietal",
                        choices = varietal_choices,
                        selected = varietal_choices[1]
                    ),
                    
                    radioButtons(
                        inputId = "model_type",
                        label = "Model type",
                        choices = c(
                            "ETS"   = "ETS",
                            "ARIMA" = "ARIMA",
                            "TSLM"  = "TSLM"
                        ),
                        selected = "ETS"
                    ),
                    
                    # --- New Customization Toggle ---
                    checkboxInput(
                        inputId = "model_customize",
                        label = "Customize Model Parameters",
                        value = FALSE
                    ),
                    
                    # Dynamic UI for Custom Params
                    uiOutput("model_custom_ui"),
                    
                    sliderInput(
                        inputId = "model_horizon_years",
                        label = "Forecast horizon (years, also defines training cutoff)",
                        min = 1,
                        max = 5,
                        value = 2,
                        step = 1
                    ),
                    
                    actionButton(
                        inputId = "build_model",
                        label = "Build model",
                        class = "btn-primary"
                    ),
                    
                    hr(),
                    h4("Model Comparison"),
                    selectInput(
                        inputId = "mb_metric_models",
                        label   = "Compare training metrics for models",
                        choices = NULL,      # filled from server
                        selected = NULL,
                        multiple = TRUE
                    )
                ),
                
                # --- Main Content ---
                h3("Training data and model"),
                
                verbatimTextOutput("model_training_info"),
                
                h4("Model report (Last Built)"),
                verbatimTextOutput("model_report"),
                
                hr(),
                h3("Model Build History"),
                gt_output("model_history_table"),
                
                hr(),
                h3("Training accuracy metrics"),
                gt_output("model_training_accuracy")
            )
        ),
        
        ##############################
        # ----- Tab 3: Forecasting ---
        ##############################
        tabPanel(
            "Forecasting",
            
            layout_sidebar(
                fillable = FALSE,
                sidebar = sidebar(
                    title = "Forecast Options",
                    open = "open",
                    
                    h4("Forecasting & validation"),
                    
                    uiOutput("forecast_model_choices_ui"),
                    
                    uiOutput("forecast_clarity_year_ui"),
                    
                    actionButton(
                        inputId = "run_forecast",
                        label = "Run forecasts",
                        class = "btn-primary"
                    ),
                    
                    helpText("Select models above to compare validation metrics."),
                    helpText("NOTE: The Forecast Plot will only display the first 4 selections.")
                ),
                
                # --- Main Content ---
                h3("Validation metrics (forecast period)"),
                gt_output("forecast_metrics"),
                
                hr(),
                h3("Forecast plots"),
                plotOutput("forecast_plot", height = "800px")
            )
        )
    )
)

##############################
# ---- SERVER ----
##############################

server <- function(input, output, session) {
    
    ##############################
    # Tab 1: Explore
    ##############################
    
    explore_filtered_ts <- reactive({
        req(input$explore_varietals, input$explore_daterange)
        
        start_date <- yearmonth(input$explore_daterange[1])
        end_date   <- yearmonth(input$explore_daterange[2])
        
        wine_ts |>
            filter(
                Varietal %in% input$explore_varietals,
                Month >= start_date,
                Month <= end_date
            )
    })
    
    # --- Dynamic UI: Specific varietal selector for STL view ---
    output$explore_stl_ui <- renderUI({
        # Only show if STL view is selected
        req(input$explore_view == "stl")
        
        # Only show choices that are currently checked in the main group
        current_selection <- input$explore_varietals
        
        if (is.null(current_selection)) {
            return(helpText("Please select at least one varietal above."))
        }
        
        selectInput(
            inputId = "explore_stl_var",
            label = "Select varietal to decompose:",
            choices = current_selection,
            selected = current_selection[1]
        )
    })
    
    output$explore_plot <- renderPlot({
        df <- explore_filtered_ts()
        req(df)
        
        if (input$explore_view == "ts") {
            # Plot all selected time series faceted
            df |>
                autoplot(Sales) +
                facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
                theme_minimal() +
                labs(
                    title = "Wine sales over time",
                    x = "Month",
                    y = "Sales"
                )
        } else {
            # STL View Logic
            req(input$explore_stl_var) # Wait for the dynamic input to exist
            
            # Ensure the selected STL varietal is actually in the data 
            # (handles race condition when unchecking boxes)
            if (!input$explore_stl_var %in% df$Varietal) return(NULL)
            
            # Filter to the single varietal chosen in the secondary dropdown
            df_one <- df |>
                filter(Varietal == input$explore_stl_var)
            
            df_one |>
                model(STL(Sales)) |>
                components() |>
                autoplot() +
                theme_minimal() +
                labs(
                    title = paste("STL decomposition for", input$explore_stl_var),
                    x = "Month"
                )
        }
    })
    
    output$explore_summary <- render_gt({
        df <- explore_filtered_ts()
        req(df)
        
        df |>
            as_tibble() |>
            group_by(Varietal) |>
            summarise(
                Observations = n(),
                Mean_Sales   = mean(Sales, na.rm = TRUE),
                SD_Sales     = sd(Sales, na.rm = TRUE),
                Min_Sales    = min(Sales, na.rm = TRUE),
                Max_Sales    = max(Sales, na.rm = TRUE),
                .groups = "drop"
            ) |>
            gt() |>
            fmt_number(
                columns = c(Mean_Sales, SD_Sales, Min_Sales, Max_Sales),
                decimals = 2
            ) |>
            tab_header(
                title = "Summary statistics by varietal",
                subtitle = "Based on selected varietals and date range"
            )
    })
    
    ##############################
    # Tab 2: Modeling
    ##############################
    
    built_models    <- reactiveVal(list())
    built_horizons  <- reactiveVal(list())
    model_history   <- reactiveVal(tibble(
        ModelName = character(),
        Varietal  = character(),
        Type      = character(),
        Horizon   = numeric(),
        TrainingCutoff = numeric(),
        BuiltAt   = as.POSIXct(NA)[-1]
    ))
    
    # --- Dynamic UI: Custom Model Parameters ---
    output$model_custom_ui <- renderUI({
        req(input$model_customize)
        
        type <- input$model_type
        
        if (type == "ETS") {
            tagList(
                div(
                    class = "d-flex align-items-center",
                    selectInput("ets_error", "Error Type", 
                                choices = c("Auto" = "Z", "Additive" = "A", "Multiplicative" = "M"), 
                                width = "100%"),
                    tooltip(bs_icon("info-circle", class="ms-2"), 
                            "Controls how error is applied. 'Auto' lets the model decide.")
                ),
                div(
                    class = "d-flex align-items-center",
                    selectInput("ets_trend", "Trend Type", 
                                choices = c("Auto" = "Z", "None" = "N", "Additive" = "A", "Multiplicative" = "M", "Damped" = "Ad"),
                                width = "100%"),
                    tooltip(bs_icon("info-circle", class="ms-2"), 
                            "Controls the trend component. 'Damped' flattens the trend over time.")
                ),
                div(
                    class = "d-flex align-items-center",
                    selectInput("ets_season", "Seasonality", 
                                choices = c("Auto" = "Z", "None" = "N", "Additive" = "A", "Multiplicative" = "M"),
                                width = "100%"),
                    tooltip(bs_icon("info-circle", class="ms-2"), 
                            "Controls seasonal patterns. 'Multiplicative' is good for expanding variance.")
                )
            )
        } else if (type == "ARIMA") {
            tagList(
                h6("Non-Seasonal (p, d, q)"),
                div(
                    class = "row g-2",
                    div(class="col-4", numericInput("arima_p", "p", value = NA, min = 0)),
                    div(class="col-4", numericInput("arima_d", "d", value = NA, min = 0)),
                    div(class="col-4", numericInput("arima_q", "q", value = NA, min = 0))
                ),
                tooltip(bs_icon("info-circle"), "Autoregressive (p), Differencing (d), Moving Average (q). Leave blank for Auto."),
                
                h6("Seasonal (P, D, Q)"),
                div(
                    class = "row g-2",
                    div(class="col-4", numericInput("arima_P", "P", value = NA, min = 0)),
                    div(class="col-4", numericInput("arima_D", "D", value = NA, min = 0)),
                    div(class="col-4", numericInput("arima_Q", "Q", value = NA, min = 0))
                ),
                tooltip(bs_icon("info-circle"), "Seasonal components. Leave blank to let the model decide.")
            )
        } else if (type == "TSLM") {
            tagList(
                div(
                    class = "d-flex align-items-center mb-2",
                    checkboxInput("tslm_trend", "Include Trend", value = TRUE),
                    tooltip(bs_icon("info-circle", class="ms-2"), "Captures long-term increase or decrease.")
                ),
                div(
                    class = "d-flex align-items-center",
                    checkboxInput("tslm_season", "Include Seasonality", value = TRUE),
                    tooltip(bs_icon("info-circle", class="ms-2"), "Captures repeating yearly patterns.")
                )
            )
        }
    })
    
    observeEvent(input$build_model, {
        req(input$model_varietal, input$model_type, input$model_horizon_years)
        
        horizon_years <- input$model_horizon_years
        last_year     <- max(year(wine_ts$Month), na.rm = TRUE)
        trn_cutoff_year <- last_year - horizon_years
        
        trn_data <- wine_ts |>
            filter(year(Month) <= trn_cutoff_year)
        
        varietal_choice <- input$model_varietal
        type <- input$model_type
        is_custom <- input$model_customize
        
        # --- Dynamic Model Definition ---
        model_spec <- if (!is_custom) {
            # DEFAULT AUTO MODES
            switch(type,
                   "ETS"   = ETS(Sales),
                   "ARIMA" = ARIMA(Sales),
                   "TSLM"  = TSLM(Sales ~ trend() + season())
            )
        } else {
            # CUSTOM MODES
            if (type == "ETS") {
                # Use "Z" (Auto) if input is missing or set to Z
                e <- if(is.null(input$ets_error)) "Z" else input$ets_error
                t <- if(is.null(input$ets_trend)) "Z" else input$ets_trend
                s <- if(is.null(input$ets_season)) "Z" else input$ets_season
                ETS(Sales ~ error(e) + trend(t) + season(s))
                
            } else if (type == "ARIMA") {
                # Default search ranges in fable if input is NA:
                # p=0:5, d=0:2, q=0:5, P=0:2, D=0:1, Q=0:2
                p_arg <- if(is.na(input$arima_p)) 0:5 else input$arima_p
                d_arg <- if(is.na(input$arima_d)) 0:2 else input$arima_d
                q_arg <- if(is.na(input$arima_q)) 0:5 else input$arima_q
                
                P_arg <- if(is.na(input$arima_P)) 0:2 else input$arima_P
                D_arg <- if(is.na(input$arima_D)) 0:1 else input$arima_D
                Q_arg <- if(is.na(input$arima_Q)) 0:2 else input$arima_Q
                
                ARIMA(Sales ~ pdq(p=p_arg, d=d_arg, q=q_arg) + PDQ(P=P_arg, D=D_arg, Q=Q_arg))
                
            } else if (type == "TSLM") {
                # Build formula string
                f <- "Sales ~ 1" # Intercept
                if (isTRUE(input$tslm_trend))  f <- paste(f, "+ trend()")
                if (isTRUE(input$tslm_season)) f <- paste(f, "+ season()")
                
                TSLM(as.formula(f))
            }
        }
        
        varietal_model <- trn_data |>
            filter(Varietal == varietal_choice) |>
            model(Model = model_spec)
        
        # Generate a distinct name for custom models
        suffix <- if(is_custom) " (Custom)" else ""
        key_name <- paste0(varietal_choice, " - ", type, suffix)
        
        # store model
        current_models <- built_models()
        current_models[[key_name]] <- varietal_model
        built_models(current_models)
        
        # store horizon
        current_horizons <- built_horizons()
        current_horizons[[key_name]] <- horizon_years
        built_horizons(current_horizons)
        
        # update model history table
        current_hist <- model_history()
        new_row <- tibble(
            ModelName = key_name,
            Varietal  = varietal_choice,
            Type      = paste0(type, suffix),
            Horizon   = horizon_years,
            TrainingCutoff = trn_cutoff_year,
            BuiltAt   = Sys.time()
        )
        model_history(bind_rows(current_hist, new_row))
    })
    
    output$model_training_info <- renderText({
        req(input$model_varietal, input$model_horizon_years)
        
        horizon_years   <- input$model_horizon_years
        last_year       <- max(year(wine_ts$Month), na.rm = TRUE)
        trn_cutoff_year <- last_year - horizon_years
        
        paste0(
            "Varietal: ", input$model_varietal, "\n",
            "Model type: ", input$model_type, "\n",
            "Forecast horizon (years): ", horizon_years, "\n",
            "Training data includes all observations up to and including year ",
            trn_cutoff_year, ". Forecast/validation period: ",
            trn_cutoff_year + 1, " to ", last_year, "."
        )
    })
    
    # --- Model report: just the most recent model ---
    output$model_report <- renderPrint({
        current_models <- built_models()
        if (length(current_models) == 0) {
            cat("No model built yet. Click 'Build model' to create one.")
        } else {
            last_key <- tail(names(current_models), 1)
            cat("Model report for:", last_key, "\n\n")
            report(current_models[[last_key]])
        }
    })
    
    # --- Model History Table ---
    output$model_history_table <- render_gt({
        hist_df <- model_history()
        if (nrow(hist_df) == 0) {
            return(gt(tibble(Message = "No models built yet.")))
        }
        
        hist_df |>
            arrange(desc(BuiltAt)) |>  
            select(ModelName, Varietal, Type, Horizon, TrainingCutoff) |>
            gt() |>
            fmt_number(
                columns = c(Horizon, TrainingCutoff),
                decimals = 0,
                use_seps = FALSE       # Prevents "2,018" for years
            ) |>
            tab_header(
                title = "Built Models Log",
                subtitle = "Specs and Training Details"
            )
    })
    
    # --- Update choices for metrics comparison selectInput ---
    observe({
        current_models <- built_models()
        if (length(current_models) == 0) {
            updateSelectInput(
                session,
                inputId = "mb_metric_models",
                choices = character(0),
                selected = character(0)
            )
        } else {
            model_names <- names(current_models)
            updateSelectInput(
                session,
                inputId = "mb_metric_models",
                choices = model_names,
                selected = model_names
            )
        }
    })
    
    # --- Training metrics: compare across selected models ---
    output$model_training_accuracy <- render_gt({
        current_models <- built_models()
        if (length(current_models) == 0) {
            return(
                gt(tibble(Message = "No models built yet."))
            )
        }
        
        selected_models <- input$mb_metric_models
        if (is.null(selected_models) || length(selected_models) == 0) {
            return(
                gt(tibble(Message = "Select one or more models to compare."))
            )
        }
        
        # compute accuracy for each selected model
        acc_list <- map(selected_models, function(k) {
            mdl <- current_models[[k]]
            if (is.null(mdl)) return(NULL)
            
            mdl |>
                accuracy() |>
                mutate(ModelName = k)
        })
        
        acc_df <- acc_list |> compact() |> bind_rows()
        if (nrow(acc_df) == 0) {
            return(
                gt(tibble(Message = "No accuracy results available for selected models."))
            )
        }
        
        acc_clean <- acc_df |>
            select(ModelName, .model, .type, everything()) |>
            rename(
                Model = .model,
                Type  = .type
            )
        
        acc_clean |>
            gt() |>
            fmt_number(
                columns = where(is.numeric),
                decimals = 2
            ) |>
            tab_header(
                title = "Training accuracy metrics",
                subtitle = "Compared across selected models"
            )
    })
    
    ##############################
    # Tab 3: Forecasting
    ##############################
    
    output$forecast_model_choices_ui <- renderUI({
        current_models <- built_models()
        if (length(current_models) == 0) {
            return(helpText("No models built yet. Build at least one model in the 'Model Building' tab."))
        }
        
        checkboxGroupInput(
            inputId = "forecast_models",
            label = "Select models to compare",
            choices = names(current_models),
            selected = head(names(current_models), 1)
        )
    })
    
    output$forecast_clarity_year_ui <- renderUI({
        current_models <- built_models()
        current_hz <- built_horizons()
        
        if (length(current_models) == 0) {
            return(NULL)
        }
        
        chosen <- input$forecast_models
        if (is.null(chosen) || length(chosen) == 0) {
            return(NULL)
        }
        
        hz_values <- unlist(current_hz[chosen])
        horizon_years <- max(hz_values, na.rm = TRUE)
        
        last_year <- max(year(wine_ts$Month), na.rm = TRUE)
        max_year_allowed <- last_year - horizon_years
        
        valid_years <- year_choices[year_choices <= max_year_allowed]
        
        if (length(valid_years) == 0) {
            return(helpText("No clarity cutoff years available (forecast period covers all data)."))
        }
        
        selectInput(
            inputId = "forecast_clarity_year",
            label = "Optional: clarity cutoff year for plotting historical data",
            choices = c("None" = "", valid_years),
            selected = ""
        )
    })
    
    forecast_results <- eventReactive(input$run_forecast, {
        current_models <- built_models()
        current_hz <- built_horizons()
        req(length(current_models) > 0, input$forecast_models)
        
        chosen_keys <- input$forecast_models
        # Note: We allow generating metrics for ALL selected models (up to 10 logic in table)
        # but the plot will restrict itself.
        
        res <- list()
        
        for (k in chosen_keys) {
            m <- current_models[[k]]
            hz_years <- current_hz[[k]]
            
            h_months <- hz_years * 12
            
            fc <- m |>
                forecast(h = h_months)
            
            res[[k]] <- list(
                model      = m,
                forecast   = fc,
                horizon_yr = hz_years
            )
        }
        
        res
    })
    
    output$forecast_metrics <- render_gt({
        res <- forecast_results()
        if (length(res) == 0) {
            return(gt(tibble(Message = "No forecasts computed yet.")))
        }
        
        # Limit metrics table to 10 for readability if many selected
        keys_to_show <- head(names(res), 10)
        
        all_metrics <- map2(
            keys_to_show, res[keys_to_show],
            ~ {
                k  <- .x
                fc <- .y$forecast
                
                fc |>
                    accuracy(data = wine_ts) |>
                    mutate(ModelName = k)
            }
        ) |>
            bind_rows()
        
        metrics_clean <- all_metrics |>
            select(ModelName, .model, .type, everything()) |>
            rename(
                Model = .model,
                Type  = .type
            )
        
        metrics_clean |>
            gt() |>
            fmt_number(
                columns = where(is.numeric),
                decimals = 2
            ) |>
            tab_header(
                title = "Validation metrics over forecast period",
                subtitle = "Up to 10 selected models shown"
            )
    })
    
    output$forecast_plot <- renderPlot({
        res <- forecast_results()
        if (length(res) == 0) return(NULL)
        
        # CONSTRAINT: Only plot the top 4 chosen models
        chosen_keys <- head(names(res), 4)
        
        clarity_year <- input$forecast_clarity_year
        clarity_year <- if (!is.null(clarity_year) && nzchar(clarity_year)) as.numeric(clarity_year) else NA_real_
        
        # Iterate through selected keys and build a separate plot for each
        plot_list <- map(chosen_keys, function(k) {
            
            fc_object <- res[[k]]$forecast
            
            # Identify the varietal for this specific model's forecast
            # (The forecast object preserves the Key from the tsibble)
            var_name <- fc_object |> 
                distinct(Varietal) |> 
                pull(Varietal)
            
            # Filter historical data ONLY for this varietal
            hist_data <- wine_ts |> 
                filter(Varietal == var_name)
            
            if (!is.na(clarity_year)) {
                hist_data <- hist_data |> filter(year(Month) >= clarity_year)
            }
            
            # Create individual plot
            # autoplot(fc_object) automatically handles prediction intervals
            p <- fc_object |>
                autoplot() +
                autolayer(hist_data, Sales, colour = "gray20") +
                theme_minimal() +
                labs(
                    title = k,
                    x = NULL, # Clean up x-axis for stacked plots
                    y = "Sales"
                ) +
                theme(legend.position = "right")
            
            return(p)
        })
        
        # Stack plots vertically using Patchwork
        wrap_plots(plot_list, ncol = 1)
    })
}

##############################
# ---- Run App ----
##############################

shinyApp(ui = ui, server = server)