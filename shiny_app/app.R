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

ui <- page_navbar(
  title = tags$span(
    class = "text-info fw-bold",
    HTML("🍷 Australian Wine Sales")
  ), 
  
  theme = bs_theme(version = 5, bootswatch = 'cerulean'), 
  
  # Use fillable = FALSE so your plots don't stretch weirdly if you aren't using full-screen logic
  fillable = FALSE, 
  
  ##############################
  # ----- Tab 1: Explore ------
  ##############################
  nav_panel(
    title = "Explore",
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        open = "open",
        
        h4("Explore time series"),
        
        checkboxGroupInput(
          inputId = "explore_varietals",
          label = "Select varietals",
          choices = varietal_choices,
          selected = varietal_choices[1]
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
        
        uiOutput("explore_stl_ui")
      ),
      
      # --- Main Content ---
      h3("Time Series & Decomposition"),
      plotOutput("explore_plot", height = "600px"), 
      hr(),
      
      accordion(
        open = FALSE, 
        accordion_panel(
          title = tags$span(class = "text-primary", "Summary Statistics"),
          value = 'summary_stats',
          gt_output("explore_summary")
        )
      )
    )
  ),
  
  ##############################
  # ----- Tab 2: Modeling -----
  ##############################
  nav_panel(
    title = "Model Building",
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        open = "open",
        
        h4("Model setup"),
        
        sliderInput(
          inputId = "model_horizon_years",
          label = "Forecast horizon (years)",
          min = 1,
          max = 5,
          value = 2,
          step = 1
        ),
        
        div(
          class = "alert alert-warning p-2", 
          style = "font-size: 0.85rem;",
          bs_icon("exclamation-triangle-fill"),
          "Rebuilding overwrites previous models of the same type."
        ),
        
        selectInput(
          inputId = "model_varietal",
          label = "Select varietal",
          choices = varietal_choices,
          selected = varietal_choices[1]
        ),
        
        radioButtons(
          inputId = "model_type",
          label = "Model type",
          choices = c("ETS", "ARIMA", "TSLM"),
          selected = "ETS"
        ),
        
        checkboxInput(
          inputId = "model_customize",
          label = "Customize Model Parameters",
          value = FALSE
        ),
        
        uiOutput("model_custom_ui"),
        
        actionButton(
          inputId = "build_model",
          label = "Build model",
          class = "btn-primary"
        ),
        
        hr(),
        h4("Review & Compare"),
        
        selectInput(
          inputId = "view_model_report",
          label = "Select model to view report",
          choices = NULL
        ),
        
        selectInput(
          inputId = "mb_metric_models",
          label   = "Compare training metrics",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      
      # --- Main Content ---
      h3("Training data and model"),
      verbatimTextOutput("model_training_info"),
      
      h4("Model report"),
      uiOutput("model_report_ui"),
      
      hr(),
      h3("Training accuracy metrics"),
      gt_output("model_training_accuracy"),
      hr(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = tags$span(class = "text-primary", "Model Build History"),
          value = "model_build",
          gt_output("model_history_table")
        )
      )
    )
  ),
  
  ##############################
  # ----- Tab 3: Validation ----
  ##############################
  nav_panel(
    title = "Validation",
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        open = "open",
        
        h4("Validation Setup"),
        uiOutput("forecast_model_choices_ui"),
        uiOutput("forecast_clarity_year_ui"),
        
        actionButton(
          inputId = "run_forecast",
          label = "Run validation",
          class = "btn-primary"
        ),
        helpText("Compare models against held-out data defined by the horizon.")
      ),
      
      h3("Validation metrics"),
      gt_output("forecast_metrics"),
      hr(),
      h3("Validation plots"),
      plotOutput("forecast_plot", height = "800px")
    )
  ),
  
  ##############################
  # ----- Tab 4: Forecast ------
  ##############################
  nav_panel(
    title = "Forecast",
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        open = "open",
        
        h4("Future Forecast Setup"),
        uiOutput("future_model_choices_ui"),
        
        sliderInput(
          inputId = "future_horizon_months",
          label = "Forecast Horizon (Months)",
          min = 1,
          max = 24,
          value = 12,
          step = 1
        ),
        
        uiOutput("future_clarity_year_ui"),
        
        actionButton(
          inputId = "run_future_forecast",
          label = "Run Forecast",
          class = "btn-success"
        ),
        
        div(
          class = "alert alert-info p-2 mt-3", 
          style = "font-size: 0.85rem;",
          bs_icon("info-circle"),
          "Models will be refitted to the full dataset before forecasting."
        )
      ),
      
      h3("Future Forecast Plots"),
      plotOutput("future_plot", height = "800px")
    )
  )
)

##############################
# ---- SERVER ----
##############################

server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
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
    
    # Auto-select the new model in the report viewer
    updateSelectInput(session, "view_model_report", selected = key_name)
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
  
  # --- UPDATED: Centered values, TSLM Padding & Fixes ---
  output$model_report_ui <- renderUI({
    current_models <- built_models()
    selected_key <- input$view_model_report
    
    if (length(current_models) == 0) {
      return(div(class="alert alert-warning", "No model built yet. Click 'Build model' to create one."))
    } else if (is.null(selected_key) || selected_key == "") {
      return(div(class="alert alert-info", "Select a model from the sidebar to view the report."))
    }
    
    # Retrieve model (mable)
    if (!selected_key %in% names(current_models)) {
      return(div(class="alert alert-danger", "Selected model not found."))
    }
    
    mdl_mable <- current_models[[selected_key]]
    
    if (!"Model" %in% names(mdl_mable)) {
      return(div("Model object structure invalid."))
    }
    
    model_def <- mdl_mable$Model[[1]]
    fit_obj   <- model_def$fit
    
    report_elements <- list()
    
    # 1. Header
    model_str <- format(model_def)
    report_elements[[1]] <- div(
      class = "mb-3 p-2 bg-light border rounded",
      h5("Model Structure", class="text-primary m-0"),
      p(class = "m-0", style = "font-family: monospace; font-size: 1.1em;", model_str)
    )
    
    # --- Helper: Clean TSLM Terms ---
    clean_tslm_terms <- function(nms) {
      if(is.null(nms)) return(character(0))
      
      nms <- str_replace(nms, fixed("(Intercept)"), "Intercept")
      nms <- str_replace(nms, fixed("trend()"), "Trend")
      
      # Handle season()yearX -> Map to Month
      # Extract number
      season_matches <- str_match(nms, "season\\(\\)year(\\d+)")
      
      has_season <- !is.na(season_matches[,1])
      if(any(has_season)) {
        indices <- as.numeric(season_matches[has_season, 2])
        # Indices are 1-12. Usually season()year2=Feb
        valid_idx <- indices >= 1 & indices <= 12
        
        # Use abbreviated month names (Jan, Feb...)
        nms[has_season][valid_idx] <- month.abb[indices[valid_idx]]
      }
      return(nms)
    }
    
    # --- Helper: Robust Wide Tibble Conversion (used for ETS params) ---
    to_wide_tibble <- function(x) {
      if (is.null(x)) return(tibble())
      if (!is.data.frame(x) && is.vector(x)) {
        return(as_tibble(as.list(x)))
      }
      if (is.data.frame(x)) {
        df <- as_tibble(x)
        names_lower <- tolower(names(df))
        if ("term" %in% names_lower && "estimate" %in% names_lower) {
          df <- df |> pivot_wider(names_from = matches("term", ignore.case=TRUE), 
                                  values_from = matches("estimate", ignore.case=TRUE))
        } else if ("parameter" %in% names_lower && "value" %in% names_lower) {
          df <- df |> pivot_wider(names_from = matches("parameter", ignore.case=TRUE), 
                                  values_from = matches("value", ignore.case=TRUE))
        }
        return(df)
      }
      return(as_tibble(as.list(x)))
    }
    
    # 2. Extract Details based on Type
    if (inherits(fit_obj, "ETS")) {
      
      # Parameters Table (Wide)
      if (!is.null(fit_obj$par)) {
        df_par <- to_wide_tibble(fit_obj$par)
        
        # Capitalize columns
        names(df_par) <- str_to_title(names(df_par))
        
        if ("L" %in% names(df_par)) {
          df_par <- df_par |> rename(`Level (Intercept)` = L)
        }
        
        tbl_par <- df_par |> 
          gt() |> 
          fmt_number(columns = where(is.numeric), decimals = 2) |> 
          cols_align(align = "center") |>
          tab_header(title = "Smoothing Parameters") |> 
          as_raw_html()
        
        report_elements[[2]] <- div(class="mb-4", HTML(tbl_par))
      }
      
    } else if (inherits(fit_obj, "ARIMA")) {
      
      # Coefficients (Vertical / Original format per request)
      if (!is.null(fit_obj$par)) {
        df_coef <- as_tibble(fit_obj$par)
        
        # Capitalize columns
        names(df_coef) <- str_to_title(names(df_coef))
        
        tbl_coef <- df_coef |> 
          gt() |> 
          fmt_number(columns = where(is.numeric), decimals = 2) |> 
          cols_align(align = "center") |>
          tab_header(title = "Model Coefficients") |> 
          as_raw_html()
        
        report_elements[[2]] <- div(class="mb-4", HTML(tbl_coef))
      }
      
    } else if (inherits(fit_obj, "TSLM") || inherits(fit_obj, "lm")) {
      
      # Coefficients (Wide)
      coefs <- tryCatch(coefficients(fit_obj), error = function(e) NULL)
      
      if (!is.null(coefs)) {
        # FIX: Robustly handle names if missing to prevent "Size 0" tibble error
        raw_names <- names(coefs)
        
        # If names() is NULL, it's likely a matrix/array. Try rownames/colnames.
        if (is.null(raw_names) && (is.matrix(coefs) || is.array(coefs))) {
          if (!is.null(rownames(coefs))) raw_names <- rownames(coefs)
          else if (!is.null(colnames(coefs))) raw_names <- colnames(coefs)
        }
        
        if (is.null(raw_names)) {
          # Fallback if names are missing entirely
          raw_names <- paste0("Term_", seq_along(coefs))
        }
        
        nm <- clean_tslm_terms(raw_names)
        
        # Double check matching lengths
        vals <- as.numeric(coefs)
        
        if (length(nm) == length(vals)) {
          df_coef <- tibble(
            Term = nm,
            Estimate = vals
          ) |>
            pivot_wider(names_from = Term, values_from = Estimate)
          
          # Names are already Title case from the cleaner/pivot
          
          tbl_coef <- df_coef |> 
            gt() |> 
            fmt_number(columns = where(is.numeric), decimals = 2) |> 
            cols_align(align = "center") |>
            # Add padding to TSLM columns
            cols_width(everything() ~ px(120)) |>
            tab_header(title = "Coefficients") |> 
            as_raw_html()
          
          report_elements[[2]] <- div(class="mb-4", HTML(tbl_coef))
        } else {
          report_elements[[2]] <- div(class="text-danger", "Error: Mismatched coefficient names.")
        }
      }
      
      # Goodness of Fit (Wide)
      try({
        s <- summary(fit_obj)
        df_gof <- tibble(
          `R-Squared` = s$r.squared,
          `Adj. R-Squared` = s$adj.r.squared,
          `Sigma` = s$sigma
        )
        
        tbl_gof <- df_gof |> 
          gt() |> 
          fmt_number(columns = where(is.numeric), decimals = 2) |> 
          cols_align(align = "center") |>
          tab_header(title = "Fit Statistics") |> 
          as_raw_html()
        
        report_elements[[3]] <- div(class="mb-4", HTML(tbl_gof))
      }, silent = TRUE)
      
    } else {
      # Fallback
      report_elements[[2]] <- pre(capture.output(report(mdl_mable)))
    }
    
    do.call(tagList, report_elements)
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
  
  # --- Update choices for metrics comparison AND Report View ---
  observe({
    current_models <- built_models()
    if (length(current_models) == 0) {
      updateSelectInput(session, "mb_metric_models", choices = character(0), selected = character(0))
      updateSelectInput(session, "view_model_report", choices = character(0))
    } else {
      model_names <- names(current_models)
      # Metrics comparison (multi-select)
      updateSelectInput(session, "mb_metric_models", choices = model_names, selected = model_names)
      
      # Report viewer (single-select)
      updateSelectInput(session, "view_model_report", choices = model_names, selected = input$view_model_report)
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
      select(ModelName, Varietal, RMSE, ME, c(MAE:MAPE)) |>
      arrange(RMSE)
    
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
  # Tab 3: Validation
  ##############################
  
  output$forecast_model_choices_ui <- renderUI({
    current_models <- built_models()
    if (length(current_models) == 0) {
      return(helpText("No models built yet. Build at least one model in the 'Model Building' tab."))
    }
    
    checkboxGroupInput(
      inputId = "forecast_models",
      label = "Select models to validate",
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
      return(gt(tibble(Message = "No validation forecasts computed yet.")))
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
      select(ModelName, Varietal, RMSE, ME, c(MAE:MAPE)) |>
      arrange(RMSE)
    
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
      var_name <- fc_object |> 
        distinct(Varietal) |> 
        pull(Varietal)
      
      # Filter historical data ONLY for this varietal
      hist_data <- wine_ts |> 
        filter(Varietal == var_name)
      
      if (!is.na(clarity_year)) {
        hist_data <- hist_data |> filter(year(Month) >= clarity_year)
      }
      
      # autoplot with forecast object + autolayer with historical data
      p <- fc_object |>
        autoplot() +
        autolayer(hist_data, Sales, colour = "gray20") +
        theme_minimal() +
        labs(
          title = k,
          x = NULL, 
          y = "Sales"
        ) +
        theme(legend.position = "right")
      
      return(p)
    })
    
    wrap_plots(plot_list, ncol = 1)
  })
  
  ##############################
  # Tab 4: Forecast (Future)
  ##############################
  
  output$future_model_choices_ui <- renderUI({
    current_models <- built_models()
    if (length(current_models) == 0) {
      return(helpText("No models built yet. Build at least one model in the 'Model Building' tab."))
    }
    
    checkboxGroupInput(
      inputId = "future_models",
      label = "Select models to forecast",
      choices = names(current_models),
      selected = head(names(current_models), 1)
    )
  })
  
  output$future_clarity_year_ui <- renderUI({
    # For Future Forecast, we can show any year available in history as a start point
    selectInput(
      inputId = "future_clarity_year",
      label = "Optional: clarity cutoff year for plotting historical data",
      choices = c("None" = "", year_choices),
      selected = ""
    )
  })
  
  future_results <- eventReactive(input$run_future_forecast, {
    req(input$future_models, input$future_horizon_months)
    
    current_models <- built_models()
    chosen_keys    <- input$future_models
    h_months       <- input$future_horizon_months
    
    res <- list()
    
    for (k in chosen_keys) {
      # 1. Get the original model (trained on partial data)
      old_model <- current_models[[k]]
      
      # 2. Get the specific varietal for this model to subset the FULL data correctly
      #    (The mable 'old_model' contains the Varietal as a key)
      var_key <- old_model |> distinct(Varietal) |> pull(Varietal)
      
      full_data_varietal <- wine_ts |> filter(Varietal == var_key)
      
      # 3. Refit the model to the FULL dataset.
      #    reestimate = TRUE ensures parameters are optimized for the full history
      new_model <- old_model |>
        refit(full_data_varietal, reestimate = TRUE)
      
      # 4. Forecast into the future
      fc <- new_model |>
        forecast(h = h_months)
      
      res[[k]] <- list(
        forecast = fc
      )
    }
    
    res
  })
  
  output$future_plot <- renderPlot({
    res <- future_results()
    if (length(res) == 0) return(NULL)
    
    # CONSTRAINT: Only plot the top 4 chosen models
    chosen_keys <- head(names(res), 4)
    
    clarity_year <- input$future_clarity_year
    clarity_year <- if (!is.null(clarity_year) && nzchar(clarity_year)) as.numeric(clarity_year) else NA_real_
    
    plot_list <- map(chosen_keys, function(k) {
      
      fc_object <- res[[k]]$forecast
      
      var_name <- fc_object |> 
        distinct(Varietal) |> 
        pull(Varietal)
      
      hist_data <- wine_ts |> 
        filter(Varietal == var_name)
      
      if (!is.na(clarity_year)) {
        hist_data <- hist_data |> filter(year(Month) >= clarity_year)
      }
      
      # autoplot with forecast object + autolayer with historical data
      p <- fc_object |>
        autoplot() +
        autolayer(hist_data, Sales, colour = "gray20") +
        theme_minimal() +
        labs(
          title = paste("Forecast:", k),
          x = NULL, 
          y = "Sales"
        ) +
        theme(legend.position = "right")
      
      return(p)
    })
    
    wrap_plots(plot_list, ncol = 1)
  })
}

##############################
# ---- Run App ----
##############################

shinyApp(ui = ui, server = server)