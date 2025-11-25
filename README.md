# Australian Wine Sales Time Series App

This Shiny application was developed for the ADS 506 (Applied Time Series Analysis) Module 5 assignment. Its primary purpose is to allow users to interactively explore, build, and compare time series models for Australian wine sales data (1980–1994).

The app facilitates a complete modeling workflow from exploratory data analysis and decomposition to model training, validation against hold-out sets, and future forecasting.

# How to Run This App

This project uses `renv` to manage R package dependencies, ensuring a reproducible environment.  The app is hosted on [Posit Connect](https://019ab7f1-2a81-e275-5edd-087b0d3fde19.share.connect.posit.cloud/).  To reproduce it and tailor it to your specific use case, follow the steps below

1)  Clone the repository to your local machine.

2)  Open the project in RStudio (open the .Rproj file if available, or set the working directory to the project root).

3)  Restore the environment:\
    Run the following command in the R console to install the exact package versions specified in the `renv.lock` file:

``` r
renv::restore()
```

4)  Run the App: Open `app.R` and click the Run App button, or run:

``` r
shiny::runApp()
```

# App Layout & Functionality

The application is divided into four main tabs, designed to guide the user through the time series analysis pipeline.

## 1. Explore

-   **Time Series Visualization**: View sales history for different wine varietals (e.g., Fortified, Dry White, Red, Sparkling) over custom date ranges.

-   **STL Decomposition**: Decompose any selected varietal into its Trend, Seasonality, and Remainder components to understand underlying patterns.

-   **Summary Statistic**s: View descriptive statistics (Mean, SD, Min, Max) for the selected data.

## 2. Model Building

-   **Model Setup**: Define a forecast horizon (1–5 years) which automatically splits the data into training and validation sets.

-   **Build Models**: Train ETS, ARIMA, or TSLM models.

    -   **Auto**: Let the algorithm select the best parameters.

    -   **Custom**: Manually tune parameters (e.g., set ARIMA p,d,q/P,D,Q or choose ETS error/trend/season types).

-   **Model Report**: View detailed model specifications (coefficients, smoothing parameters) and fit statistics.

-   **Training Metrics**: Compare training accuracy (RMSE, MAE, MAPE, etc.) across multiple built models.

## 3. Validation

-   **Performance Comparison**: Validate your trained models against the "hold-out" data defined by your forecast horizon.

-   **Metrics Table**: Compare out-of-sample accuracy metrics to see which model generalizes best.

-   **Visual Validation**: Plot the model forecasts overlaying the actual historical data to visually assess fit and bias.

## 4. Forecast

-   **Future Projection**: Select your best-performing models to forecast sales into the unknown future (up to 24 months).

-   **Refitting**: Note that models selected here are automatically refitted to the full data set (training + validation) before generating future forecasts.

# Credits & References

-   **Data Story Context**: Insights regarding the structural decline of fortified wines were drawn from ["The Rise and Fall of Fortified Wine Popularity" at Boobota Wine](https://wine.boobota.com/the-rise-and-fall-of-fortified-wine-popularity).

-   **Development Assistance**: Code structure and troubleshooting assistance provided by Gemini 3.

-   **Data Source**: The data set is sourced from the forecast package by [Rob Hyndman: Australian total wine sales by wine makers](https://pkg.robjhyndman.com/forecast/reference/wineind.html#source).
