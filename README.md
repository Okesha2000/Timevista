# TimeVista Shiny App
## Overview
TimeVista is a Shiny application designed for time series analysis. It allows users to upload their own CSV files or select from built-in datasets to perform various types of time series analysis including data exploration, decomposition, SARIMA modeling, diagnostics, and forecasting.

## Features
Upload Data: Users can upload their own CSV files or select from built-in datasets.
Data Exploration: Visualize the time series and view summary statistics.
Decomposition: View the seasonal decomposition of the time series.
SARIMA Model: Fit and view the summary of a SARIMA model.
Diagnostics: Provides diagnostic plots for the fitted SARIMA model.
Forecasting: Specify the forecast horizon and view the forecast plot.
Dashboard: Combines decomposition, SARIMA model summary, and forecasting plots.
Help: Provides information about the app and how to use it.

## Known Drawbacks
The app might not handle very large datasets efficiently.
Only supports univariate time series analysis.
Limited options for customizing plots.
The app searches for SARIMA models using the auto.arima function which won't always produce the best model.

## Usage
Navigate to the 'Dataset' tab to upload your CSV file or select a built-in dataset.
Use the 'Data Exploration' tab to visualize the time series and view summary statistics.
In the 'Decomposition' tab, view the seasonal decomposition of the time series.
Use the 'SARIMA Model' tab to fit and view the summary of a SARIMA model.
The 'Diagnostics' tab provides diagnostic plots for the fitted SARIMA model.
In the 'Forecasting' tab, specify the forecast horizon and view the forecast plot.
The 'Dashboard' tab combines decomposition, SARIMA model summary, and forecasting plots.
## Author
Okesha Karunarathne
