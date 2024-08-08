# Shiny Application
# Assignment 3 - Shiny Application

This project is a Shiny application for data analysis and model training. The application provides various functionalities such as data visualization, data splitting, and training different machine learning models using the [`caret`]

## Project Structure

```
Ass3Data.csv
global.R
server.R
ui.R
```

- **Ass3Data.csv**: Contains the dataset used in the application.
- **global.R**: Contains global variables and functions used across the application.
- **server.R**: Contains the server-side logic of the Shiny application.
- **ui.R**: Contains the user interface definition of the Shiny application.

## Features

### Data Tab

- **Data Summary**: Displays a summary of the dataset.
- **IQR Multiplier**: Slider input to adjust the IQR multiplier.
- **Standardise Chart**: Checkbox to standardize the chart.
- **Box Plots**: Displays box plots of the data.
- **Missing Data Plot**: Displays a plot of missing data.
- **Correlation Plot**: Displays a correlation plot.
- **Data Table**: Displays the dataset in a table format.

### Split Tab

- **Train Proportion**: Slider input to adjust the proportion of data used for training.
- **Split Summary**: Displays a summary of the data split.

### Available Methods Tab

- **Regression Methods in caret**: Displays available regression methods in the [`caret`] package.

### Methods Tab

- **Parallel Processing**: Checkbox to enable or disable parallel processing.
- **Model Training**: Provides tabs for training different models such as NULL Model, GLMnet Model, Ridge Model, Boosted Linear Model, Elasticnet, SVM Linear, and Least Angle Regression.
- **Resampled Performance**: Displays the resampled performance of the trained models.
- **Model Plots**: Displays plots of the trained models.
- **Model Summary**: Displays a summary of the trained models.

### Model Selection Tab

- **Cross Validation Results**: Displays cross-validation results.
- **Show Notch**: Checkbox to show or hide notches in the plot.
- **Normalise**: Checkbox to normalize the plot.
- **Model Choice**: Radio buttons to select the model.

### Performance Tab

- **Test Summary**: Displays a summary of the test results.
- **Test Plot**: Displays a plot of the test results.

## Usage

1. **Install Required Packages**: Ensure you have the required packages installed.
    ```r
    install.packages(c("shiny", "DT", "shinycssloaders", "caret", "bsplus"))
    ```

2. **Run the Application**: Open the project in RStudio or any R environment and run the application.
    ```r
    shiny::runApp()
    ```

## Dataset

The dataset [`Ass3Data.csv`] contains various columns such as [`ID`], [`Alcohol`], [`Coffee`], [`Exercise`], [`NumDocVisits`], [`BloodType`], [`ReagentA`] to [`ReagentN`] and [`Y`].

## License

This project is licensed under the MIT License.

