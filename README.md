---

# LIMRA Lapse Predictor README

## Overview
The LIMRA Lapse Predictor is a Shiny-based dashboard tool designed to predict policy lapses in universal life insurance, with a focus on the millennial demographic. This tool is part of a capstone project and integrates various R libraries and H2O machine learning framework.

## Features
- **Predictive Model**: Utilizes XGBoost and GBM models to predict policy lapses.
- **User Inputs**: Includes sliders and pickers for user-defined parameters like age, premium, risk category, etc.
- **Visualizations**: Features ROC curves, Precision vs Recall plots, and Survival Analysis plots.
- **Association Rule Mining**: Implements the Apriori Algorithm to uncover relationships between variables.
- **Interactive Elements**: Shiny Widgets and Plotly for dynamic user interaction.

## Installation
- Requires R and Shiny server.
- Dependencies: `flexdashboard`, `shinyWidgets`, `lime`, `arulesViz`, `tidyverse`, `survival`, `h2o`, etc.
- Data: The tool uses LIMRA dataset, pre-processed and stored as RDS files.

## Usage
1. **Launch Dashboard**: Run the R script to start the Shiny app.
2. **Input Parameters**: Use the provided widgets to input policyholder's details.
3. **View Predictions and Analysis**: Interact with the visualizations for insights on policy lapse probability.
4. **Business Strategies**: Based on prediction, review recommended business strategies.

## Data Handling
- Data preprocessing steps are included in the script.
- Data is loaded from CSV and RDS files, ensuring efficient handling of large datasets.

## Additional Notes
- The tool was specifically tailored for millennial policyholders' data.
- Aimed at providing actionable insights for insurance companies to reduce lapse rates.

For detailed documentation, refer to the script comments and inline documentation within the dashboard.

---
