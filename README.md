# latin-america-REER-thesis
Cleaned dataset contribution and processing code for thesis analysis of real effective exchange rates in Latin America

# Determinants of the Real Effective Exchange Rate in Latin America

This repository contains the cleaned dataset and R code used for my thesis on the determinants of the real effective exchange rate (REER) in Latin America.

## Project Overview

The study analyzes a panel dataset of 16 Latin American countries from 1992 to 2023. The analysis uses variable transformations, LASSO feature selection, and several panel regression specifications, including pooled OLS, country fixed effects, time fixed effects, and two-way fixed effects models.

## Repository Structure

```text
data/
  Thesis_Tidy_Data.xlsx

scripts/
  01_prepare_data_impute_missing_values.R
  02_descriptive_statistics.R
  03_transform_select_fit_models.R

outputs/
  tables/
  figures/

docs/
  variable_notes.md
```

## How to Run the Code

Run the scripts in the following order:

1. ```scripts/01_prepare_data_impute_missing_values.R```
2. ```scripts/02_descriptive_statistics.R```
3. ```scripts/03_transform_select_fit_models.R```

The third script estimates the final model specifications and compares the pooled OLS, country fixed effects, time fixed effects, and two-way fixed effects models.

## Required R Packages

```text
install.packages(c(
  "readxl",
  "dplyr",
  "tidyr",
  "VIM",
  "patchwork",
  "ggplot2",
  "glmnet",
  "fixest"
))
```

## Data

The dataset is a cleaned panel dataset constructed from REER and World Bank indicators. The final analysis excludes 2024 due to missingness across several variables.

## Author

Grace McCullough
