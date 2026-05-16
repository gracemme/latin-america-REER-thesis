# Thesis Work 2 - Descriptive Statistics

# Run first script if not already in environment
# source("scripts/01_prepare_data_impute_missing_values.R")

# Load packages
library(patchwork)
library(ggplot2)

# Descriptive Statistics function:
desc_stats <- function(data, vars) {
  data.frame(
    Variable = vars,
    n        = sapply(vars, function(v) sum(!is.na(data[[v]]))),
    mean     = sapply(vars, function(v) mean(data[[v]], na.rm = TRUE)),
    sd       = sapply(vars, function(v) sd(data[[v]], na.rm = TRUE)),
    min      = sapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
    q1       = sapply(vars, function(v) quantile(data[[v]], 0.25, na.rm = TRUE)),
    median   = sapply(vars, function(v) median(data[[v]], na.rm = TRUE)),
    q3       = sapply(vars, function(v) quantile(data[[v]], 0.75, na.rm = TRUE)),
    max      = sapply(vars, function(v) max(data[[v]], na.rm = TRUE)),
    IQR      = sapply(vars, function(v) IQR(data[[v]], na.rm = TRUE))
  )
}

# INDEPENDENT VAR: REER
# Descriptive Statistics:
desc_stats(tidy_data, "REER")
# Histogram:
hist(tidy_data$REER, main = "REER")
# Line plot (color-separated by country):
ggplot(tidy_data, aes(x = Year, y = REER, color = Country, group = Country)) +
  geom_line() +
  labs(title = "REER", x = "Year", y = "Value", color = "Country") +
  theme_minimal()
# RESULTS:
#    There is a general upward trend in the line chart, but excluding Argentina,
#    the variance increases over time. The histogram looks mostly normal, but has
#    a right skew.

# ADOLESCENT FERTILITY RATE, AGE DEPENDENCY RATIO, BIRTH RATE, CLAIMS ON CENTRAL GOVT,
#    CURRENT ACCOUNT, DEATH RATE
# Descriptive Statistics:
desc_stats(tidy_data, c("adol_fert", "age_dep", "birth", "claims_govt", "curr_acct",
                        "death"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$adol_fert, main = "Adolescent Fertility")
hist(tidy_data$age_dep, main = "Age Dependency")
hist(tidy_data$birth, main = "Birth Rate")
hist(tidy_data$claims_govt, main = "Claims on Central Govt")
hist(tidy_data$curr_acct, main = "Current Account")
hist(tidy_data$death, main = "Death Rate")
# Line Charts:
vars <- c("adol_fert", "age_dep", "birth", "claims_govt", "curr_acct", "death")
titles <- c("Adolescent Fertility", "Age Dependency", "Birth Rate", "Claims on Central Govt",
            "Current Account", "Death Rate")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# DISCREPANCY IN EXPENDITURE ESTIMATE, EMPLOYMENT IN AGRICULTURE, EMPLOYMENT IN INDUSTRY,
#    EMPLOYMENT IN SERVICES, EMPLOYMENT TO POPULATION RATIO, EXPORT UNIT VALUE INDEX
# Descriptive Statistics:
desc_stats(tidy_data, c("disc_exp", "agriculture", "industry", "services", "emp_pop",
                        "exp_unit_value"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$disc_exp, main = "Discrepancy in Estimate")
hist(tidy_data$agriculture, main = "Employment in Agriculture")
hist(tidy_data$industry, main = "Employment in Industry")
hist(tidy_data$services, main = "Employment in Services")
hist(tidy_data$emp_pop, main = "Employment to Population")
hist(tidy_data$exp_unit_value, main = "Export Unit Value")
# Line Charts:
vars <- c("disc_exp", "agriculture", "industry", "services", "emp_pop", "exp_unit_value")
titles <- c("Discrepancy in Estimate", "Employment in Agriculture", "Employment in Industry",
            "Employment in Services", "Employment to Population", "Export Unit Value")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# EXPORT VALUE INDEX, EXPORT VOLUME INDEX, EXPORT OF GOODS AND SERVICES, FDI FLOWS,
#    FERTILITY RATE, FINAL CONSUMPTION EXPENDITURE
# Descriptive Statistics:
desc_stats(tidy_data, c("exp_value", "exp_vol", "exp_goods", "FDI_flows", "fertility",
                        "final_cons"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$exp_value, main = "Export Value")
hist(tidy_data$exp_vol, main = "Export Volume")
hist(tidy_data$exp_goods, main = "Export of Goods and Services")
hist(tidy_data$FDI_flows, main = "FDI Flows")
hist(tidy_data$fertility, main = "Fertility Rate")
hist(tidy_data$final_cons, main = "Final Consumption Expenditure")
# Line Charts:
vars <- c("exp_value", "exp_vol", "exp_goods", "FDI_flows", "fertility", "final_cons")
titles <- c("Export Value", "Export Volume", "Export of Goods and Services",
            "FDI Flows", "Fertility Rate", "Final Consumption Expenditure")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# FIXED TELEPHONE SUBSCRIPTIONS, GENERAL GOVERNMENT FINAL CONSUMPTION, IMPORT UNIT
#    VALUE INDEX, IMPORT VALUE INDEX, IMPORT VOLUME INDEX, IMPORTS OF GOODS AND SERVICES
# Descriptive Statistics:
desc_stats(tidy_data, c("telephone", "govt_final_cons", "imp_unit_value", "imp_value", "imp_vol",
                        "imp_goods"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$telephone, main = "Telephone Subscriptions")
hist(tidy_data$govt_final_cons, main = "General Govt Final Consumption")
hist(tidy_data$imp_unit_value, main = "Import Unit Value")
hist(tidy_data$imp_value, main = "Import Value")
hist(tidy_data$imp_vol, main = "Import Volume")
hist(tidy_data$imp_goods, main = "Imports of Goods and Services")
# Line Charts:
vars <- c("telephone", "govt_final_cons", "imp_unit_value", "imp_value", "imp_vol", "imp_goods")
titles <- c("Telephone Subscriptions", "General Govt Final Consumption", "Import Unit Value",
            "Import Value", "Import Volume", "Imports of Goods and Services")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# INDIVIDUALS USING THE INTERNET, INFLATION - GDP DEFLATOR, INSURANCE AND FINANCIAL
#    SERVICES (% OF EXPORTS), INSURANCE AND FINANCIAL SERVICES (% OF IMPORTS), LIFE
#    EXPECTANCY, NET BARTER TERMS OF TRADE
# Descriptive Statistics:
desc_stats(tidy_data, c("internet", "inflation", "ins_exp", "ins_imp", "life_exp",
                        "ToT"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$internet, main = "Individuals Using the Internet")
hist(tidy_data$inflation, main = "Inflation")
hist(tidy_data$ins_exp, main = "Insurance + Financial Services (% exports)")
hist(tidy_data$ins_imp, main = "Insurance + Financial Services (% imports)")
hist(tidy_data$life_exp, main = "Life Expectancy")
hist(tidy_data$ToT, main = "Terms of Trade")
# Line Charts:
vars <- c("internet", "inflation", "ins_exp", "ins_imp", "life_exp", "ToT")
titles <- c("Individuals Using the Internet", "Inflation", "Insurance + Financial Services (% exports)",
            "Insurance + Financial Services (% imports)", "Life Expectancy", "Terms of Trade")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# NET MIGRATION, POPULATION GROWTH, POPULATION TOTAL, GDP PER CAPITA, TARIFF RATE,
#    TOTAL RESERVES
# Descriptive Statistics:
desc_stats(tidy_data, c("migration", "pop_grow", "pop", "GDPpc", "tariff",
                        "reserves"))
# Histograms:
par(mfrow = c(2,3))
hist(tidy_data$migration, main = "Net Migration")
hist(tidy_data$pop_grow, main = "Population Growth")
hist(tidy_data$pop, main = "Population Total")
hist(tidy_data$GDPpc, main = "GDP Per Capita")
hist(tidy_data$tariff, main = "Tariff Rate")
hist(tidy_data$reserves, main = "Total Reserves")
# Line Charts:
vars <- c("migration", "pop_grow", "pop", "GDPpc", "tariff", "reserves")
titles <- c("Net Migration", "Population Growth", "Population Total",
            "GDP Per Capita", "Tariff Rate", "Total Reserves")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 2, ncol = 3)

# TRADE, Unemployment
# Descriptive Statistics:
desc_stats(tidy_data, c("trade", "unemp"))
# Histograms:
par(mfrow = c(1,2))
hist(tidy_data$trade, main = "Trade")
hist(tidy_data$unemp, main = "Unemployment")
# Line Charts:
vars <- c("trade", "unemp")
titles <- c("Trade", "Unemployment")
plots <- lapply(seq_along(vars), function(x) {
  ggplot(tidy_data, aes(x = Year, y = .data[[vars[x]]], color = Country, group = Country)) +
    geom_line() +
    labs(title = titles[x], x = "Year", y = "Value", color = "Country") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9), 
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    )
})
wrap_plots(plots, nrow = 1, ncol = 2)

# Remove uneccessary environment objects
rm(list = c("titles", "vars", "plots"))