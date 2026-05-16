# Thesis Work 3 - Variable Transformation, Feature Selection and Model Fitting

# Run previous scripts if not already in environment
# source("scripts/02_descriptive_statistics.R")

# Load packages
library(glmnet)
library(fixest)

# Before fitting, some variables must be transformed. Those with a significant
#    right skew and no negative values (that will be log transformed) are: telephone,
#    internet, pop, GDPpc, and reserves. Log-transform each:
tidy_data$telephone_log <- log(tidy_data$telephone)
tidy_data$internet_log <- log(tidy_data$internet)
tidy_data$pop_log <- log(tidy_data$pop)
tidy_data$GDPpc_log <- log(tidy_data$GDPpc)
tidy_data$reserves_log <- log(tidy_data$reserves)
# Check transformation:
par(mfrow = c(2,3))
hist(tidy_data$telephone_log, main = "Log of Telephone Subscriptions")
hist(tidy_data$internet_log, main = "Log of Individuals Using the Internet")
hist(tidy_data$pop_log, main = "Log of Population Total")
hist(tidy_data$GDPpc_log, main = "Log of GDP Per Capita")
hist(tidy_data$reserves_log, main = "Log of Reserves")
# All transformations have worked!

# Those variables with a significant right skew and negative values (that will be
#    arcsinh transformed) are:
tidy_data$claims_govt_asinh <- asinh(tidy_data$claims_govt)
tidy_data$inflation_asinh <- asinh(tidy_data$inflation)
tidy_data$ins_exp_asinh <- asinh(tidy_data$ins_exp)
# Check transformation:
par(mfrow = c(2,2))
hist(tidy_data$claims_govt_asinh, main = "asinh of Claims on Central Govt")
hist(tidy_data$inflation_asinh, main = "asinh of Inflation")
hist(tidy_data$ins_exp_asinh, main = "asinh of Insurance and Financial Services (% of exports)")
# These transformations also worked!

# Current account has a left skew (transform by squaring or cubing)
tidy_data$curr_acct_square <- (tidy_data$curr_acct)^2
tidy_data$curr_acct_cube <- (tidy_data$curr_acct)^3
# Check transformation:
par(mfrow = c(2,1))
hist(tidy_data$curr_acct_square, main = "Square of Current Account")
hist(tidy_data$curr_acct_cube, main = "Cube of Current Account")
# These transformations really didn't help, so I will not be transforming this variable.
tidy_data <- tidy_data %>% select(-curr_acct_square, -curr_acct_cube)

#-------------------------------------------------------------------------------
# Lasso Regression Feature Selection

# Late decision: I will remove tariff as a factor as its real values were only recorded
#    until 2022. The 2023 values have been imputed in the first script, but it
#    is not secure to impute an entire year.
tidy_data <- tidy_data %>% select(-tariff)

# Create variables for Lasso Regression
y <- tidy_data$REER
X <- tidy_data %>% select(-REER, -Country, -Year, -telephone, -internet, -pop, -GDPpc,
                          -reserves, -claims_govt, -inflation, -ins_exp)

# Convert X to a matrix then normalize
X <- model.matrix(~ ., data = X)[, -1] # Removing intercept
X <- scale(X)

# Run Lasso
set.seed(123)
lasso <- cv.glmnet(x = X, y = y, alpha = 1, standardize = FALSE)
coefs <- coef(lasso, s = "lambda.1se")
coefs

# The regression keeps the variables: adol_fert, curr_acct, death, disc_exp, 
#    industry, emp_pop, fertility, final_cons, govt_final_cons, imp_unit_value,
#    imp_vol, imp_goods, ins_imp, life_exp, ToT, migration, pop_grow, trade, unemp,
#    internet_log, pop_log, reserves_log, claims_govt_asinh, inflation_asinh, ins_exp_asinh

# Create new dataframe with Lasso predictors for easier modeling
good_preds <- rownames(coefs)[coefs[,1] != 0]
good_preds <- good_preds[good_preds != "(Intercept)"]
tidy_data_small <- tidy_data[ , c("REER", "Country", "Year", good_preds)]

rm(list = c("X", "good_preds", "y"))

# Because Lasso recognizes that they don't partially correlate with the REER (dropping
#    based on direct effect)

# Write about GDPpc - it is correlated strongly with some variables, look at classical
#    economic theory of "size matters," might be that population size explains
#    or is explained by GDPpc, etc...(include logs) always include both directions - 
#    endogeneity (this is in results section)

# In methodology, you say which method you used, how it works, why you're using it

# Into (1-2 pg), literature review, methodology and data (one big chapter, separate
#    into two subsections. Third-level subchapters in methodology - don't go below
#    three levels), results + discussion chapter (subsections in there), conclusion

#-------------------------------------------------------------------------------
# Modeling 1 - pooled simple OLS regression
OLS_simple_1 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                      + emp_pop + fertility + final_cons + govt_final_cons
                      + imp_unit_value + imp_vol + imp_goods + ins_imp + life_exp
                      + ToT + migration + pop_grow + trade + unemp + internet_log
                      + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                      + ins_exp_asinh, data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_1)

# Remove least significant predictors until only significant remain (keep those
#    on borderline of 10% significance level to be used as controls)
OLS_simple_2 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                      + fertility + final_cons + govt_final_cons + imp_unit_value
                      + imp_vol + imp_goods + ins_imp + life_exp + ToT + migration
                      + pop_grow + trade + unemp + internet_log + pop_log + reserves_log
                      + claims_govt_asinh + inflation_asinh + ins_exp_asinh,
                      data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_2)

OLS_simple_3 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                      + fertility + final_cons + govt_final_cons + imp_unit_value
                      + imp_vol + imp_goods + life_exp + ToT + migration + pop_grow
                      + trade + unemp + internet_log + pop_log + reserves_log
                      + claims_govt_asinh + inflation_asinh + ins_exp_asinh,
                      data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_3)

OLS_simple_4 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                      + fertility + final_cons + govt_final_cons + imp_unit_value
                      + imp_vol + life_exp + ToT + migration + pop_grow + trade
                      + unemp + internet_log + pop_log + reserves_log + claims_govt_asinh
                      + inflation_asinh + ins_exp_asinh, data = tidy_data_small,
                      cluster = ~Country)
summary(OLS_simple_4)

OLS_simple_5 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                      + fertility + final_cons + govt_final_cons + imp_unit_value
                      + imp_vol + life_exp + ToT + migration + pop_grow + trade
                      + internet_log + pop_log + reserves_log + claims_govt_asinh
                      + inflation_asinh + ins_exp_asinh, data = tidy_data_small,
                      cluster = ~Country)
summary(OLS_simple_5)

OLS_simple_6 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                      + fertility + final_cons + govt_final_cons + imp_unit_value
                      + imp_vol + life_exp + ToT + migration + pop_grow + trade
                      + internet_log + pop_log + reserves_log + inflation_asinh
                      + ins_exp_asinh, data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_6)

OLS_simple_7 <- feols(REER ~ death + disc_exp + industry + emp_pop + fertility
                      + final_cons + govt_final_cons + imp_unit_value + imp_vol
                      + life_exp + ToT + migration + pop_grow + trade + internet_log
                      + pop_log + reserves_log + inflation_asinh + ins_exp_asinh,
                      data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_7)

OLS_simple_8 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                      + govt_final_cons + imp_unit_value + imp_vol + life_exp + ToT
                      + migration + pop_grow + trade + internet_log + pop_log
                      + reserves_log + inflation_asinh + ins_exp_asinh,
                      data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_8)

OLS_simple_9 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                      + govt_final_cons + imp_unit_value + imp_vol + life_exp + ToT
                      + migration + pop_grow + trade + pop_log + reserves_log
                      + inflation_asinh + ins_exp_asinh, data = tidy_data_small,
                      cluster = ~Country)
summary(OLS_simple_9)

OLS_simple_10 <- feols(REER ~ death + industry + fertility + final_cons + govt_final_cons
                      + imp_unit_value + imp_vol + life_exp + ToT + migration
                      + pop_grow + trade + pop_log + reserves_log + inflation_asinh
                      + ins_exp_asinh, data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_10)

OLS_simple_11 <- feols(REER ~ death + industry + fertility + final_cons + govt_final_cons
                       + imp_unit_value + life_exp + ToT + migration + pop_grow
                       + trade + pop_log + reserves_log + inflation_asinh + ins_exp_asinh,
                       data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_11)

OLS_simple_12 <- feols(REER ~ death + industry + fertility + final_cons + govt_final_cons
                       + imp_unit_value + life_exp + migration + pop_grow + trade
                       + pop_log + reserves_log + inflation_asinh + ins_exp_asinh,
                       data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_12)

OLS_simple_13 <- feols(REER ~ death + fertility + final_cons + govt_final_cons
                       + imp_unit_value + life_exp + migration + pop_grow + trade
                       + pop_log + reserves_log + inflation_asinh + ins_exp_asinh,
                       data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_13)

OLS_simple_14 <- feols(REER ~ death + fertility + final_cons + imp_unit_value
                       + life_exp + migration + pop_grow + trade + pop_log + reserves_log
                       + inflation_asinh + ins_exp_asinh, data = tidy_data_small,
                       cluster = ~Country)
summary(OLS_simple_14)

OLS_simple_15 <- feols(REER ~ death + fertility + final_cons + imp_unit_value
                       + life_exp + migration + pop_grow + trade + pop_log + reserves_log
                       + inflation_asinh, data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_15)

OLS_simple_FINAL <- feols(REER ~ death + fertility + imp_unit_value + life_exp
                       + migration + pop_grow + trade + pop_log + reserves_log
                       + inflation_asinh, data = tidy_data_small, cluster = ~Country)
summary(OLS_simple_FINAL)

# After the removal process, the significant predictors of the REER are: death,
#    fertility, imp_unnit_value, life_exp, migration, pop_grow, trade, pop_log,
#    reserves_log, inflation_asinh
rm(list = c("OLS_simple_1", "OLS_simple_2", "OLS_simple_3", "OLS_simple_4",
            "OLS_simple_5", "OLS_simple_6", "OLS_simple_7", "OLS_simple_8",
            "OLS_simple_9", "OLS_simple_10", "OLS_simple_11", "OLS_simple_12",
            "OLS_simple_13", "OLS_simple_14", "OLS_simple_15"))

# Modeling 2 - Include Country Fixed Effects
# Follow the same steps as above but include Country fixed effects
countryFE_1 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                      + emp_pop + fertility + final_cons + govt_final_cons
                      + imp_unit_value + imp_vol + imp_goods + ins_imp + life_exp
                      + ToT + migration + pop_grow + trade + unemp + internet_log
                      + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                      + ins_exp_asinh | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_1)

countryFE_2 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                     + emp_pop + fertility + final_cons + imp_unit_value + imp_vol
                     + imp_goods + ins_imp + life_exp + ToT + migration + pop_grow
                     + trade + unemp + internet_log + pop_log + reserves_log
                     + claims_govt_asinh + inflation_asinh + ins_exp_asinh | Country,
                     data = tidy_data_small, cluster = ~Country)
summary(countryFE_2)

countryFE_3 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                     + emp_pop + fertility + final_cons + imp_unit_value + imp_vol
                     + imp_goods + ins_imp + life_exp + ToT + migration + pop_grow
                     + trade + unemp + internet_log + pop_log + reserves_log
                     + inflation_asinh + ins_exp_asinh | Country, data = tidy_data_small,
                     cluster = ~Country)
summary(countryFE_3)

countryFE_4 <- feols(REER ~ curr_acct + death + disc_exp + industry + emp_pop
                     + fertility + final_cons + imp_unit_value + imp_vol + imp_goods
                     + ins_imp + life_exp + ToT + migration + pop_grow + trade
                     + unemp + internet_log + pop_log + reserves_log + inflation_asinh
                     + ins_exp_asinh | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_4)

countryFE_5 <- feols(REER ~ curr_acct + death + disc_exp + emp_pop + fertility
                     + final_cons + imp_unit_value + imp_vol + imp_goods + ins_imp
                     + life_exp + ToT + migration + pop_grow + trade + unemp
                     + internet_log + pop_log + reserves_log + inflation_asinh
                     + ins_exp_asinh | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_5)

countryFE_6 <- feols(REER ~ curr_acct + death + disc_exp + fertility + final_cons
                     + imp_unit_value + imp_vol + imp_goods + ins_imp + life_exp
                     + ToT + migration + pop_grow + trade + unemp + internet_log
                     + pop_log + reserves_log + inflation_asinh + ins_exp_asinh
                     | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_6)

countryFE_7 <- feols(REER ~ curr_acct + death + disc_exp + fertility + final_cons
                     + imp_unit_value + imp_vol + imp_goods + ins_imp + life_exp
                     + ToT + migration + pop_grow + unemp + internet_log + pop_log
                     + reserves_log + inflation_asinh + ins_exp_asinh | Country,
                     data = tidy_data_small, cluster = ~Country)
summary(countryFE_7)

countryFE_8 <- feols(REER ~ curr_acct + death + disc_exp + fertility + final_cons
                     + imp_unit_value + imp_vol + imp_goods + ins_imp + life_exp
                     + migration + pop_grow + unemp + internet_log + pop_log
                     + reserves_log + inflation_asinh + ins_exp_asinh | Country, 
                     data = tidy_data_small, cluster = ~Country)
summary(countryFE_8)

countryFE_9 <- feols(REER ~ curr_acct + death + disc_exp + fertility + imp_unit_value 
                     + imp_vol + imp_goods + ins_imp + life_exp + migration + pop_grow 
                     + unemp + internet_log + pop_log + reserves_log + inflation_asinh
                     + ins_exp_asinh | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_9)

countryFE_10 <- feols(REER ~ curr_acct + death + disc_exp + fertility + imp_unit_value
                     + imp_vol + imp_goods + ins_imp + life_exp + migration + pop_grow
                     + unemp + internet_log + pop_log + reserves_log + inflation_asinh
                     | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_10)

countryFE_11 <- feols(REER ~ curr_acct + death + disc_exp + fertility + imp_unit_value
                      + imp_vol + imp_goods + life_exp + migration + pop_grow + unemp
                      + internet_log + pop_log + reserves_log + inflation_asinh
                      | Country, data = tidy_data_small, cluster = ~Country)
summary(countryFE_11)

countryFE_12 <- feols(REER ~ curr_acct + death + fertility + imp_unit_value + imp_vol
                      + imp_goods + life_exp + migration + pop_grow + unemp + internet_log
                      + pop_log + reserves_log + inflation_asinh | Country,
                      data = tidy_data_small, cluster = ~Country)
summary(countryFE_12)

countryFE_13 <- feols(REER ~ curr_acct + death + fertility + imp_unit_value + imp_vol
                      + imp_goods + life_exp + migration + pop_grow + unemp + pop_log
                      + reserves_log + inflation_asinh | Country, data = tidy_data_small, 
                      cluster = ~Country)
summary(countryFE_13)

countryFE_FINAL <- feols(REER ~ curr_acct + death + fertility + imp_unit_value
                         + imp_vol + imp_goods + life_exp + pop_grow + unemp
                         + pop_log + reserves_log + inflation_asinh | Country,
                         data = tidy_data_small, cluster = ~Country)
summary(countryFE_FINAL)

# After removal, the significant predictors are: curr_acct, death, fertility,
#     imp_unit_value, imp_vol, imp_goods, life_exp, pop_grow, unemp, pop_log,
#    reserves_log, inflation_asinh
rm(list = c("countryFE_1", "countryFE_2", "countryFE_3", "countryFE_4", "countryFE_5",
            "countryFE_6", "countryFE_7", "countryFE_8", "countryFE_9", "countryFE_10",
            "countryFE_11", "countryFE_12", "countryFE_13"))

# Modeling 3 - Include Time Fixed Effects
timeFE_1 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry + emp_pop 
                  + fertility + final_cons + govt_final_cons + imp_unit_value + imp_vol
                  + imp_goods + ins_imp + life_exp + ToT + migration + pop_grow
                  + trade + unemp + internet_log + pop_log + reserves_log
                  + claims_govt_asinh + inflation_asinh + ins_exp_asinh | Year, 
                  data = tidy_data_small, cluster = ~Country)
summary(timeFE_1)

timeFE_2 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry + emp_pop
                  + fertility + final_cons + govt_final_cons + imp_unit_value + imp_vol
                  + ins_imp + life_exp + ToT + migration + pop_grow + trade + unemp
                  + internet_log + pop_log + reserves_log + claims_govt_asinh 
                  + inflation_asinh + ins_exp_asinh | Year, data = tidy_data_small,
                  cluster = ~Country)
summary(timeFE_2)

timeFE_3 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry + emp_pop
                  + fertility + final_cons + govt_final_cons + imp_unit_value + imp_vol
                  + life_exp + ToT + migration + pop_grow + trade + unemp + internet_log
                  + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                  + ins_exp_asinh | Year, data = tidy_data_small, cluster = ~Country)
summary(timeFE_3)

timeFE_4 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry + emp_pop
                  + fertility + final_cons + govt_final_cons + imp_unit_value + imp_vol
                  + life_exp + ToT + migration + pop_grow + trade + unemp + internet_log
                  + pop_log + reserves_log + inflation_asinh + ins_exp_asinh | Year,
                  data = tidy_data_small, cluster = ~Country)
summary(timeFE_4)

timeFE_5 <- feols(REER ~ adol_fert + death + disc_exp + industry + emp_pop + fertility
                  + final_cons + govt_final_cons + imp_unit_value + imp_vol + life_exp
                  + ToT + migration + pop_grow + trade + unemp + internet_log 
                  + pop_log + reserves_log + inflation_asinh + ins_exp_asinh | Year,
                  data = tidy_data_small, cluster = ~Country)
summary(timeFE_5)

timeFE_6 <- feols(REER ~ adol_fert + death + disc_exp + industry + emp_pop + fertility
                  + final_cons + govt_final_cons + imp_unit_value + imp_vol + life_exp
                  + ToT + migration + pop_grow + trade + internet_log + pop_log
                  + reserves_log + inflation_asinh + ins_exp_asinh | Year,
                  data = tidy_data_small, cluster = ~Country)
summary(timeFE_6)

timeFE_7 <- feols(REER ~ death + disc_exp + industry + emp_pop + fertility + final_cons
                  + govt_final_cons + imp_unit_value + imp_vol + life_exp + ToT
                  + migration + pop_grow + trade + internet_log + pop_log + reserves_log
                  + inflation_asinh + ins_exp_asinh | Year, data = tidy_data_small,
                  cluster = ~Country)
summary(timeFE_7)

timeFE_8 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                  + govt_final_cons + imp_unit_value + imp_vol + life_exp + ToT
                  + migration + pop_grow + trade + internet_log + pop_log + reserves_log
                  + inflation_asinh + ins_exp_asinh | Year, data = tidy_data_small,
                  cluster = ~Country)
summary(timeFE_8)

timeFE_9 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                  + govt_final_cons + imp_unit_value + imp_vol + life_exp + ToT 
                  + migration + pop_grow + trade + pop_log + reserves_log + inflation_asinh
                  + ins_exp_asinh | Year, data = tidy_data_small, cluster = ~Country)
summary(timeFE_9)

timeFE_10 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                   + imp_unit_value + imp_vol + life_exp + ToT + migration + pop_grow
                   + trade + pop_log + reserves_log + inflation_asinh + ins_exp_asinh
                   | Year, data = tidy_data_small, cluster = ~Country)
summary(timeFE_10)

timeFE_11 <- feols(REER ~ death + industry + emp_pop + fertility + final_cons
                   + imp_unit_value + imp_vol + life_exp + migration + pop_grow
                   + trade + pop_log + reserves_log + inflation_asinh + ins_exp_asinh
                   | Year, data = tidy_data_small, cluster = ~Country)
summary(timeFE_11)

timeFE_12 <- feols(REER ~ death + industry + fertility + final_cons + imp_unit_value
                   + imp_vol + life_exp + migration + pop_grow + trade + pop_log
                   + reserves_log + inflation_asinh + ins_exp_asinh | Year,
                   data = tidy_data_small, cluster = ~Country)
summary(timeFE_12)

timeFE_13 <- feols(REER ~ death + industry + fertility + final_cons + imp_unit_value
                   + imp_vol + life_exp + migration + pop_grow + trade + pop_log
                   + reserves_log + inflation_asinh | Year, data = tidy_data_small,
                   cluster = ~Country)
summary(timeFE_13)

timeFE_FINAL <- feols(REER ~ death + industry + fertility + imp_unit_value + imp_vol
                      + life_exp + migration + pop_grow + trade + pop_log + reserves_log
                      + inflation_asinh | Year, data = tidy_data_small, cluster = ~Country)
summary(timeFE_FINAL)

# Significant predictors: death, industry, fertility, imp_unit_value, imp_vol,
#    life_exp, migration, pop_grow, trade, pop_log, reserves_log, inflation_asinh
rm(list = c("timeFE_1", "timeFE_2", "timeFE_3", "timeFE_4", "timeFE_5", "timeFE_6",
            "timeFE_7", "timeFE_8", "timeFE_9", "timeFE_10", "timeFE_11", "timeFE_12",
            "timeFE_13"))

# Modeling 4 - Include Two-Way Fixed Effects (My Chosen Model)
twowayFE_1 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                  + emp_pop + fertility + final_cons + govt_final_cons + imp_unit_value
                  + imp_vol + imp_goods + ins_imp + life_exp + ToT + migration
                  + pop_grow + trade + unemp + internet_log + pop_log + reserves_log
                  + claims_govt_asinh + inflation_asinh + ins_exp_asinh
                  | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_1)

twowayFE_2 <- feols(REER ~ adol_fert + curr_acct + death + disc_exp + industry
                    + fertility + final_cons + govt_final_cons + imp_unit_value
                    + imp_vol + imp_goods + ins_imp + life_exp + ToT + migration
                    + pop_grow + trade + unemp + internet_log + pop_log + reserves_log
                    + claims_govt_asinh + inflation_asinh + ins_exp_asinh
                    | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_2)

twowayFE_3 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + final_cons + govt_final_cons + imp_unit_value + imp_vol
                    + imp_goods + ins_imp + life_exp + ToT + migration + pop_grow
                    + trade + unemp + internet_log + pop_log + reserves_log
                    + claims_govt_asinh + inflation_asinh + ins_exp_asinh
                    | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_3)

twowayFE_4 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + final_cons + imp_unit_value + imp_vol + imp_goods + ins_imp
                    + life_exp + ToT + migration + pop_grow + trade + unemp + internet_log
                    + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                    + ins_exp_asinh | Country + Year, data = tidy_data_small,
                    cluster = ~Country)
summary(twowayFE_4)

twowayFE_5 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + final_cons + imp_unit_value + imp_vol + imp_goods + ins_imp
                    + ToT + migration + pop_grow + trade + unemp + internet_log
                    + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                    + ins_exp_asinh | Country + Year, data = tidy_data_small,
                    cluster = ~Country)
summary(twowayFE_5)

twowayFE_6 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + final_cons + imp_unit_value + imp_vol + imp_goods + ins_imp
                    + migration + pop_grow + trade + unemp + internet_log + pop_log
                    + reserves_log + claims_govt_asinh + inflation_asinh + ins_exp_asinh
                    | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_6)

twowayFE_7 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + final_cons + imp_unit_value + imp_vol + imp_goods + ins_imp
                    + migration + pop_grow + trade + unemp + internet_log + pop_log
                    + reserves_log + claims_govt_asinh + inflation_asinh
                    | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_7)

twowayFE_8 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + imp_unit_value + imp_vol + imp_goods + ins_imp + migration
                    + pop_grow + trade + unemp + internet_log + pop_log + reserves_log
                    + claims_govt_asinh + inflation_asinh | Country + Year,
                    data = tidy_data_small, cluster = ~Country)
summary(twowayFE_8)

twowayFE_9 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + imp_unit_value + imp_vol + imp_goods + ins_imp + pop_grow
                    + trade + unemp + internet_log + pop_log + reserves_log
                    + claims_govt_asinh + inflation_asinh | Country + Year,
                    data = tidy_data_small, cluster = ~Country)
summary(twowayFE_9)

twowayFE_10 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                    + imp_unit_value + imp_vol + imp_goods + pop_grow + trade
                    + unemp + internet_log + pop_log + reserves_log + claims_govt_asinh
                    + inflation_asinh | Country + Year, data = tidy_data_small,
                    cluster = ~Country)
summary(twowayFE_10)

twowayFE_11 <- feols(REER ~ adol_fert + curr_acct + disc_exp + industry + fertility
                     + imp_unit_value + imp_vol + imp_goods + pop_grow + trade
                     + unemp + pop_log + reserves_log + claims_govt_asinh + inflation_asinh
                     | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_11)

twowayFE_12 <- feols(REER ~ curr_acct + disc_exp + industry + fertility + imp_unit_value
                     + imp_vol + imp_goods + pop_grow + trade + unemp + pop_log
                     + reserves_log + claims_govt_asinh + inflation_asinh
                     | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_12)

twowayFE_13 <- feols(REER ~ curr_acct + disc_exp + industry + fertility + imp_unit_value
                     + imp_vol + imp_goods + pop_grow + trade + unemp + reserves_log
                     + claims_govt_asinh + inflation_asinh | Country + Year, 
                     data = tidy_data_small, cluster = ~Country)
summary(twowayFE_13)

twowayFE_14 <- feols(REER ~ curr_acct + disc_exp + industry + fertility + imp_unit_value
                     + imp_vol + imp_goods + pop_grow + trade + reserves_log
                     + claims_govt_asinh + inflation_asinh | Country + Year,
                     data = tidy_data_small, cluster = ~Country)
summary(twowayFE_14)

twowayFE_15 <- feols(REER ~ curr_acct + industry + fertility + imp_unit_value
                     + imp_vol + imp_goods + pop_grow + trade + reserves_log
                     + claims_govt_asinh + inflation_asinh | Country + Year,
                     data = tidy_data_small, cluster = ~Country)
summary(twowayFE_15)

twowayFE_16 <- feols(REER ~ curr_acct + industry + fertility + imp_unit_value
                     + imp_vol + imp_goods + pop_grow + trade + reserves_log
                     + inflation_asinh | Country + Year, data = tidy_data_small,
                     cluster = ~Country)
summary(twowayFE_16)

twowayFE_FINAL <- feols(REER ~ curr_acct + industry + fertility + imp_unit_value
                        + imp_vol + imp_goods + pop_grow + reserves_log + inflation_asinh
                        | Country + Year, data = tidy_data_small, cluster = ~Country)
summary(twowayFE_FINAL)

# Significant predictors: curr_acct, industry, fertility, imp_unit_value, imp_vol,
#    imp_goods, pop_grow, reserves_log, inflation_asinh
rm(list = c("twowayFE_1", "twowayFE_2", "twowayFE_3", "twowayFE_4", "twowayFE_5",
            "twowayFE_6", "twowayFE_7", "twowayFE_8", "twowayFE_9", "twowayFE_10",
            "twowayFE_11", "twowayFE_12", "twowayFE_13", "twowayFE_14", "twowayFE_15",
            "twowayFE_16"))

# Modeling 5 - Include Random Effects
# Random effects cannot be used for this study as there are more predictors than
#    countries, so the calculation is mathematically impossible. This scenario
#    favors fixed effects.

# Model comparison
etable(OLS_simple_FINAL, countryFE_FINAL, timeFE_FINAL, twowayFE_FINAL,
       headers = c("Pooled Simple OLS", "Country FE", "Time FE", "Two-Way FE"))

# For export:
# etable(OLS_simple_FINAL, countryFE_FINAL, timeFE_FINAL, twowayFE_FINAL,
#       headers = c("Pooled Simple OLS", "Country FE", "Time FE", "Two-Way FE"),
#       file = "final_model_table.html")

# AIC and BIC comparison
# Create comparison table
model_fit_stats <- data.frame(
  Model = c("Pooled OLS",
            "Country Fixed Effects",
            "Time Fixed Effects",
            "Two-Way Fixed Effects"),
  
  AIC = c(
    AIC(OLS_simple_FINAL),
    AIC(countryFE_FINAL),
    AIC(timeFE_FINAL),
    AIC(twowayFE_FINAL)
  ),
  
  BIC = c(
    BIC(OLS_simple_FINAL),
    BIC(countryFE_FINAL),
    BIC(timeFE_FINAL),
    BIC(twowayFE_FINAL)
  )
)

# Print results
print(model_fit_stats)
