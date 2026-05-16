# Thesis Work 1 - Preparing Data and Imputing Missing Values

# Load packages
library(dplyr)
library(tidyr)
library(VIM)

# Import tidy dataset including countries, years, REER variable, and possible predictors
tidy_data <- as.data.frame(readxl::read_excel("data/Thesis_Tidy_Data.xlsx", na = ".."))

# Rename columns for easier processing
colnames(tidy_data) <- c("Country", "Year", "REER", "adol_fert", "age_dep", "birth",
                         "broad", "broad_grow", "broad_reserves", "claims_govt_broad",
                         "claims_govt", "education", "curr_acct", "health_exp", "death",
                         "disc_exp", "agriculture", "industry", "services", "emp_pop",
                         "exp_unit_value", "exp_value", "exp_vol", "exp_goods", 
                         "FDI_flows", "fertility", "final_cons", "telephone", "GNI",
                         "govt_final_cons", "imp_unit_value", "imp_value", "imp_vol",
                         "imp_goods", "internet", "inflation", "ins_exp", "ins_imp",
                         "life_exp", "ToT", "migration", "pop_grow", "pop", "GDPpc",
                         "tariff", "reserves", "trade", "unemp")

# Remove columns which were decided to be unfit for analysis
tidy_data <- tidy_data %>% select(-broad, -broad_grow, -broad_reserves, -claims_govt_broad,
                                  -education, -health_exp, -GNI)

# Remove 2024 rows - there are not enough variables that have data for 2024
tidy_data <- tidy_data %>% filter(Year != 2024)

# Change discrepancy in expenditure to % of GDP instead of currency amounts (each
#    country has a different currency, so not standardizing would skew analysis)
GDP_LCU <- as.data.frame(readxl::read_excel("data/GDPCURRENTLCU_WB.xlsx", na=".."))
rownames(GDP_LCU) <- GDP_LCU[[3]]
GDP_LCU <- GDP_LCU[, -c(1:4)]
GDP_LCU <- GDP_LCU[, -34]
# Converting GDP_LCU to tidy format:
GDP_LCU <- GDP_LCU %>% tibble::rownames_to_column(var = "Country") %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP_LCU") %>%
  mutate(Year = sub(" .*", "", Year), Year = as.integer(Year))
# Merge GDP_LCU into tidy_data and standardize disc_exp
tidy_data <- tidy_data %>% left_join(GDP_LCU, by = c("Country", "Year")) %>%
  mutate(disc_exp = (disc_exp/GDP_LCU)*100)
# Remove GDP_LCU column and dataframe - no longer needed
tidy_data <- tidy_data %>% select(-GDP_LCU)
rm(GDP_LCU)

#-------------------------------------------------------------------------------
# Imputing missing values - Mean and Median


# The variables with < 1% missing values:
sum(is.na(tidy_data$claims_govt))/length(tidy_data$claims_govt)
sum(is.na(tidy_data$disc_exp))/length(tidy_data$disc_exp)
sum(is.na(tidy_data$FDI_flows))/length(tidy_data$FDI_flows)
sum(is.na(tidy_data$final_cons))/length(tidy_data$final_cons)
sum(is.na(tidy_data$telephone))/length(tidy_data$telephone)
sum(is.na(tidy_data$reserves))/length(tidy_data$reserves)
# I will use mean or median imputation for these. Plot to check distribution:
par(mfrow = c(2,3))
hist(na.omit(tidy_data$claims_govt), main = "Claims on Central Govt")
hist(na.omit(tidy_data$disc_exp), main = "Discrepancy in Estimate")
hist(na.omit(tidy_data$FDI_flows), main = "FDI Flows")
hist(na.omit(tidy_data$final_cons), main = "Final Consumption Expenditure")
hist(na.omit(tidy_data$telephone), main = "Telephone Subscriptions")
hist(na.omit(tidy_data$reserves), main = "Reserves")
par(mfrow = c(1,1))
# Variables with right skew (impute using median): claims_govt, telephone, reserves
# Variables with normal distribution (impute using mean): disc_exp, FDI_flows, final_cons

# Imputation

# claims_govt
# Panama has 12% of its values missing (consecutively), so KNN (n=5) imputation
#   will be used for this, with median imputation used for the rest.
# Extract country
panama_claims_govt <- tidy_data %>% filter(Country == "Panama") %>% arrange(Year)
# Perform K-NN imputation
panama_claims_govt <- VIM::kNN(panama_claims_govt, variable = "claims_govt", k = 5, imp_var = FALSE)
# Place back into tidy_data
tidy_data <- tidy_data %>% left_join(panama_claims_govt %>% select(Country, Year, claims_govt),
                                     by = c("Country", "Year"), suffix = c("", "_imp")) %>%
  mutate(claims_govt = ifelse(is.na(claims_govt), claims_govt_imp, claims_govt)) %>%
  select(-claims_govt_imp)
# Remove panama dataframe - no longer needed
rm(panama_claims_govt)
# Fill in remaining NAs with median imputation (within country, over time)
tidy_data <- tidy_data %>% group_by(Country) %>%
  mutate(claims_govt = coalesce(claims_govt, median(claims_govt, na.rm = TRUE))) %>% ungroup()

# Remaining variables
tidy_data <- tidy_data %>% group_by(Country) %>%
  mutate(disc_exp = coalesce(disc_exp, mean(disc_exp, na.rm = TRUE)),
         FDI_flows = coalesce(FDI_flows, mean(FDI_flows, na.rm = TRUE)),
         final_cons = coalesce(final_cons, mean(final_cons, na.rm = TRUE)),
         telephone = coalesce(telephone, median(telephone, na.rm = TRUE)),
         reserves = coalesce(reserves, median(reserves, na.rm = TRUE))
         ) %>% ungroup()


# Variables with > 1% missing values:
sum(is.na(tidy_data$emp_pop))/length(tidy_data$emp_pop)
sum(is.na(tidy_data$internet))/length(tidy_data$internet)
sum(is.na(tidy_data$ins_exp))/length(tidy_data$ins_exp)
sum(is.na(tidy_data$tariff))/length(tidy_data$tariff)
# I will use K-NN (k=5) imputation for these. Imputation:

# emp_pop
tidy_data <- tidy_data %>% group_by(Country) %>%
  group_modify(~ VIM::kNN(.x, variable = "emp_pop", k = 5, imp_var = FALSE)) %>% ungroup()

# internet
tidy_data <- tidy_data %>% group_by(Country) %>%
  group_modify(~ VIM::kNN(.x, variable = "internet", k = 5, imp_var = FALSE)) %>% ungroup()

# ins_exp
tidy_data <- tidy_data %>% group_by(Country) %>%
  group_modify(~ VIM::kNN(.x, variable = "ins_exp", k = 5, imp_var = FALSE)) %>% ungroup()

# tariff
tidy_data <- tidy_data %>% group_by(Country) %>%
  group_modify(~ VIM::kNN(.x, variable = "tariff", k = 5, imp_var = FALSE)) %>% ungroup()

# Check:
sum(is.na(tidy_data))
# Yay! No NAs!