## Install kare saare zaroori packages (Install Packages)
##Packages
library(arrow)
library(plm)
library(tidyr)
library(dplyr)
library(mFilter)
library(ggplot2)
library(stringr)
library(fixest)
library(readr)
library(HonestDiD)
library(lmtest)
library(sandwich)
library(fixest)
library(readxl)
library(modelsummary)

## Isko Padhe R mei (Read file)
main_df <- read_excel("1st_global.xlsx")
gdp_df <-  read_csv('population_data/1st_global.csv')
pdata <- pdata.frame(main_df, index = c("Country", "Year"))

###########################################################
#####Saare FE wale regressions yaha se#####################
###########################################################

fe_model_1 <- plm(rop ~ exp + NWnfa_shrY + kl_index, 
                  index = c("Country", "Year"), 
                  data = pdata, 
                  model = "within")
fe_model_2 <- plm(rop ~ exp + NWnfa_shrY + kl_index + factor(Year), 
                  data = pdata, 
                  model = "within", 
                  effect = "individual")
summary(fe_model_1)
summary(fe_model_2)

# Apply HP filter function
apply_hpfilter <- function(df) {
  df <- df %>% arrange(Year)
  df$prod_trend <- hpfilter(as.numeric(df$prod_index), freq = 1600, type = "lambda")$trend
  df$prod_deviation <- df$prod_index - df$prod_trend
  return(df)
}

# Apply HP filter to each Country
pdata <- pdata %>%
  group_by(Country) %>%
  group_modify(~ apply_hpfilter(.x)) %>%
  ungroup()

# Convert back to pdata.frame
pdata <- pdata.frame(pdata, index = c("Country", "Year"))

# Openness regressions with country fixed effects
pdata <- pdata[!is.na(pdata$openness), ]  
fe_model_3 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + exp, 
                  index = c("Country", "Year"), 
                  data = pdata, 
                  model = "within", 
                  na.action = na.exclude)

fe_model_4 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + prod_deviation, 
                  index = c("Country", "Year"), 
                  data = pdata, 
                  model = "within", 
                  na.action = na.exclude)

# Time fixed effects models
fe_model_5 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + exp + factor(Year),
                  index = c("Country", "Year"),  
                  data = pdata,  
                  model = "within",  
                  effect = "twoways")

fe_model_6 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + prod_deviation + factor(Year),
                  index = c("Country", "Year"),  
                  data = pdata,  
                  model = "within",  
                  effect = "twoways")

# Other openness variables
fe_model_tot <- plm(rop ~  lag(rop, 1) + tot + kl_index + exp + factor(Year),
                    index = c("Country", "Year"),  
                    data = pdata,  
                    model = "within",  
                    effect = "twoways")

fe_model_tariff <- plm(rop ~ lag(rop, 1) + tariff + kl_index + exp + factor(Year),
                       index = c("Country", "Year"),  
                       data = pdata,  
                       model = "within",  
                       effect = "twoways")

fe_model_exp <- plm(rop ~ lag(rop, 1) + exp_gdp + kl_index + exp + factor(Year),
                    index = c("Country", "Year"),  
                    data = pdata,  
                    model = "within",  
                    effect = "twoways")

fe_model_imp <- plm(rop ~ lag(rop, 1) + imp_gdp + kl_index + exp + factor(Year),
                    index = c("Country", "Year"),  
                    data = pdata,  
                    model = "within",  
                    effect = "twoways")

# Calculate clustered standard errors for all models
models <- list(fe_model_3, fe_model_4, fe_model_5, fe_model_6, 
               fe_model_tot, fe_model_tariff, fe_model_exp, fe_model_imp)

clustered_se_list <- lapply(models, function(x) {
  vcovHC(x, type = "HC1", cluster = "group")
})

# Display results with clustered standard errors
model_names <- c("Model 3", "Model 4", "Model 5", "Model 6", 
                 "Total Trade", "Tariff", "Export GDP", "Import GDP")

for (i in seq_along(models)) {
  cat(paste0("\n", model_names[i], " Results (clustered at Country level):\n"))
  print(coeftest(models[[i]], vcov = clustered_se_list[[i]]))
}



###########################################################################
###########################################################################
###########################################################################

## Rate of profit aur capital accumulation ka sammelan
pdata$cap_accum <- diff(pdata$Fixed_capital_stock) / lag(pdata$Fixed_capital_stock)
fe_model_cap_full <- plm(cap_accum ~ log(rop) + factor(Year), 
                         data = pdata, 
                         index = c("country", "Year"), 
                         model = "within", 
                         effect = "twoways")

summary(fe_model_cap_full)

library(dplyr)
library(mFilter)
library(purrr)

# Ensure Year is numeric
pdata$Year <- as.numeric(as.character(pdata$Year))

# Split by country
pdata_split <- pdata %>%
  group_by(Country) %>%
  arrange(Year) %>%
  group_split()

# Apply HP filter and compute capacity utilization
pdata_filtered <- map_dfr(pdata_split, function(df) {
  gdp_ts <- ts(df$gdp, start = min(df$Year), end = max(df$Year), frequency = 1)
  hp_out <- hpfilter(gdp_ts, freq = 100)
  df$gdp_trend <- as.numeric(hp_out$trend)
  df$gdp_cycle <- as.numeric(hp_out$cycle)
  df$capacity_utilization <- (df$gdp / df$gdp_trend) * 100
  return(df)
})

fe_model_cap_util <- plm(cap_accum ~ log(rop) + pdata_filtered$capacity_utilization + factor(Year), 
                         data = pdata_filtered, 
                         index = c("Country", "Year"), 
                         model = "within", 
                         effect = "twoways")

summary(fe_model_cap_full)
summary(fe_model_cap_util)

## Instrument Analysis 
df_pop <- read_csv("dyadic_trade_bilateral_pop.csv")
target_iso <- c('DEU', 'ESP', 'FRA', 'GBR', 'NLD', 'SWE', 'USA')
##Wapis main_df isko ghusaane ka kaam kare

main_df <- read_excel("1st_global.xlsx")
main_df <- main_df %>%
  mutate(
    iso_o = case_when(
      Country == "Germany" ~ "DEU",
      Country == "Spain" ~ "ESP",
      Country == "France" ~ "FRA",
      Country == "Sweden" ~ "SWE",
      Country == "USA" ~ "USA",
      Country == "Netherlands" ~ "NLD",
      Country == "UK" ~ "GBR"
    ),
    Year = as.integer(Year),
    openness = as.numeric(openness),
    rop = as.numeric(rop),
    labor = as.numeric(labor),
    gdp = as.numeric(gdp),
    kl_index = as.numeric(kl_index),
    exp = as.numeric(exp)
  ) %>%
  select(-Country)

df_bilateral<- df_pop %>%
  left_join(gdp_df %>% select(iso, year, GDP), 
            by = c("iso_origin" = "iso", "year" = "year")) %>%
  group_by(iso_origin, iso_destination, year) %>%
  summarise(
    trade_flow = sum(trade_flow, na.rm = TRUE),
    GDP = first(GDP),
    log_sea_dist_short = first(log(sea_dist_short)),
    has_colony = first(current_colony),
    uncertainty_destination = first(uncertainty_destination),
    uncertainty_origin = first(uncertainty_origin),
    anomaly_origin = first(anomaly_origin),
    anomaly_destination = anomaly_destination
  ) %>%
  ungroup()

uncertainty_origin_df <- df_bilateral %>%
  filter(iso_origin %in% target_iso) %>%
  group_by(iso_origin, year) %>%
  summarise(
    uncertainty_origin = first(uncertainty_origin),
    uncertainty_destination = first(uncertainty_destination),
    anomaly_origin = first(anomaly_origin),
    anomaly_destination = first(anomaly_destination),
    .groups = 'drop'
  ) %>%
  rename(iso_o = iso_origin, Year = year)

# Now merge this with your main_df (before creating pdata)
main_df <- main_df %>%
  left_join(uncertainty_origin_df, by = c("iso_o", "Year"))

# If you want to keep it in pdata, update your pdata.frame
pdata <- pdata.frame(main_df, index = c("iso_o", "Year"), drop.index = FALSE)

# Check the result
head(pdata[, c("iso_o", "Year", "uncertainty_origin")])



gravity_model <- fepois(
  trade_flow ~ log_sea_dist_short*uncertainty_destination + uncertainty_destination*has_colony| 
    iso_origin + iso_destination + year,
  data = df_bilateral,
  cluster = ~iso_origin + iso_destination
)
summary(gravity_model)

df_bilateral <- df_bilateral %>%
  mutate(pred_trade_flow = predict(gravity_model, newdata = .)) %>%
  mutate(
    pred_trade_flow = replace_na(pred_trade_flow, 0),
    pred_bilateral_openness = pred_trade_flow / (GDP + 1e-6)
  )

pred_openness <- df_bilateral %>%
  group_by(iso_origin, year) %>%
  summarise(pred_openness = sum(pred_bilateral_openness, na.rm = TRUE)) %>%
  rename(iso_o = iso_origin) %>%
  arrange(iso_o, year) %>%
  group_by(iso_o) %>%
  mutate(pred_openness_lag = lag(pred_openness, 1)) %>%
  ungroup()

# Ensure all required columns are numeric and handle NAs
main_df <- main_df %>%
  mutate(
    across(c(tot, tariff, exp_gdp, imp_gdp), as.numeric),
    across(c(tot, tariff, exp_gdp, imp_gdp), ~replace_na(., 0))  # Replace NA with 0 if appropriate
  )

# Verify columns exist before creating pdata
print(colnames(pdata))

# Create panel data frame WITHOUT dropping columns
pdata <- pdata.frame(main_df, index = c("iso_o", "Year"), drop.index = FALSE)

main_df <- main_df %>%
  left_join(pred_openness %>% select(iso_o, year, pred_openness, pred_openness_lag),
            by = c("iso_o", "Year" = "year"))
main_df <- main_df %>%
  mutate(
    pred_openness = as.numeric(unlist(pred_openness)),
    pred_openness_lag = as.numeric(unlist(pred_openness_lag)),
    log_pred_openness = log(pred_openness + 1),
    log_pred_openness_lag = log(pred_openness_lag + 1)
  )
View(main_df)
pdata <- pdata.frame(main_df, index = c("iso_o", "Year"))

# First stage with two instruments
first_stage_1 <- feols(
  log(openness) ~ log(pred_openness)+ log(pred_openness_lag) | iso_o + Year,
  data = pdata,
  cluster = ~iso_o
)

# First stage with one instrument
first_stage_2 <- feols(
  log(openness) ~ log(pred_openness) | iso_o + Year,
  data = pdata,
  cluster = ~iso_o
)

# View results
summary(first_stage_1)
summary(first_stage_2)
summary(gravity_model)

##################################################
##########Instrument regressions##################
##################################################


fe_model_1_iv <- feols(
  rop ~ kl_index + exp + uncertainty_origin| iso_o| 
    log(openness) ~ log(pred_openness),
  data = pdata,
  cluster = ~iso_o
)

fe_model_2_iv <- feols(
  rop ~ kl_index + exp + uncertainty_origin| iso_o + Year| 
    log(openness) ~ log(pred_openness),
  data = pdata,
  cluster = ~iso_o
)

fe_model_3_iv <- feols(
  rop ~ kl_index + exp + uncertainty_origin| iso_o | 
    log(openness) ~ log(pred_openness) + log(pred_openness_lag),
  data = pdata,
  cluster = ~iso_o
)

fe_model_4_iv <- feols(
  rop ~ kl_index + exp + uncertainty_origin| iso_o + Year| 
    log(openness) ~ log(pred_openness) + log(pred_openness_lag),
  data = pdata,
  cluster = ~iso_o
)

summary(fe_model_1_iv)
summary(fe_model_2_iv)
summary(fe_model_3_iv)
summary(fe_model_4_iv)
