## Install kare saare zaroori packages (Install Packages)

if(!require(plm)){install.packages("plm")}
if (!require("arrow")) install.packages("arrow")
library(arrow)
library(plm)
library(dplyr)
library(mFilter) 

## Isko Padhe R mei (Read file)
View(df)
df <- X1st_global
## Net Foreign Assets ke regressions kare
pdata <- pdata.frame(df, index = c("Country", "Year"))
fe_model_1 <- plm(rop ~ exp + NWnfa_shrY + kl_index, index = c("country", "Year"), data = pdata, model = "within")
fe_model_2 <- plm(rop ~ exp + NWnfa_shrY + kl_index + factor(Year), data = pdata, model = "within", effect="individual")
summary(fe_model_1)
summary(fe_model_2)


#Hpfilter lagaye yaha (Apply hpfilter)
apply_hpfilter <- function(df) {
  df <- df %>% arrange(Year)  # Ensure sorting by Year within each country
  df$prod_trend <- hpfilter(as.numeric(df$prod_index), freq = 1600, type = "lambda")$trend
  df$prod_deviation <- df$prod_index - df$prod_trend
  return(df)
}

# Har Mulq ka HPfilter banaye yahan 
pdata <- pdata %>%
  group_by(Country) %>%
  group_modify(~ apply_hpfilter(.x)) %>%
  ungroup()
pdata <- pdata.frame(pdata, index = c("Country", "Year"))
head(pdata[, c("Year", "Country", "prod_index", "prod_trend", "prod_deviation")])


## Openness wale regressions kare, pehle sirf country fixed effects

pdata <- pdata[!is.na(pdata$openness), ]  
fe_model_3 <- plm((rop) ~ lag(rop, 1) + log(openness) + kl_index + exp, index = c("country", "Year"), data = pdata, model = "within", na.action = na.exclude)
fe_model_4 <- plm((rop) ~ lag(rop, 1) + log(openness) + kl_index + prod_deviation, index = c("country", "Year"), data = pdata, model = "within", na.action = na.exclude)

### Time fixed effects ke saath
fe_model_5 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + exp + factor(Year),index = c("country", "Year"),  
                  data = pdata,  
                  model = "within",  
                  effect = "twoways")  
fe_model_6 <- plm(rop ~ lag(rop, 1) + log(openness) + kl_index + prod_deviation + factor(Year),index = c("country", "Year"),  
                  data = pdata,  
                  model = "within",  
                  effect = "twoways")  
summary(fe_model_3)
summary(fe_model_4)
summary(fe_model_5)
summary(fe_model_6)

## Baaki Openness ke variables ka regression kare 

fe_model_tot <- plm(rop ~ lag(rop, 1) + tot + kl_index + exp + factor(Year),index = c("country", "Year"),  
                  data = pdata,  
                  model = "within",  
                  effect = "twoways")  
summary(fe_model_tot)

fe_model_tariff <- plm(rop ~ lag(rop, 1) + (tariff) + kl_index + exp + factor(Year),index = c("country", "Year"),  
                    data = pdata,  
                    model = "within",  
                    effect = "twoways")  
summary(fe_model_tariff)

fe_model_exp <- plm(rop ~ lag(rop, 1) + log(exp_gdp) + kl_index + exp + factor(Year),index = c("country", "Year"),  
                       data = pdata,  
                       model = "within",  
                       effect = "twoways")  
summary(fe_model_exp)

fe_model_imp <- plm(rop ~ lag(rop, 1) + imp_gdp + kl_index + exp + factor(Year),index = c("country", "Year"),  
                       data = pdata,  
                       model = "within",  
                       effect = "twoways")  
summary(fe_model_exp)
summary(fe_model_imp)
summary(fe_model_tariff)
summary(fe_model_tot)


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
