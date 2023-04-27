################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(lmtest)
library(broom)
library(sandwich)
library(kableExtra)
library(fixest)

################################################################################
#' Load data
################################################################################

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

# Adding the Fama-French factors

load("data/rdata/cleaned_data_for_analysis/ff-factors/smb_and_hml.Rdata")

# Merge the two datasets

df <- stock_data_monthly

portfolio_returns <- portfolio_returns %>% 
  rename(YearMonth = yearmonth)

df <- df %>% 
  left_join(portfolio_returns, by = c("YearMonth"))

# Next we have to calculate long short portfolios of strong LTD stocks minus
# weak LTD stocks and their value-weighted monthly returns

df <- df %>% 
  arrange(company, YearMonth) %>% 
  group_by(company) %>% 
  mutate(lead_ret = lead(monthly_ret)) %>% 
  na.omit(lead_ret) %>% 
  ungroup() %>%
  group_by(YearMonth) %>% 
  mutate(quantile = rank(ltd, ties.method = "first")/length(ltd),
         quintile = case_when(quantile <= 0.2 ~ 1,
                              quantile > 0.2 & quantile <= 0.4 ~ 2,
                              quantile > 0.4 & quantile <= 0.6 ~ 3,
                              quantile > 0.6 & quantile <= 0.8 ~ 4,
                              quantile > 0.8 & quantile <= 1 ~ 5)) %>% 
  # distinct(city, YearMonth, quintile) %>% 
  # group_by(city, YearMonth) %>% 
  # summarise(n = length(quintile)) %>% 
  # pull(n) %>% 
  # unique()
  #        #quintile_2 = quantile(ltd, seq(0, 1, 0.2)),
  #        #quintile_3 = assign_quintile(ltd)) %>% 
  
  group_by(YearMonth, quintile) %>% 
  mutate(mw_quintile = market_value/sum(market_value)) %>% 
  ungroup() 


# Weight check

weight_check <- df %>% 
  group_by(YearMonth, quintile) %>% 
  summarise(mw = sum(mw_quintile)) %>% 
  pull(mw) %>% 
  all.equal(., rep(1, times = length(.))) 

fut_monthly_ret <- df %>% 
  group_by(YearMonth, quintile) %>% 
  mutate(future_ret = sum(mw_quintile*lead_ret)) %>% 
  filter(row_number(desc(date)) == 1) %>% 
  ungroup() %>% 
  arrange(company, YearMonth)


# Must check that there are equally many observations in each quintile for each 
# date in each city, i.e. 1 per quintile for each date in each city. This is 
# because we only need the future return for each quintile, YearMonth and city.

fut_monthly_ret %>% 
  group_by(YearMonth, quintile) %>% 
  summarise(n = length(company)) %>% 
  pull(n) %>% 
  all.equal(current = rep(1, 1305))

# Check that there are 5 quintiles for every YearMonth for every city. 

fut_monthly_ret %>% 
  group_by(YearMonth) %>% 
  summarise(n = length(quintile)) %>% 
  pull(n) %>% 
  unique()


# Select only the variables we are interested in

# fut_monthly_ret <- fut_monthly_ret %>% 
#   select(YearMonth, quintile, future_ret) %>% 
#   arrange(YearMonth, quintile) %>% 
#   ungroup()

regression_data <- fut_monthly_ret %>% 
  arrange(YearMonth, quintile) %>% # Adding this changes the mean diff_ret from negative to positive, but I dont understand why
  group_by(YearMonth) %>% 
  summarise(diff_ret = future_ret[quintile == 5] - future_ret[quintile = 1]) %>% #,
            #market_return = sum((market_value/sum(market_value))*monthly_ret)) %>% 
  ungroup()

mean(regression_data$diff_ret)
# Now we have the difference returns. Must add the hml factors



regression_data <- regression_data %>% 
  left_join(portfolio_returns, by = c("YearMonth"))

# Lastly, we must calculate the market returns and add them to the regression data
market_returns <- df %>% 
  group_by(YearMonth) %>% 
  summarise(market_return = sum((market_value/sum(market_value))*monthly_ret)) %>% 
  select(YearMonth, market_return)

regression_data <- regression_data %>% 
  left_join(market_returns, by = c("YearMonth"))

reg_1 <- feols(diff_ret ~ market_return, data = regression_data)
reg_2 <- feols(diff_ret ~ market_return + hml, data = regression_data)
reg_3 <- feols(diff_ret ~ market_return + hml + smb, regression_data)

reg_table_1 <- etable(list(reg_1, reg_2, reg_3),
                      tex = TRUE,
                      label = "trading_strategy_label",
                      title = "Trading strategy",
                      dict = c(diff_ret = "$LTD5_{r_{t+1}} - LTD1_{r_{t+1}}$", market_return = "$r_{m} - r_{f}$", hml = "HML", 
                               smb = "SMB")) %>% 
  TexTools::ltx_caption_robust(., tbl_note = 
                                 "Regression of one-month-ahead value-weigthed long  short portfolio returns 
                               (strong LTD stocks minus weak LTD stocks) on the
                               factors from the Fama and French (1993) three-factor model . (1) is a univariate regression
                               on the market return in excess of the risk-free rate, (2) adds
                               the HML factor, and (3) adds the SMB factor. The construction of the 
                               HML and SMB factors are outlined in the Appendix in section 7.2.") %>% 
  TexTools::ltx_placement(tbl_placement = "H")


TexTools::write_tex(reg_table_1, path = "_tables/long_short_reg")
