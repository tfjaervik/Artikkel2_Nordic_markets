################################################################################
#' Load packages
################################################################################


################################################################################
#' Load data
################################################################################

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

ltd_sd <- stock_data_monthly %>% 
  group_by(company) %>% 
  summarise(sd = sd(ltd)) %>% 
  pull(sd) %>% 
  mean(., na.rm = TRUE)

utd_sd <- stock_data_monthly %>% 
  group_by(company) %>% 
  summarise(sd = sd(utd)) %>% 
  pull(sd) %>% 
  mean(., na.rm = TRUE)
################################################################################
#' Economic significance univariate sorts
################################################################################

ec_sign_univariate_sorts <- 0.191*0.0028*12

0.191*12
ec_sign_univariate_sorts_utd <- -0.691*0.0024*12

ec_sign_double_sorts <- 0.432*ltd_sd*12

ec_sign_regr_1 <- 0.0303*1200*ltd_sd

ec_sign_regr_2 <- 0.0384*1200*ltd_sd
