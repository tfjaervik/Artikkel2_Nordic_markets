##### Load packages -------

library(tidyverse)
library(zoo)

# End of 'Load packages'---


# Calculate on daily dataset, and then aggregate later

load("data/rdata/cleaned_data_for_analysis/stock_data.RData")


df <- stock_data


# Remove NA values

df <- df %>% 
  na.omit()

# End of 'Load data'---

################################################################################
#' Calculate return based variables
################################################################################

# Load functions

source("r_scripts/function_scripts/return_based_variables_functions.R")


# Want to use 12 month rolling windows just as with LTD. 

# Must add yearmon column 'YearMonth' 

# df <- stock_data %>% 
#   mutate(YearMonth = zoo::as.yearmon(date))

# Split into list and operate on each dataset separately. data.table::rbindlist()
# at the end. 

data_list <- split(df, f = df$company)

# Make all the elements tibbles

data_list <- data_list %>% 
  map(., as_tibble)


return_based_variables <- data_list %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = calc_beta,
                                                 output_variable = "beta")}) %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = calc_cokurtosis,
                                                 output_variable = "cokurtosis")}) %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = calc_coskewness,
                                                 output_variable = "coskewness")}) %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = calc_downside_beta,
                                                 output_variable = "downside_beta")}) %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = calc_upside_beta,
                                                 output_variable = "upside_beta")}) %>% 
  map(., ~ {rolling_window_multivariate_modified(data = .x,
                                                 columns = c("ret", "ret_index"),
                                                 function_name = capm_alpha,
                                                 output_variable = "capm_alpha")})


rm(data_list, df)

# rolling_window_multivariate_modified(data = data_list[[1]],
#                                      columns = c("ret", "ret_index"),
#                                      function_name = capm_alpha,
#                                      output_variable = "capm_alpha") %>% View()
#                                      

# Save list to be processed in a more powerful computer

save(return_based_variables, file = "data/rdata/rdata_nordic_index/return_based_variables_list.Rdata")

load("data/rdata/rdata_nordic_index/return_based_variables_list.Rdata")

# Store dataset

df2 <- data.table::rbindlist(return_based_variables) %>%
  as_tibble() %>% # since rbindlist makes all the data frames into a data.table
  arrange(company, date) %>%
  ungroup() %>%
  mutate(across(.cols = c(beta, cokurtosis, coskewness, downside_beta,
                          upside_beta, capm_alpha), as.numeric)) # Removed ff3-alpha here since we do not have the factors yet


# Save daily dataset

# save(df2, file = "data/rdata/rdata_multinational/stock_data_with_ret_variables_daily.Rdata")
# 
# load("data/rdata/rdata_multinational/stock_data_with_ret_variables_daily.Rdata")


# Aggregate returns to monthly level. 

df3 <- df2 %>% 
  group_by(company, YearMonth) %>% 
  mutate(monthly_ret = prod(ret + 1) -1,
         monthly_ret_index = prod(ret_index + 1) -1) %>% 
  filter(date == max(date)) %>% # Keep only last day of every month
  select(-c(tot_ret_index, n_valid_ret, index_value, ret_index_total)) %>% # Removed price from the vector because it was not there for some reason
  na.omit() %>%  # Remove na values introduced by calculating all the return based variables. (12 months per company)
  ungroup()

df4 <- df3 %>% 
  mutate(across(.cols = c(beta, cokurtosis, coskewness, downside_beta,
                          upside_beta, capm_alpha), as.numeric))


df4 %>% head(n = 1000) %>% View()
df4 %>% str()

# Must also remove any potential NAs in the newly converted variables

df4 <- df4 %>% 
  na.omit()
# Export monthly dataset with return based variables

stock_data_monthly <- df4

# Add monthly_ret_index_2 calculated based on market values on the last available
# date of the month, instead of aggregating the monthly index returns. I think that 
# is more correct. 

stock_data_monthly <- stock_data_monthly %>% 
  group_by(date) %>% 
  mutate(index_value_2 = sum(market_value)) %>% 
  ungroup() %>% 
  mutate(market_weight_2 = market_value/index_value_2) %>% 
  group_by(date) %>% 
  mutate(ret_index_total_2 = sum(market_weight_2*ret)) %>% 
  ungroup() %>% 
  mutate(monthly_ret_index_2 = ret_index_total_2 - ret*market_weight_2) %>% 
    select(-c(index_value_2, market_weight_2, ret_index_total_2))

save(stock_data_monthly, file = "data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")


# load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")
# 
# stock_data_monthly$monthly_ret_index %>% summary()

