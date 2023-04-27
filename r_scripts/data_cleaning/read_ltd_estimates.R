##### Load packages -------

library(tidyverse)
library(zoo)
library(furrr)
library(readxl)
library(lubridate)
library(data.table)

# End of 'Load packages'---



##### Load RDS data -------

# Read in RDS files, remove any NA values in the estimates, i.e. the initial 
# estimation period and any potential errors. 
# NOTE: We removed two files from the ltd_estimates_nordic_index folder because
# they were corrupted. They are listed in a README file in the folder 'README documents'.

ltd_estimates <-  list.files(path = "data/ltd_estimates_nordic_index", 
                             pattern = ".rds", full.names = TRUE) %>%
                          map(readRDS) %>% 
                          #map(~{.x %>% 
                          #      drop_na(monthly_estimate)}
                          #   ) %>% 
                          rbindlist()


# End of 'Load RDS data'---

##### Merge RDS data -------




# Look for duplicated dates for each security

ltd_estimates %>% 
  group_by(company) %>% 
  summarise(unique = length(unique(date)), total = length(date)) %>% 
  mutate(diff = total-unique,
         index = diff != 0) %>% 
  pull(index) %>% 
  which()

# There are four companies that have duplicated dates. 

duplicated_dates <- ltd_estimates %>% 
  group_by(company) %>% 
  summarise(unique = length(unique(date)), total = length(date)) 


duplicated_dates <- duplicated_dates[c(1536, 1615, 2254, 2255), ] %>% 
  pull(company)


test_data <- ltd_estimates %>% 
  filter(company %in% duplicated_dates)

test_data_copy <- test_data

test_data_copy <- test_data_copy %>% 
  duplicated() %>% 
  sum()

test_2 <- test_data[1, ] %>% unlist()
test_3 <- test_data[1902, ] %>% unlist()

test_2 == test_3


country_test <- test_data %>% 
  group_by(company) %>% 
  summarise(countries = unique(country))

# The problem is not double dates, the problem is that the same company is listed
# in two different countries. As they have different returns, we solve this by renaming
# the companies by adding their associated country of listing. 

ltd_estimates <- ltd_estimates %>%
  group_by(company) %>% 
  mutate(company = paste0(company, " - ", country))


ltd_estimates %>% filter(company %in% duplicated_dates) %>% View()
# Remove dates that are the same as the previous date (grouped by company)

ltd_estimates <- ltd_estimates %>% 
  group_by(company) %>% 
  arrange(date) %>% 
  filter(!(date == lag(date))) %>% 
  ungroup()

# Create copy of ltd_estimates, in case we need to reload it

ltd_estimates_copy <- ltd_estimates


# arrange by company and date

ltd_estimates <- ltd_estimates %>% 
  arrange(company, date)



# End of 'Clean and Merge RDS data'---

##### Split LTD/UTD etc. estimates into different columns and fill() -------

# First replace all "-" with "/" unless preceded by "e", to make all separators
# equal (had for some reason separated by "/" and then "-" for all the rest,
# which created issues with scientific notation e-10). Then split columns into 
# 5. Lastly, make the columns numeric

ltd_estimates <- ltd_estimates %>% 
  mutate(LTD_estimate = str_replace_all(monthly_estimate, "(?<!e)-", "/")) %>% 
  separate("LTD_estimate", c("ltd", "cop_combo", "utd", "ltd_nll", "cop_combo_nll"),
           sep = "/")


ltd_estimates_copy_separated <- ltd_estimates
# pull out ltd to see why NAs are introducec by coercion below

ltd_vec <- ltd_estimates %>% pull(ltd)

which(ltd_vec %in% c("NA"))

df3[90055:90200, ] %>% View()

# These can probably be explained by the fact that the optimization algorithm
# used when estimating ltd does not always converge. In that case, it has been 
# told to assign the value NA. Hence, we can delete all such cases and then see 
# if we still get NAs introduced by coercion

# Save file before dropping NAs because R is having memory issues. Delete all 
# other files and load the file back into R. 

save(ltd_estimates, file = "data/rdata/cleaned_data_for_analysis/ltd_estimates.Rdata")

rm(list = ls())

load("data/rdata/cleaned_data_for_analysis/ltd_estimates.Rdata")

ltd_estimates <- ltd_estimates %>% 
  drop_na(ltd) %>% 
  drop_na(ltd_nll)

ltd_estimates <- ltd_estimates %>% 
  mutate(ltd           = as.numeric(ltd),
         cop_combo     = as.numeric(cop_combo),
         utd           = as.numeric(utd),
         ltd_nll       = as.numeric(ltd_nll),
         cop_combo_nll = as.numeric(cop_combo_nll))

# Some NAs where introduced by coercion. This is probably because the optimization
# algorith used for the ltd estimates does not always converge, and hence it has
# returned an NA value. We remove these observations. There were only 3 warnings.

ltd_estimates <- ltd_estimates %>% 
  drop_na(ltd)

# Now it is okay. 

# End of 'Split LTD/UTD etc. estimates into different columns and fill()'---

# Rename data and save file

stock_data <- ltd_estimates


# Must deal with extreme return observations before we aggregate to monthly returns
# (optimally, this should have been dealt with before the LTD estimates, but the 
# problem only concerns some companies, as can be seen by plotting the returns)

which(stock_data$ret > 2) %>% length()
which(stock_data$ret < -0.9) %>% length()


# stock_data <- stock_data %>% 
#   filter(!(ret < -0.9 | ret > 2))


# We want to remove the smallest companies in the sample. The way we choose to do
# so is the following. We identify any company that has ever been part of the smallest
# 10 percent of companies on any date. Any such company is removed from the sample entirely. 
# This is done so that we do not get very irregular return intervals for companies that 
# only partially drop out of the sample. We do not want such irregularities in our later
# regressions. 



stock_data <- stock_data %>% 
  group_by(date) %>% 
  mutate(quantile_mv = quantile(market_value, probs = c(0.10))) %>% 
  ungroup() %>% 
  mutate(
    smallest = market_value <= quantile_mv) %>%
  group_by(company) %>% 
  mutate(delete_company = sum(smallest)) %>% 
  ungroup() %>% 
  filter(delete_company == 0) %>% 
  select(-c(quantile_mv, smallest, delete_company))



save(stock_data, file = "data/rdata/cleaned_data_for_analysis/stock_data.RData")





##### Create monthly return stock file -------

load("data/rdata/cleaned_data_for_analysis/stock_data.RData")

df <- stock_data

head(stock_data) %>% View()

library(zoo)

# Create monthly returns, remove any na's created and keep only last date of 
# every month

df <- df %>% 
  mutate(YearMonth = as.yearmon(date)) %>% 
  group_by(company, YearMonth) %>% 
  mutate(monthly_ret = prod(1 + ret) - 1,
         monthly_ret_index = prod(1 + ret_index) - 1) %>% 
  filter(date == max(date)) %>% 
  ungroup() 

# Note that for some months, the monthly return will not be completely correct,
# as the whole month may not be contained in the data sample. 
# It might be preferable to download the monthly returns and merge them into the
# dataset. 

save(df, file = "data/rdata/cleaned_data_for_analysis/stock_data_monthly.Rdata")

# End of 'Create monthly return stock file'---

