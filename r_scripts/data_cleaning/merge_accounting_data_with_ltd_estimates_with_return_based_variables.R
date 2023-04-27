################################################################################
#' Load packages
################################################################################

library(tidyverse)

################################################################################
#' Load data
################################################################################

# ltd_estimates_with_return_based_variables

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

# accounting data

load("data/rdata/cleaned_data_for_analysis/ff-factors/ff5_data_cleaned.Rdata")

ff5_df <- ff5_df %>% 
  mutate(YearMonth = zoo::as.yearmon(date)) %>% 
  select(YearMonth, company, tot_assets, tot_liabilities, com_shares_outstanding, price, country) %>% 
  drop_na()

# Merge data

# Not all names match because we do not have unique names due to lack of mnemonics in the oldest dataset.
# Only a few companies have duplicates, so we remove them and add the accounting data to the rest by merging on
# the names without mnemonics added to the end. 

ff5_2 <- ff5_df %>% 
  mutate(company2 = company,
    company = str_sub(company, end = -7),
         company = str_squish(company))

ff5_2[ff5_2 %>% select(YearMonth, company) %>% duplicated() %>% which(), ] %>% View()

remove_companies <- ff5_2[ff5_2 %>% select(YearMonth, company) %>% duplicated() %>% which(), ] %>% 
  pull(company) %>% 
  unique()

ff5_2 <- ff5_2 %>% 
  filter(!(company %in% remove_companies))

stock_data_monthly_2 <- stock_data_monthly %>% 
  mutate(company = ifelse((str_ends(company, "norway")), str_sub(company, end = -9),company),
         company = ifelse((str_ends(company, "sweden")), str_sub(company, end = -9),company),
         company = ifelse((str_ends(company, "denmark")), str_sub(company, end = -10),company),
         company = ifelse((str_ends(company, "finland")), str_sub(company, end = -10),company),
         company = str_squish(company))

stock_data_monthly_2 <- stock_data_monthly_2 %>% 
  left_join(ff5_2, by = c("company", "YearMonth", "country"))

# Save file

save(stock_data_monthly_2, file = "data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly_with_accounting_variables.Rdata")
