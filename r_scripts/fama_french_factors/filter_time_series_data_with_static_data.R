################################################################################
#' Load packages
################################################################################

library(tidyverse)

################################################################################
#' Load data
################################################################################

# Time series data

load("data/rdata/cleaned_data_for_analysis/ff-factors/ff5_data_cleaned.Rdata")

# Static data

load("data/rdata/cleaned_data_for_analysis/ff-factors/nordic_static.Rdata")

# Extract all the company names that we want to keep in the time series data. 
# To do this, we extract all the company names in the static data, and keep only
# those companies in the time series data.

keep <- nordic_static %>% 
  pull(name)

# First count unique companies before removal

n_before_removal <- ff5_df %>% 
  pull(company) %>% 
  unique() %>% 
  length()

# Remove all companies that are not contained in the static data

ff5_df <- ff5_df %>% 
  filter(company %in% keep)

# Count unique companies after removal

n_after_removal <- ff5_df %>% 
  pull(company) %>% 
  unique() %>% 
  length()

# We removed a total of

(n_before_removal - n_after_removal) #494 companies

# and we are left with 1849 companies in the sample. 

# Save dataset

save(ff5_df, file = "data/rdata/cleaned_data_for_analysis/ff-factors/filtered_time_series_data.Rdata")
