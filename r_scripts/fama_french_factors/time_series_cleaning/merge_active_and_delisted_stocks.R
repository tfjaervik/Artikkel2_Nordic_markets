################################################################################
#' Load packages
################################################################################

library(tidyverse)

################################################################################
#' Load data
################################################################################

# NOTE: SHOULD DO SOME ADDITIONAL CLEANING WHEN THE DATA IS MERGED. FOR EXAMPLE, 
# REMOVE OCCURENCES OF NEGATIVE OPERATING INCOME.


# Active stocks

load("data/rdata/cleaned_data_for_analysis/ff-factors/denmark_active_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/sweden_active_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/norway_active_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/finland_active_cleaned.Rdata")

# Delisted stocks

load("data/rdata/cleaned_data_for_analysis/ff-factors/denmark_delisted_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/sweden_delisted_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/norway_delisted_cleaned.Rdata")
load("data/rdata/cleaned_data_for_analysis/ff-factors/finland_delisted_cleaned.Rdata")

# rbind() and arrange()

ff5_df <- rbind(dk_active_long, se_active_long,
                no_active_long, fi_active_long,
                dk_delisted_long, se_delisted_long,
                no_delisted_long, fi_delisted_long) %>% 
  arrange(country, company, date)

# Save file

save(ff5_df, file = "data/rdata/cleaned_data_for_analysis/ff-factors/ff5_data_cleaned.Rdata")
