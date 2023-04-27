################################################################################
#' Load packages
################################################################################

library(tidyverse)

################################################################################
#' Load data
################################################################################

load("data/rdata/rdata_factors/ff5/nordic_static.Rdata")
load("data/rdata/rdata_nordic_index/nordic_index.Rdata")


################################################################################
#' Merge static data with panel data on company name
################################################################################

# Change 'name' to 'company' in the static dataset

nordic_static <- nordic_static %>% 
  rename(company = name)

df <- inner_join(nordic_index, nordic_static, by = "company")

# Let's check that all country rows match up

all.equal(df$country.x, df$country.y) # There is not a perfect match, why not?


