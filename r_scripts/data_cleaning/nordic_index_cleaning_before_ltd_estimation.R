################################################################################
#' Load data
################################################################################

# Active stocks

load("data/rdata/rdata_copenhagen/copenhagen_active.Rdata")
load("data/rdata/rdata_stockholm/stockholm_active.Rdata")
load("data/rdata/rdata_oslo/oslo_active.Rdata")
load("data/rdata/rdata_helsinki/helsinki_active.Rdata")

# Dead stocks

load("data/rdata/rdata_delisted/copenhagen_delisted_cleaned.Rdata")
load("data/rdata/rdata_delisted/stockholm_delisted_cleaned.Rdata")
load("data/rdata/rdata_delisted/oslo_delisted_cleaned.Rdata")
load("data/rdata/rdata_delisted/helsinki_delisted_cleaned.Rdata")


# Merge active and dead country by country, add country column
# and filter out data before january 4 1999

denmark <- rbind(df_copenhagen, df_copenhagen_active)
sweden <- rbind(df_stockholm, df_stockholm_active)
norway <- rbind(df_oslo, df_oslo_active)
finland <- rbind(df_helsinki, df_helsinki_active)

denmark <- denmark %>% 
  mutate(country = "denmark") %>% 
  filter(date >= "1999-01-04")

sweden <- sweden %>% 
  mutate(country = "sweden") %>% 
  filter(date >= "1999-01-04")

norway <- norway %>% 
  mutate(country = "norway") %>% 
  filter(date >= "1999-01-04")

finland <- finland %>% 
  mutate(country = "finland") %>% 
  filter(date >= "1999-01-04") %>% 
  drop_na(market_value)



# Add exchange rates to each country and transform price, market value and 
# total return index. Finland is denominated in euro, so we do not change finland

load("data/rdata/rdata_exchange_rates/exch_DKK-EURO.Rdata")
load("data/rdata/rdata_exchange_rates/exch_SEK-EURO.Rdata")
load("data/rdata/rdata_exchange_rates/exch_NOK-EURO.Rdata")

# Recalculate returns, will lose one observation per company, but that is OK.
# NOTE!!!!!!!!!!!!
# NOTE!!!!!!!!!!!!
# Have forgotten to group by company here, which will introduce errors in the
# first return observation of any company except for the first in each country!!
# Should remove such erroneous returns later and hope that they do not impact
# the LTD estimates too much!

denmark <- denmark %>% 
  left_join(exch_DK) %>% 
  mutate(price = price/rate,
         market_value = market_value/rate,
         tot_ret_index = tot_ret_index/rate,
         ret = tot_ret_index/lag(tot_ret_index)-1,
         ret = ifelse(lag(tot_ret_index) == 0, price/lag(price), ret),
         ret = ifelse(tot_ret_index == 0, price/lag(price), ret)) %>% 
  drop_na(ret) %>% 
  select(-rate) %>% 
  drop_na(market_value)

sweden <- sweden %>% 
  left_join(exch_SE) %>% 
  mutate(price = price/rate,
         market_value = market_value/rate,
         tot_ret_index = tot_ret_index/rate,
         ret = tot_ret_index/lag(tot_ret_index)-1,
         ret = ifelse(lag(tot_ret_index) == 0, price/lag(price), ret),
         ret = ifelse(tot_ret_index == 0, price/lag(price), ret)) %>% 
  drop_na(ret) %>% 
  select(-rate) %>% 
  drop_na(market_value)

norway <- norway %>% 
  left_join(exch_NO) %>% 
  mutate(price = price/rate,
         market_value = market_value/rate,
         tot_ret_index = tot_ret_index/rate,
         ret = tot_ret_index/lag(tot_ret_index)-1,
         ret = ifelse(lag(tot_ret_index) == 0, price/lag(price), ret),
         ret = ifelse(tot_ret_index == 0, price/lag(price), ret)) %>% 
  drop_na(ret) %>% 
  select(-rate) %>% 
  drop_na(market_value)


# Extract common dates

dates_denmark <- denmark %>% pull(date) %>% unique()
dates_sweden <- sweden %>% pull(date) %>% unique()
dates_norway <- norway %>% pull(date) %>% unique()
dates_finland <- finland %>% pull(date) %>% unique()

common_dates <- intersect(dates_denmark, dates_sweden) %>% 
  intersect(dates_norway) %>%
  intersect(dates_finland) %>% 
  as.Date(origin = "1970-01-01")

# Next we rbind the datasets and keep only common dates

nordic_index <- rbind(denmark, sweden, norway, finland) %>% 
  filter(date %in% common_dates) %>% 
  arrange(country, company, date) %>% 
  select(-price)

# Calculate total market value for each date. To be used to calculate market weights

nordic_index <- nordic_index %>% 
  group_by(date) %>% 
  mutate(index_value = sum(market_value)) %>% 
  ungroup()


# Calculate value weighted index returns.

nordic_index <- nordic_index %>% 
  mutate(market_weight = market_value/index_value) %>% 
  group_by(date) %>% 
  mutate(ret_index_total = sum(market_weight*ret)) %>% 
  ungroup() %>% 
  mutate(ret_index = ret_index_total - ret*market_weight)



# Check that weights sum to 1 for each date. 

nordic_index %>% 
  group_by(date) %>% 
  select(market_weight) %>% 
  summarise(weight_sum = sum(market_weight)) %>% 
  pull(weight_sum) %>% 
  unique # OK

# Save dataset for analysis on server

save(nordic_index, file = "data/rdata/rdata_nordic_index/nordic_index.Rdata")

