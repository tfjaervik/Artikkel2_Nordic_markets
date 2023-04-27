################################################################################
#' Load packages
################################################################################

library(tidyverse)


################################################################################
#' Load data
################################################################################

load("data/rdata/cleaned_data_for_analysis/ff-factors/filtered_time_series_data.Rdata")

msci_nordic <- read_excel("data/factors/msci_nordic_monthly_price.xls", skip = 6)
colnames(msci_nordic) <- c("date", "ret")

msci_nordic <- msci_nordic %>% 
  mutate(date = str_replace(date, "Jan ", "01-"),
         date = str_replace(date, "Feb ", "02-"),
         date = str_replace(date, "Mar ", "03-"),
         date = str_replace(date, "Apr ", "04-"),
         date = str_replace(date, "May ", "05-"),
         date = str_replace(date, "Jun ", "06-"),
         date = str_replace(date, "Jul ", "07-"),
         date = str_replace(date, "Aug ", "08-"),
         date = str_replace(date, "Sep ", "09-"),
         date = str_replace(date, "Oct ", "10-"),
         date = str_replace(date, "Nov ", "11-"),
         date = str_replace(date, "Dec ", "12-"),
         date = str_replace(date, ", ", "-"),
         date = as.Date(date, format = "%m-%d-%Y"),
         yearmonth = zoo::as.yearmon(date)) %>% 
  rename(msci_ret = ret) %>% 
  select(-date)


# Add exchange rates to each country and transform price, market value and 
# total return index. Finland is denominated in euro, so we do not change finland

load("data/rdata/rdata_exchange_rates/exch_DKK-EURO.Rdata")
load("data/rdata/rdata_exchange_rates/exch_SEK-EURO.Rdata")
load("data/rdata/rdata_exchange_rates/exch_NOK-EURO.Rdata")

# Must add country variable to the exchange rates, rbind them and then merge them
# into the data

exch_DK <- exch_DK %>% 
  mutate(country = "denmark")

exch_NO <- exch_NO %>% 
  mutate(country = "norway")

exch_SE <- exch_SE %>% 
  mutate(country = "sweden")

rates <- rbind(exch_DK, exch_NO, exch_SE)

################################################################################
#' Convert relevant variables to Euro
################################################################################

ff5_df <- ff5_df %>% 
  left_join(rates, by = c("date", "country")) %>% 
  mutate(price = ifelse(country == "denmark" | country == "sweden" | country == "norway", price/rate, price),
         tot_ret_index = 
           ifelse(country == "denmark" | country == "sweden" | country == "norway", tot_ret_index/rate, tot_ret_index),
         op_inc = ifelse(country == "denmark" | country == "sweden" | country == "norway", op_inc/rate, op_inc),
         tot_assets = ifelse(country == "denmark" | country == "sweden" | country == "norway", tot_assets/rate, tot_assets),
         tot_liabilities = ifelse(country == "denmark" | country == "sweden" | country == "norway", tot_liabilities/rate, tot_liabilities)
         ) %>% 
  group_by(company) %>% 
  mutate(ret = tot_ret_index/lag(tot_ret_index)-1,
         ret = ifelse(lag(tot_ret_index) == 0, price/lag(price), ret),
         ret = ifelse(tot_ret_index == 0, price/lag(price), ret)) %>% 
  ungroup() %>% 
  drop_na(price, tot_ret_index, op_inc, tot_assets, tot_liabilities, com_shares_outstanding, ret) %>% 
  select(-rate) 

# How many companies do we have left?

ff5_df %>% pull(company) %>% unique() %>% length() #1675

################################################################################
#' Clean sample return matrix
################################################################################

# Must check for extreme returns. We identify 
# any returns that are higher than 300% in any given month

plot(ff5_df$date, ff5_df$ret) # Should create a before and after plot


ff5_df %>% filter(ret > 3) # There are 32 returns which exceed 300 %
ff5_df %>% filter(ret < -0.8)
ff5_df %>% filter(ret > 3 & (lag(ret) < -0.67 | lead(ret) < -0.67))
ff5_df %>% filter(ret > 3 & lead(ret) < -0.5)


# What if we winzorize at the 99.9 and 0.01 percent level

ff5_df %>% mutate(ret = DescTools::Winsorize(ret, probs = c(0.001, 0.999))) %>% 
  ggplot(., aes(x = date, y = ret)) +
  geom_point()

# This leaves no extreme return observations

# Let's create a value-weighted index return

ff5_df <- ff5_df %>% 
  group_by(date) %>% 
  mutate(mw = (com_shares_outstanding*price)/sum(com_shares_outstanding*price)) %>% 
  ungroup() 

index_returns <- ff5_df %>% 
  group_by(date) %>% 
  mutate(index_ret = sum(mw*ret)) %>% 
  ungroup() %>% 
  select(date, index_ret) %>% 
  distinct() %>% 
  arrange(date) %>% 
  mutate(yearmonth = zoo::as.yearmon(date))

# Add msci returns and plot

index_returns <- index_returns %>% 
  left_join(msci_nordic, by = "yearmonth")

index_returns <- index_returns %>% 
  mutate(msci_ret = as.numeric(msci_ret),
         msci_ret = msci_ret/lag(msci_ret) - 1)

cor(index_returns$index_ret, index_returns$msci_ret, use = "complete.obs") # Correlation of 0.98
plot(index_returns$index_ret, index_returns$msci_ret)

# We save the ff5_df after cleaning extreme returns

save(ff5_df, file = "data/rdata/cleaned_data_for_analysis/ff-factors/ff5_data_finished.Rdata")



