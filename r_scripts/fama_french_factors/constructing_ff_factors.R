################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(zoo)

################################################################################
#' Load data
################################################################################

load("data/rdata/cleaned_data_for_analysis/ff-factors/ff5_data_finished.Rdata")

# add year and yearmonth variable. Also, there is a problem with the company netbooster,
# so we remove it from the data

ff5_df <- ff5_df %>% 
  mutate(year = lubridate::year(date),
         yearmonth = as.yearmon(date),
         month = lubridate::month(date)) %>% 
  filter(company != "NETBOOSTER DK:NBO")

# Market cap (size) for year t is calculated by multiplying the price at 31.12 in year
# t-1 each year with shares outstanding on the same date. 

size <- ff5_df %>%
  group_by(company, year) %>% 
  slice(which.max(cumsum(!is.na(price) & !is.na(com_shares_outstanding)))) %>% # Keep only last observation of every year
  ungroup() %>% 
  filter(month == 12) %>% # Keep only observations where the last yearly observation is in december
  mutate(market_cap = price*com_shares_outstanding) %>% 
  select(company, year, market_cap) %>% 
  mutate(year = year + 1) %>% 
  distinct()


# For some reason, total assets and liabilites fluctuate during the year, though I thought it was yearly
# Hence, we keep only the last observation of every year, as this is likely what would be the yearly estimate
book_equity <- ff5_df %>% 
  group_by(company, year) %>% 
  slice(which.max(cumsum(!is.na(price) & !is.na(com_shares_outstanding)))) %>% # Keep only last observation of every year
  ungroup() %>% 
  select(company, year, tot_assets, tot_liabilities, yearmonth, month) %>% 
  mutate(be = tot_assets - tot_liabilities,
         year = year + 1,
         yearmonth = yearmonth + 1/12, 
         month = lubridate::month(yearmonth)) %>% 
  select(company, year, be, yearmonth, month) %>% 
  distinct()

book_to_market <- book_equity %>% 
  left_join(size, by = c("company", "year")) %>% 
  mutate(bm = be/market_cap) %>% 
  drop_na(bm) %>% 
  select(company, year, yearmonth, month, market_cap, bm) %>% 
  group_by(year) %>% 
  mutate(size_break = median(market_cap),
         low = quantile(bm, probs = c(0.3)),
         high = quantile(bm, probs = c(0.7))) %>% 
  ungroup() %>% 
 mutate(month = 6)

# Next we want to classify the different stocks for every year

portfolio_classes <- book_to_market %>% 
  mutate(size = case_when(market_cap <= size_break ~ "s",
                          market_cap > size_break ~ "b"),
         bm_class = case_when(bm <= low ~ "l",
                              bm >= high ~ "h",
                              .default = "n"),
         portfolio_sorts = paste0(size, "/", bm_class)) %>% 
  select(company, year, month, market_cap, portfolio_sorts)

# Next we merge the desired portfolio sorts together with market cap back into the original data, but one year ahead


ff5_df <- ff5_df %>% 
  distinct() %>% 
  left_join(portfolio_classes, by = c("company", "year", "month")) %>% 
  group_by(company) %>%
  fill(portfolio_sorts)
  
# Must set portfolio weights for each stock in every portfolio sort in june every year. These weights
# are kept constant from july through june of next year, and then they are recalculated. We can then merge them back
# on company and yearmonth + 1 (merge them back in july) and then use fill() to get the correct weights for every time period.
# Lastly, we compute value_weighted portfolio returns for every portfolio sort


# portfolio_weights <- ff5_df %>% 
#   select(company, yearmonth, month, market_cap, portfolio_sorts, price, com_shares_outstanding) %>% 
#   #filter(month == 6) %>% 
#   mutate(market_cap_2 = price*com_shares_outstanding) %>%
#   drop_na(portfolio_sorts) %>% 
#   group_by(yearmonth, portfolio_sorts) %>% 
#   mutate(portfolio_weights = market_cap_2/sum(market_cap_2)) %>% 
#   ungroup() %>% 
#   select(-c(price, com_shares_outstanding, market_cap_2, market_cap)) %>% 
#   drop_na() %>% 
#   distinct() %>% 
#   #mutate(yearmonth = yearmonth + 1/12) %>% 
#   select(company, yearmonth, portfolio_weights, portfolio_sorts)  

ff5_df <- ff5_df %>% 
  #select(company, yearmonth, month, market_cap, portfolio_sorts, price, com_shares_outstanding) %>% 
  #filter(month == 6) %>% 
  mutate(market_cap_2 = price*com_shares_outstanding) %>%
  drop_na(portfolio_sorts) %>% 
  group_by(yearmonth, portfolio_sorts) %>% 
  mutate(portfolio_weights = market_cap_2/sum(market_cap_2)) %>% 
  ungroup() %>% 
  select(-c(market_cap_2, market_cap)) %>% 
  drop_na() #%>% 
  #distinct() %>% 
  #mutate(yearmonth = yearmonth + 1/12) %>% 
  #select(company, yearmonth, portfolio_weights, portfolio_sorts)


# portfolio_weights <- ff5_df %>% 
#   select(company, yearmonth, month, market_cap, portfolio_sorts, price, com_shares_outstanding) %>% 
#   filter(month == 6) %>% 
#   mutate(market_cap_2 = price*com_shares_outstanding) %>%
#   drop_na() %>% 
#   group_by(yearmonth, portfolio_sorts) %>% 
#   mutate(portfolio_weights = market_cap_2/sum(market_cap_2)) %>% 
#   ungroup() %>% 
#   select(-c(price, com_shares_outstanding, market_cap_2)) %>% 
#   drop_na() %>% 
#   distinct() %>% 
#   mutate(yearmonth = yearmonth + 1/12) %>% 
#   select(company, yearmonth, portfolio_weights, portfolio_sorts)  
  #distinct() 



# Check that weights sum to 1 (OK)

# portfolio_weights %>%
#   group_by(yearmonth, portfolio_sorts) %>%
#   summarise(n = sum(portfolio_weights)) %>%
#   pull(n) %>%
#   unique()

  ff5_df %>%
    group_by(yearmonth, portfolio_sorts) %>%
    summarise(n = sum(portfolio_weights)) %>%
    pull(n) %>%
    unique()


# Merge weights back into ff5_df

# ff5_df <- ff5_df %>% 
#   left_join(portfolio_weights, by = c("company", "yearmonth", "portfolio_sorts")) #%>% 
  #group_by(company) %>% 
  #fill(portfolio_weights) %>% 
  #ungroup()

# Check that weights within each group adds to 1

ff5_df %>% 
  drop_na() %>% 
  group_by(yearmonth, portfolio_sorts) %>% 
  summarise(n = sum(portfolio_weights)) %>% 
  #mutate(date = as.Date(yearmonth)) %>% 
  #filter(date >= "1998-07-01") %>% 
  pull(n) %>% 
  unique()

# Next we must compute the returns for all the 2x3 portfolios. We do it one by one


s_l <- ff5_df %>% 
  filter(portfolio_sorts == "s/l") %>% 
  group_by(yearmonth) %>% 
  mutate(sl_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, sl_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)

s_n <- ff5_df %>% 
  filter(portfolio_sorts == "s/n") %>% 
  group_by(yearmonth) %>% 
  mutate(sn_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, sn_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)

s_h <- ff5_df %>% 
  filter(portfolio_sorts == "s/h") %>% 
  group_by(yearmonth) %>% 
  mutate(sh_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, sh_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)

b_l <- ff5_df %>% 
  filter(portfolio_sorts == "b/l") %>% 
  group_by(yearmonth) %>% 
  mutate(bl_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, bl_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)

b_n <- ff5_df %>% 
  filter(portfolio_sorts == "b/n") %>% 
  group_by(yearmonth) %>% 
  mutate(bn_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, bn_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)

b_h <- ff5_df %>% 
  filter(portfolio_sorts == "b/h") %>% 
  group_by(yearmonth) %>% 
  mutate(bh_returns = sum(portfolio_weights*ret)) %>% 
  select(year, yearmonth, bh_returns) %>% 
  distinct() %>% 
  arrange(yearmonth)


portfolio_returns <- b_h %>% 
  left_join(b_n, by = c("year", "yearmonth")) %>% 
  left_join(b_l, by = c("year", "yearmonth")) %>% 
  left_join(s_h, by = c("year", "yearmonth")) %>% 
  left_join(s_n, by = c("year", "yearmonth")) %>% 
  left_join(s_l, by = c("year", "yearmonth"))

# Now that we have the portfolio returns, we can create the factor returns

# Fama-French defines size smb as 1/3(s/l + s/m + s/h) - 1/3(b/l + b/m + b/h)
# hml is defined as 1/2(s/h + b/h) - 1/2(s/l + b/l)

portfolio_returns <- portfolio_returns %>% 
  mutate(smb = (1/3)*(sl_returns + sn_returns + sh_returns) - (1/3)*(bl_returns + bn_returns + bh_returns),
         hml = (1/2)*(sh_returns + bh_returns) - (1/2)*(sl_returns + bl_returns)) %>% 
  select(yearmonth, smb, hml)

# Save ff-returns

save(portfolio_returns, file = "data/rdata/cleaned_data_for_analysis/ff-factors/smb_and_hml.Rdata")


