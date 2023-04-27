################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(lmtest)
library(broom)
library(sandwich)
library(kableExtra)

################################################################################
#' Coskewness x LTD
################################################################################

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")


df <- stock_data_monthly

df <- df %>% 
  arrange(company, YearMonth) %>% 
  group_by(company) %>% 
  mutate(lead_ret = lead(monthly_ret)) %>% 
  na.omit(lead_ret) %>% 
  ungroup() %>%
  group_by(YearMonth) %>% 
  filter(length(unique(company)) >= 5) %>%
  mutate(coskewness_quantile = rank(coskewness, ties.method = "first")/length(coskewness),
         coskewness_quintile = case_when(coskewness_quantile <= 0.2 ~ 1,
                                         coskewness_quantile > 0.2 & coskewness_quantile <= 0.4 ~ 2,
                                         coskewness_quantile > 0.4 & coskewness_quantile <= 0.6 ~ 3,
                                         coskewness_quantile > 0.6 & coskewness_quantile <= 0.8 ~ 4,
                                         coskewness_quantile > 0.8 & coskewness_quantile <= 1 ~ 5)) %>% 
  group_by(YearMonth, coskewness_quintile) %>% 
  mutate(ltd_nll_quantile = rank(ltd_nll, ties.method = "first")/length(ltd_nll),
         ltd_nll_quintile = case_when(ltd_nll_quantile <= 0.2 ~ 1,
                                  ltd_nll_quantile > 0.2 & ltd_nll_quantile <= 0.4 ~ 2,
                                  ltd_nll_quantile > 0.4 & ltd_nll_quantile <= 0.6 ~ 3,
                                  ltd_nll_quantile > 0.6 & ltd_nll_quantile <= 0.8 ~ 4,
                                  ltd_nll_quantile > 0.8 & ltd_nll_quantile <= 1 ~ 5)) %>% 
  group_by(YearMonth, coskewness_quintile, ltd_nll_quintile) %>% 
  mutate(mw_ltd_nll_quintile = market_value/sum(market_value)) %>% 
  ungroup() 


# Now let's do a weight check

weight_check <- df %>% 
  group_by(YearMonth, coskewness_quintile, ltd_nll_quintile) %>% 
  summarise(mw = sum(mw_ltd_nll_quintile)) %>% 
  pull(mw) %>% 
  all.equal(., rep(1, times = length(.))) # OK

# Function to add significance level and percentage to return numbers

add_significance_level <- function(data, p_value){
  
  # Data is a tibble/data frame
  
  aux <- vector(mode = "numeric", length = length(data))
  
  for(i in 1:length(data)){
    if(p_value[i] < 0.1 & p_value[i] > 0.05){
      aux[i] <- paste0(data[i], "\\%", "*")
    } else if(p_value[i] <= 0.05 & p_value[i] > 0.01){
      aux[i] <- paste0(data[i], "\\%", "**")
    } else if(p_value[i] <= 0.01){
      aux[i] <- paste0(data[i], "\\%", "***")
    } else{
      aux[i] <- paste0(data[i], "\\%")
    }
  }
  
  return(aux)
}

# Calculate 1 month ahead value-weighted returns for each quintile

fut_monthly_ret <- df %>% 
  group_by(YearMonth, coskewness_quintile, ltd_nll_quintile) %>% 
  mutate(future_ret = sum(mw_ltd_nll_quintile*lead_ret)) %>% 
  filter(row_number(desc(date)) == 1) %>% 
  ungroup() %>% 
  arrange(company, YearMonth)

# Note that we now only keep one company, because we are only interested in the
# portfolio return, i.e. future ret per quintile and YearMonth



# Must check that there are equally many observations in each ltd_nll_quintile for each 
# coskewness_quantile and date in each city, i.e. 1 per quintile for each date in each city. This is 
# because we only need the future return for each ltd_nll_quintile, coskewness_quintile, YearMonth and city.

fut_monthly_ret %>% 
  group_by(YearMonth, coskewness_quintile, ltd_nll_quintile) %>% 
  summarise(n = length(company)) %>% 
  pull(n) %>% 
  all.equal(current = rep(1, 6525))

# Check that there are 5 quintiles for every YearMonth for every city and coskewness_quintile 

fut_monthly_ret %>% 
  group_by(YearMonth, coskewness_quintile) %>% 
  summarise(n = length(ltd_nll_quintile)) %>% 
  pull(n) %>% 
  unique()



fut_monthly_ret <- fut_monthly_ret %>% 
  select(YearMonth, coskewness_quintile, ltd_nll_quintile, future_ret) %>% 
  arrange(YearMonth, coskewness_quintile, ltd_nll_quintile) %>% 
  ungroup()




# test_future_ret 
fut_monthly_ret_2 <- fut_monthly_ret %>% 
  group_by(YearMonth, coskewness_quintile) %>% 
  summarise(diff_ret = future_ret[ltd_nll_quintile == 5] - future_ret[ltd_nll_quintile = 1]) %>% 
  ungroup()  



calculate_double_sorts <- function(x, col_name){
  
  aux_coskewness <- as.numeric(substr(col_name, 1, 1))
  
  aux_df <- fut_monthly_ret_2 %>% 
    filter(coskewness_quintile == aux_coskewness)
  
  result <- x %>% 
    filter(coskewness_quintile == aux_coskewness) %>% 
    group_by(ltd_nll_quintile) %>% 
    summarise(mean_future_ret = mean(future_ret, na.rm = T),#,
              newey_west_se = lm(future_ret ~ 1) %>% 
                lmtest::coeftest(., vcov = sandwich::NeweyWest(lm(future_ret ~ 1))) %>% 
                broom::tidy() %>% 
                .$p.value) %>% 
    rename(Return = mean_future_ret,
           p_value = newey_west_se,
           Quintile = ltd_nll_quintile) %>% 
    mutate(Quintile = as.character(Quintile)) %>% 
    add_row(Quintile = "Strong - Weak", Return = 
              mean(aux_df$diff_ret, na.rm = T),
            p_value = lm(aux_df$diff_ret ~ 1) %>% 
              lmtest::coeftest(., vcov = 
                                 sandwich::NeweyWest(
                                   lm(aux_df$diff_ret ~ 1))
              ) %>% 
              broom::tidy() %>% 
              .$p.value) %>% 
    mutate(Return = round(Return*100, digits = 3),
           Return = add_significance_level(data = Return, p_value = p_value),
           p_value = round(p_value, digits = 3),
           Quintile = case_when(Quintile == "1" ~ "1 Weak LTD",
                                Quintile == "5" ~ "5 Strong LTD",
                                TRUE ~ Quintile)) %>% 
    select(-p_value) 
  
  names(result) <- c("Quintile", col_name)
  
  return(result)
  
}

calculate_double_sorts(fut_monthly_ret, col_name = "1 Low Coskewness")


named_sorts <- c("1 Low Coskewness", "2", "3", "4", "5 High Coskewness")

estimates <- named_sorts %>% 
  map(., ~{calculate_double_sorts(fut_monthly_ret, 
                                  col_name = .x)})


estimates_df <- estimates[[1]] %>% 
  left_join(estimates[[2]], by = c("Quintile")) %>% 
  left_join(estimates[[3]], by = c("Quintile")) %>% 
  left_join(estimates[[4]], by = c("Quintile")) %>% 
  left_join(estimates[[5]], by = c("Quintile"))





table_3 <- kable(estimates_df,
                 format = "latex",
                 booktabs = T,
                 escape = F,
                 col.names = c("Quintile", "1 Low Coskewness", 
                               "2", "3", "4", "5 High coskewness"),
                 caption = "Future returns sorted on coskewness and LTD",
                 label = "coskewness_ltd_nll_sorted_future_returns") %>% 
  kable_styling() %>% 
  TexTools::ltx_caption(., tbl_note = 
                          "Stocks have been sorted into quintiles based on their
                        estimated coskewness and LTD, but using the optimized negative log-likelihood
                        function as a decision criterion for choosing the optimal
                        copula specification. For each quintile, a value weighted return
                        for month t+1 has been calculated using weights from
                        month t. This gives 1 time series per quintile of value
                        weighted portfolio returns in month t+1. The column 
                        'Returns' report the time average of these series. *, **
                        and *** indicate statistical significance at,
                        respectively, the 10\\%, 5\\% and 1\\% significance level.
                        
                        Note that in the presence of tied coskewness and/or LTD values, the first 
                        observation with that coskewness/LTD value is ranked lower than the 
                        following observation with the same coskewness/LTD value. This pattern
                        repeats in the presence of more than two ties. T-statistics for
                        all the hypothesis testing have been calculated using Newey
                        and West (1987) standard errors." , 
                        print_tbl = TRUE) 

# Save table as Rdata before choosing position, so that we may easily reposition
# the table later, without having to recalculate the table.

save(table_3, file = "data/rdata/rdata_tables/double_sorts_coskewness_ltd_nll.Rdata")

# Load the table and set the positioning

load("data/rdata/rdata_tables/double_sorts_coskewness_ltd_nll.Rdata")
table_3 <- table_3 %>% 
  TexTools::ltx_placement(tbl_placement = "H")


TexTools::write_tex(table_3, path = "_tables/double_sorts_coskewness_ltd_nll")    


