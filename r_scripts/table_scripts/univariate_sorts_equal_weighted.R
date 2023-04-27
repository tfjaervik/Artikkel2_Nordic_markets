library(tidyverse)
library(lmtest)
library(broom)
library(sandwich)

################################################################################
#' LTD
################################################################################



load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")
df <- stock_data_monthly


df <- df %>% 
  #filter(date >= "2000-01-01") %>% 
  arrange(company, YearMonth) %>% 
  group_by(company) %>% 
  mutate(lead_ret = lead(monthly_ret)) %>% 
  na.omit(lead_ret) %>% 
  ungroup() %>%
  group_by(YearMonth) %>% 
  filter(length(unique(company)) >= 5) %>%
  mutate(quantile = rank(ltd, ties.method = "first")/length(ltd),
         quintile = case_when(quantile <= 0.2 ~ 1,
                              quantile > 0.2 & quantile <= 0.4 ~ 2,
                              quantile > 0.4 & quantile <= 0.6 ~ 3,
                              quantile > 0.6 & quantile <= 0.8 ~ 4,
                              quantile > 0.8 & quantile <= 1 ~ 5)) %>% 
  ungroup() 



# Function to add significance level and percentage to return numbers

add_significance_level <- function(data, p_value){
  
  # Data is a tibble/data frame
  
  aux <- vector(mode = "numeric", length = length(data))
  
  for(i in 1:length(data)){
    if(p_value[i] < 0.1 & p_value[i] > 0.05){
      aux[i] <- paste0(data[i], "%", "*")
    } else if(p_value[i] <= 0.05 & p_value[i] > 0.01){
      aux[i] <- paste0(data[i], "%", "**")
    } else if(p_value[i] <= 0.01){
      aux[i] <- paste0(data[i], "%", "***")
    } else{
      aux[i] <- paste0(data[i], "%")
    }
  }
  
  return(aux)
}

# Calculate 1 month ahead equal-weighted returns for each quintile

fut_monthly_ret <- df %>% 
  group_by(YearMonth, quintile) %>% 
  mutate(future_ret = mean(lead_ret)) %>% 
  filter(row_number(desc(date)) == 1) %>% 
  ungroup() %>% 
  arrange(company, YearMonth)

# Note that we now only keep one company, because we are only interested in the
# portfolio return, i.e. future ret per quintile and YearMonth



# Must check that there are equally many observations in each quintile for each 
# date in each city, i.e. 1 per quintile for each date in each city. This is 
# because we only need the future return for each quintile, YearMonth and city.

fut_monthly_ret %>% 
  group_by(YearMonth, quintile) %>% 
  summarise(n = length(company)) %>% 
  pull(n) %>% 
  all.equal(current = rep(1, 1305))

# Check that there are 5 quintiles for every YearMonth for every city. 

fut_monthly_ret %>% 
  group_by(YearMonth) %>% 
  summarise(n = length(quintile)) %>% 
  pull(n) %>% 
  unique()



fut_monthly_ret <- fut_monthly_ret %>% 
  select(YearMonth, quintile, future_ret) %>% 
  arrange(YearMonth, quintile) %>% 
  ungroup()

 


fut_monthly_ret_2 <- fut_monthly_ret %>% 
  group_by(YearMonth) %>% 
  summarise(diff_ret = future_ret[quintile == 5] - future_ret[quintile = 1]) %>% 
  ungroup()




univariate_results <- fut_monthly_ret %>%
  group_by(quintile) %>% 
  summarise(mean_future_ret = mean(future_ret, na.rm = T),#,
            newey_west_se = lm(future_ret ~ 1) %>% 
              lmtest::coeftest(., vcov = sandwich::NeweyWest(lm(future_ret ~ 1))) %>% 
              broom::tidy() %>% 
              .$p.value) %>% 
  rename(Return = mean_future_ret,
         p_value = newey_west_se,
         Quintile = quintile) %>% 
  mutate(Quintile = as.character(Quintile)) %>% 
  add_row(Quintile = "Strong - Weak", Return = 
            mean(fut_monthly_ret_2$diff_ret, na.rm = T),
          p_value = lm(fut_monthly_ret_2$diff_ret ~ 1) %>% 
            lmtest::coeftest(., vcov = 
                               sandwich::NeweyWest(
                                 lm(fut_monthly_ret_2$diff_ret ~ 1))
            ) %>% 
            broom::tidy() %>% 
            .$p.value) %>% 
  mutate(Return = round(Return*100, digits = 3),
         Return = add_significance_level(data = Return, p_value = p_value),
         p_value = round(p_value, digits = 3),
         Quintile = case_when(Quintile == "1" ~ "1 Weak LTD",
                              Quintile == "5" ~ "5 Strong LTD",
                              TRUE ~ Quintile))

library(kableExtra)

univariate_sorts_table <- kable(univariate_results,
                                format = "latex",
                                booktabs = T,
                                col.names = c("Quintile", "Returns", "P-value"),
                                caption = "Future returns sorted on LTD",
                                label = "LTD_sorted_future_returns_equal_weighted") %>% 
  kable_styling() %>% 
  TexTools::ltx_caption(., tbl_note = 
                          "Stocks have been sorted into quintiles based on their
                        estimated LTD. For each quintile, an equal weighted return
                        for month t+1 has been calculated. This gives 1 time series per quintile of value
                        weighted portfolio returns in month t+1. The column 
                        'Returns' report the time average of these series. *, **
                        and *** indicate statistical significance at,
                        respectively, the 10\\%, 5\\% and 1\\% significance level.
                        
                        Note that in the presence of tied LTD values, the first 
                        observation with that LTD value is ranked lower than the 
                        following observation with the same LTD value. This pattern
                        repeats in the presence of more than two ties.", 
                        print_tbl = TRUE) %>% 
  TexTools::ltx_placement(tbl_placement = "ht")


TexTools::write_tex(univariate_sorts_table, path = "_tables/univariate_sorts_equal_weighted")

