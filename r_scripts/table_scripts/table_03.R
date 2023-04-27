################################################################################
#' Summary statistics for return based variables
################################################################################


##### Load data -------

# load("data/rdata/rdata_multinational/stock_data_with_ret_variables_monthly.R")

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

df <- stock_data_monthly 

which(df$monthly_ret > 2)
which(df$monthly_ret_index_2 > 2)

stock_data_monthly[c(which(df$monthly_ret > 2)), ] %>% View()

summary_data <- df %>% 
  select(beta, cokurtosis, coskewness, downside_beta, upside_beta,
         #capm_alpha,
         monthly_ret, monthly_ret_index_2) 


# End of 'Load data'---


##### Try first with gtsummary -------


library(tidyverse)
library(gtsummary)



summary_return_variables <- tbl_summary(summary_data,
                                        label = list(beta ~ "Beta",
                                                     cokurtosis ~ "Cokurtosis", coskewness ~ "Coskewness",
                                                     downside_beta ~ "Downside beta", upside_beta ~ "Upside beta",
                                                     #capm_alpha ~ "CAPM alpha", 
                                                     monthly_ret ~ "Monthly stock return",
                                                     monthly_ret_index_2 ~ "Monthly index return"),
                                        statistic = list(all_continuous() ~ "{mean} / {median} ({IQR})")) 

# Insert row as row number 6, with content c("Variable", NA)
# Rename columns 

name_aux <- paste0("N = ", 
                   formatC(nrow(summary_data), format="f", 
                           big.mark = ",", digits=0)) # Added thousands separator

tbl_df <- summary_return_variables[["table_body"]] %>% 
  select(label, stat_0) %>% 
  rename(Characteristic = label) #%>% 
  #add_row(Characteristic = "Variable", stat_0 =NA, .after = 5)

colnames(tbl_df)[which(colnames(tbl_df) == "stat_0")] <- name_aux # Do this because of strange name

library(kableExtra)

options(knitr.kable.NA = '') # Make kable read NA as blank

latex_table <- kable(tbl_df,
                     format = "latex",
                     booktabs = T,
                     caption = "Summary Return Based Variables",
                     label = "sum_ret_variables") %>% 
  row_spec(0, bold = T) %>% 
  #row_spec(6, bold = T) %>% 
  TexTools::ltx_caption(tbl_note = "Summary statistics for return based variables
                        (except LTD and UTD). It should be read as:
                        mean/median (IQR), where IQR stands for interquartile range.") %>% 
  TexTools::ltx_placement(tbl_placement = "ht")



TexTools::write_tex(latex_table, "_tables/table_03")


# End of 'Try first with gtsummary'---
