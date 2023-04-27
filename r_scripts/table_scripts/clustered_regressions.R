library(tidyverse)
library(lmtest)
library(broom)
library(sandwich)
library(kableExtra)
library(fixest)

load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

df <- stock_data_monthly
df <- df %>%
  mutate(across(.cols = c(ltd, ltd_nll, utd, cop_combo, cop_combo_nll), as.numeric)) %>%
  group_by(company) %>%
  mutate(lead_ret = lead(monthly_ret)) %>%
  na.omit(lead_ret) %>%
  ungroup()



reg_1 <- feols(lead_ret ~ ltd, cluster= c("YearMonth"), data = df)
reg_2 <- feols(lead_ret ~ ltd + beta, cluster = c("YearMonth"), data = df)
reg_3 <- feols(lead_ret ~ ltd + beta + downside_beta, cluster = c("YearMonth"), data = df)
reg_4 <- feols(lead_ret ~ ltd + beta + downside_beta + upside_beta, cluster = c("YearMonth"), data = df)

reg_table_1 <- etable(list(reg_1, reg_2, reg_3, reg_4),
                      tex = TRUE,
                      label = "reg_clustered_on_time_1",
                      title = "Future returns regressed on different factors",
                      dict = c(lead_ret = "$r_{t+1}$", ltd = "LTD", beta = "$\\beta$", 
                               downside_beta = "$\\beta^{-}$", upside_beta = "$\\beta^{+}$",
                               YearMonth = "date")) %>% 
  TexTools::ltx_caption_robust(., tbl_note = 
                                 "Regression of one-month-ahead returns on different
                               sets of control variables. The dependent variable is
                               the one-month-ahead return. (1) is a univariate regression
                               on LTD, (2) is a bivariate regression on LTD and beta,
                               (3) is a multivariate regression on LTD, beta and downside 
                               beta, and (4) is a multivariate regression on LTD, beta,
                               downside beta and upside beta. Standard errors are clustered
                               on months.") %>% 
  TexTools::ltx_placement(tbl_placement = "H")


TexTools::write_tex(reg_table_1, path = "_tables/reg_clustered_on_time_1")



reg_5 <- feols(lead_ret ~ ltd + beta + downside_beta + upside_beta + coskewness,
               cluster = c("YearMonth"), data = df)
reg_6 <- feols(lead_ret ~ ltd + beta + downside_beta + upside_beta + coskewness + cokurtosis,
               cluster = c("YearMonth"), data = df)


# Adding the Fama-French factors

load("data/rdata/cleaned_data_for_analysis/ff-factors/smb_and_hml.Rdata")

portfolio_returns <- portfolio_returns %>% 
  rename(YearMonth = yearmonth)

df <- df %>% 
  left_join(portfolio_returns, by = c("YearMonth"))

reg_7 <- feols(lead_ret ~ ltd + beta + downside_beta + upside_beta + coskewness + cokurtosis + hml,
               cluster = c("YearMonth"), data = df)

reg_8 <- feols(lead_ret ~ ltd + beta + downside_beta + upside_beta + coskewness + cokurtosis + hml + smb,
               cluster = c("YearMonth"), data = df)


reg_table_2 <- etable(list(reg_5, reg_6, reg_7, reg_8),
                      tex = TRUE,
                      label = "reg_clustered_on_time_2",
                      title = "Future returns regressed on different factors",
                      dict = c(lead_ret = "$r_{t+1}$", ltd = "LTD", beta = "$\\beta$", 
                               downside_beta = "$\\beta^{-}$", upside_beta = "$\\beta^{+}$",
                               smb = "SMB", hml = "HML", coskewness = "Coskewness",
                               cokurtosis = "Cokurtosis",
                               YearMonth = "date")) %>% 
  TexTools::ltx_caption_robust(., tbl_note = 
                                 "Regression of one-month-ahead returns on different
                               sets of control variables. The dependent variable is
                               the one-month-ahead return. (1) is a multivariate 
                               regression on LTD, beta, downside beta, upside beta and coskewness. For each regression, we
                               add one control variable. In (2) we add cokurtosis. 
                               Standard errors are clustered on months. In (3) we add the Fama-French
                               factor High minus Low (HML) and in (4) we add the Fama-French factor
                               Small minus Big (SMB).")%>% 
  TexTools::ltx_placement(tbl_placement = "H")

TexTools::write_tex(reg_table_2, path = "_tables/reg_clustered_on_time_2")





