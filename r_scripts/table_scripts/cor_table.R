################################################################################
#' Correlation matrix return based variables
################################################################################
library(rstatix)
library(kableExtra)
load("data/rdata/rdata_nordic_index/ltd_estimates_with_return_based_variables_monthly.Rdata")

df <- stock_data_monthly

cor_data <- df %>% 
  select(ltd, utd, beta, upside_beta, downside_beta, cokurtosis, coskewness)

cor_matrix <- round(cor(cor_data), digits = 3) %>% pull_lower_triangle(., diagonal = TRUE) %>% as.data.frame() 

cor_matrix$rowname <- c("LTD", "UTD", "$\\beta$", "$\\beta^{+}$", "$\\beta^{-}$", "COKURTOSIS", "COSKEWNESS")

colnames(cor_matrix) <- c("", "LTD", "UTD", "$\\beta$", "$\\beta^{+}$", "$\\beta^{-}$", "COKURTOSIS", "COSKEWNESS")

cor_table <- kable(cor_matrix, 
                   format = "latex",
                   booktabs = T, 
                   caption = "Correlation matrix return based variables",
                   label = "cor_matrix", 
                   escape = F,
                   align = "c",) %>% 
  kable_styling(#latex_options = c("repeat_header", "scale_down")
  ) %>% 
  #landscape() %>% 
  TexTools::ltx_caption(tbl_note = "This table displays the 
                        correlation matrix for all the return
                        based variables used in this study") %>% 
  TexTools::ltx_placement(tbl_placement = "ht") 


TexTools::write_tex(cor_table, "_tables/cor_table")  
cor_table %>% writeClipboard()
