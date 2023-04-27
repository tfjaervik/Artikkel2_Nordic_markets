##### Load packages -------

library(tidyverse)
library(kableExtra)
# End of 'Load packages'---

##### Load data -------

load("data/rdata/cleaned_data_for_analysis/stock_data_monthly.Rdata")

stock_data <- df
# End of 'Load data'---

##### LTD summary -------

ltd_summary <- stock_data %>% 
  pull(ltd) %>% 
  summary() 

ltd_tibble <- enframe(c("Minimum" = unname(ltd_summary[1]), "1st Quartile" = unname(ltd_summary[2]),
                        "Median" = unname(ltd_summary[3]), "Mean" = unname(ltd_summary[4]),
                        "3rd Quartile" = unname(ltd_summary[5]), "Maximum" = unname(ltd_summary[6]))) %>% 
  pivot_wider(names_from = name, values_from = value)

ltd_tibble[1, ] <- round(ltd_tibble[1, ], digits = 3)


table_01 <- ltd_tibble %>% 
  kable(., format = "latex",
        caption = "LTD summary",
        label = "ltd_summary",
        booktab = T) %>% 
  row_spec(row = 0, align = "c", bold = T) %>% 
  TexTools::ltx_caption("This table summarises the distribution of LTD estimates
                        across the pooled sample, i.e. across all stocks and dates.") %>% 
  TexTools::ltx_placement(tbl_placement = "h")


# table_01 <- as.array(ltd_summary) %>% 
#   kable(., format = "latex")

TexTools::write_tex(table_01, "_tables/table_01")

# One version with centered numbers

table_01 <- ltd_tibble %>% 
  kable(., format = "latex",
        caption = "LTD summary",
        label = "ltd_summary_centered",
        booktab = T) %>% 
  row_spec(row = 0, align = "c", bold = T) %>%
  row_spec(row = 1, align = "c") %>% 
  TexTools::ltx_caption("This table summarises the distribution of LTD estimates
                        across the pooled sample, i.e. across all stocks and dates.") %>% 
  TexTools::ltx_placement(tbl_placement = "h")


# table_01 <- as.array(ltd_summary) %>% 
#   kable(., format = "latex")

TexTools::write_tex(table_01, "_tables/table_01_centered")

# End of 'LTD summary'---

