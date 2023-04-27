################################################################################
#' Return based variables definitions
################################################################################

# Create the different columns in the dataframe 

col1 <- c("$r_{t}$ ($r_{m}/r_{f}$)", 
          "LTD", 
          "$\\beta$", 
          "$\\beta^{-}$", 
          "$\\beta^{+}$",
          "Coskewness",
          "Cokurtosis",
          "size",
          "bookmarket",
          # "capm-alpha, ff-alpha",
          "HML",
          "SMB")

col2 <- c("Return at time t (market return/risk-free rate),the two first in excess
          of the risk-free rate.",
          "Lower tail dependence coefficient of individual stock returns
             and market returns. The coefficient is estimated on daily
             data with 12 month rolling windows. The estimation frequency
             is 1 month.",
          "Covariance between excess stock return and excess market return. 
             Calculated on monthly data.",
          "Downside beta estimated on daily return data from one year, 
             following Ang, Chen and Xing (2006). It is the the same as $\\beta$,
             but calculated conditional on return observations exceeding the 
             mean of the market return observations.",
          "Upside beta estimated on daily return data from one year, 
          following Ang, Chen and Xing (2006). It is the the same as $\\beta$,
          but calculated conditional on return observations being lower than the 
          mean of the market return observations.",
          "The coskewness of the daily return of a stock with the market.",
          "To cokurtosis of the daily return of a stock with the market",
          "The logarithm of market capitalization",
          "Book value divided by market capitalization, where book value is
          calculated as Total Assets minus Total Liabilities",
          # "The CAPM and Fama and French (1993) three-factor alpha of a (portfolio of)
          # stock(s). Calculated on daily data.",
          "The High minus Low factor from Fama and French (1993). Calculated from monthly
          data from TDS.",
          "The Small minus Big factor from Fama and French (1993). Calculated from monthly
          data from TDS.")

col3 <- c("TDS, Estimated", 
          "TDS, Estimated", 
          "TDS, Estimated", 
          "TDS, Estimated", 
          "TDS, Estimated",
          "TDS, Estimated",
          "TDS, Estimated",
          "TDS, Estimated",
          "TDS, Estimated",
          # "TDS, Estimated",
          "TDS, Estimated", 
          "TDS, Estimated")

df <- as_tibble(matrix(ncol = 3, nrow = length(col1)))

# Create dataframe with three columns

df_names <- c("Variable", "Definition", "Data source")

colnames(df) <- df_names

df[, 1] <- col1


df[, 2] <- col2


df[, 3] <- col3



# Create table 

def_table <- kable(df,
                   caption = "Data definitions",
                   format = "latex",
                   booktabs = T,
                   label = "data_definitions",
                   escape = F) %>% # escape = F to read latex code in table
  #kable_styling(latex_options = "scale_down") %>%
  column_spec(column = 2, width = "25em") %>% 
  column_spec(column = 1, width = "5em") %>% 
  column_spec(column = 3, width = "5em") %>% 
  row_spec(row = 0, align = "c", bold = T) %>% 
  TexTools::ltx_caption(., tbl_note = 
                          "This table summarises the definitions of the main variables
                        used in this study.") %>% 
  TexTools::ltx_placement(tbl_placement = "H")
#%>% 
#writeClipboard()


TexTools::write_tex(def_table, "_tables/data_definitions.tex")

# NOTE: Must \usepackage{array} in overleaf to render properly.


################################################################################
#' MSCI definitions
################################################################################
library(tidyverse)
library(kableExtra)
# Create the different columns in the dataframe 

col1 <- c("ALL CAP (Large + Mid + Small + Micro Cap)",
          "LARGE GROWTH Large Cap Growth",
          "LARGE CAP Large Cap",
          "LARGE VALUE Large Cap Value",
          "SMALL GROWTH Small Cap Growth",
          "SMALL CAP Small Cap",
          "SMALL VALUE Small Cap Value")



df <- as_tibble(matrix(ncol = 1, nrow = length(col1)))

# Create dataframe with three columns

df_names <- c("Indices")

colnames(df) <- df_names

df[, 1] <- col1


indices_table <- kable(df,
                       caption = "MSCI Indices",
                       format = "latex",
                       booktabs = T,
                       label = "msci_indices",
                       #escape = F
) %>% # escape = F to read latex code in table
  kable_styling() %>% 
  row_spec(row = 0, align = "c", bold = T) %>% 
  TexTools::ltx_caption(, tbl_note = 
                          "This table summarises the different indices from MSCI
                        that have been used to calculate the HML and SMB factors.
                        Note that the indices are downloaded for each country.") %>% 
  TexTools::ltx_placement(tbl_placement = "H")



TexTools::write_tex(indices_table, "_tables/MSCI_indices.tex")
