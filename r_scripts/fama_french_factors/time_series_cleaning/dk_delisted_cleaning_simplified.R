################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)

################################################################################
#' Load data
################################################################################

dk_delisted <- read_excel("data/worldscope_datastream/delisted/worldscope_copenhagen_delisted_returns_mnemonic.xlsx")

colnames(dk_delisted)[3] <- "date"
# Make all columns numeric except "date".

# dk_delisted <- dk_delisted %>%
#   mutate(across(!c(Name1, Symbol, date), as.numeric))

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(dk_delisted) <- str_remove_all(colnames(dk_delisted), "\\.\\.\\.(.+)")



# Remove all columns that are named "#ERROR"

remove_columns_1 <- which(stringr::str_detect(colnames(dk_delisted), "#ERROR"))
dk_delisted <- dk_delisted[, -remove_columns_1]
rm(remove_columns_1)


# Remove columns without names

# empty_columns <- colnames(dk_delisted) %>% 
#   is.na() %>%
#   which()

# dk_delisted <- dk_delisted[, -empty_columns]
# rm(empty_columns)

# Identify which columns are price columns (except the first three columns)

# price_columns <- which(!stringr::str_detect(colnames(dk_delisted), " - "))

price_columns <- which(!(str_ends(colnames(dk_delisted), " - OPERATING INCOME") | 
                           str_ends(colnames(dk_delisted), " - TOTAL ASSETS") |
                           str_ends(colnames(dk_delisted), " - TOTAL LIABILITIES") |
                           str_ends(colnames(dk_delisted), " - COMMON SHARES OUTSTANDING") |
                           str_ends(colnames(dk_delisted), " - TOT RETURN IND")))
price_columns <- price_columns[-c(1:3)]


# Want to add the ending " - PRICE" to the columns indexed by price_columns

for(i in price_columns){
  # if(!(str_ends(colnames(dk_delisted[i]), " - OPERATING INCOME") | 
  #      str_ends(colnames(dk_delisted[i]), " - TOTAL ASSETS") |
  #      str_ends(colnames(dk_delisted[i]), " - TOTAL LIABILITIES") |
  #      str_ends(colnames(dk_delisted[i]), " - COMMON SHARES OUTSTANDING") |
  #      str_ends(colnames(dk_delisted[i]), " - TOT RETURN IND"))){
  #   
  #   colnames(dk_delisted)[i] <- paste0(colnames(dk_delisted[i]), " - PRICE")
  #   
  # }
  colnames(dk_delisted)[i] <- paste0(colnames(dk_delisted[i]), " - PRICE")
}

rm(price_columns)
# colnames(dk_delisted)[1] <- "date"


# Want to remove columns that does not have a mnemonics header (a valid mnemonic in the
# first row of the dataframe)

missing_mnemonic <- which(as.character(dk_delisted[1, ]) == "NA")
missing_mnemonic <- missing_mnemonic[-1]

dk_delisted <- dk_delisted[, -missing_mnemonic]

rm(missing_mnemonic)
# Want to switch all the column names before the hyphen to the corresponding one
# in the Name column. Match on mnemonic

mnemonics <- dk_delisted[, 2] %>% drop_na() %>% unlist()
mnemonics_match <- dk_delisted[1, 4:ncol(dk_delisted)] %>% unlist()

mnemonics_list <- split(dk_delisted$Name, dk_delisted$Symbol)

for(i in 1:length(mnemonics)){
  test_1 <- which(mnemonics_match == mnemonics[i])
  
  # Break in case there is no matching mnemonic
  
  if(length(test_1) == 0){
    next
  }
  # ncol_vec <- length(test_1)
  
  replacement_string <- colnames(dk_delisted[test_1 + 3])
  
  for(j in 1:length(replacement_string)){
    
    if(str_ends(replacement_string[j], " - OPERATING INCOME")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - OPERATING INCOME")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
    if(str_ends(replacement_string[j], " - TOTAL ASSETS")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - TOTAL ASSETS")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
    if(str_ends(replacement_string[j], " - TOTAL LIABILITIES")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - TOTAL LIABILITIES")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
    if(str_ends(replacement_string[j], " - COMMON SHARES OUTSTANDING")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - COMMON SHARES OUTSTANDING")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
    if(str_ends(replacement_string[j], " - TOT RETURN IND")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - TOT RETURN IND")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
    if(str_ends(replacement_string[j], " - PRICE")){
      replacement_string[j] <- str_replace(replacement_string[j],  
                  str_sub(replacement_string[j], 
                          1, 
                          str_locate(replacement_string[j], " - PRICE")[1]-1),
                  mnemonics_list[[mnemonics[i]]])
    }
    
  }
  
  colnames(dk_delisted)[test_1 + 3] <- replacement_string
  
  
  # colnames(dk_delisted)[test_1 + 3] <- str_replace(colnames(dk_delisted)[test_1 + 3],  
  #                                                str_sub(colnames(dk_delisted)[test_1 + 3], 
  #                                                        1, 
  #                                                        str_locate(colnames(dk_delisted)[test_1 + 3], " -")[1]-1),
  #                                                mnemonics_list[[mnemonics[i]]])
  
}

# Now we can remove the first two columns and the first row of dk_delisted. Also 
# remove all rows after the dates end, since these give problems with pivoting 
# the data (they give duplicated rows)

dk_delisted_copy <- dk_delisted

dk_delisted <- dk_delisted[-1, ] %>% 
  select(-c(Name, Symbol)) %>% 
  slice_head(n = 314)


# dk_delisted <- dk_delisted_copy

# Pivot data to long format

foo <- 
  dk_delisted %>% 
  pivot_longer(cols = -date) %>% 
  mutate(var_type = str_extract(name, "(OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING|PRICE|TOT RETURN IND)"),
         name = str_remove(name, "- (OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING|PRICE|TOT RETURN IND)")) %>% 
  pivot_wider(id_cols = c(date, name), names_from = var_type, values_from = value) %>% 
  transmute(date = lubridate::date(date),
            company = str_squish(name),
            op_inc = as.numeric(`OPERATING INCOME`),
            tot_assets = as.numeric(`TOTAL ASSETS`),
            tot_liabilities = as.numeric(`TOTAL LIABILITIES`),
            com_shares_outstanding = as.numeric(`COMMON SHARES OUTSTANDING`),
            price = as.numeric(`PRICE`),
            tot_ret_index = as.numeric(`TOT RETURN IND`)) %>% 
  arrange(company, date)

dk_delisted_long <- foo


