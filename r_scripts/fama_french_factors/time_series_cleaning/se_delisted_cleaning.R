################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)

################################################################################
#' Load data
################################################################################

se_delisted <- read_excel("data/worldscope_datastream/delisted/worldscope_stockholm_delisted_returns_mnemonic.xlsx")
delisting_date <- read_excel("data/worldscope_datastream/delisted/stockholm_delisted_ISIN_delisting_date.xlsx",
                             col_types = c("guess", "guess", "guess", "date"))

################################################################################
#' Clean data
################################################################################

# Check duplicated names

se_delisted$Name %>% duplicated() %>% which()

# Check duplicated mnemonics

se_delisted$Symbol %>% duplicated() %>% which()

# All are uniquely identified by mnemonics
# In that case we do not need the ISIN.

# Add " MNEMONIC" to all names, then all will be uniquely identified. When 
# merging on names later, we could just create a new column without this ending
# granted the all names are unique.

colnames(se_delisted)[3] <- "date"

# Does any company names start with WC?

str_starts(se_delisted$Name, "WC") %>% sum()

# No. This means that we can remove all columns that start with "WC", since these 
# do not contain any data

remove_wc_col <- which(str_starts(colnames(se_delisted), "WC"))

se_delisted <- se_delisted[, -remove_wc_col]

rm(remove_wc_col)

# Make all columns numeric except "date".

# se_delisted <- se_delisted %>%
#   mutate(across(!c(Name1, Symbol, date), as.numeric))

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(se_delisted) <- str_remove_all(colnames(se_delisted), "\\.\\.\\.(.+)")



# Remove all columns that are named "#ERROR"

remove_columns_1 <- which(stringr::str_detect(colnames(se_delisted), "#ERROR"))
se_delisted <- se_delisted[, -remove_columns_1]
rm(remove_columns_1)


# Remove columns without names

# empty_columns <- colnames(se_delisted) %>% 
#   is.na() %>%
#   which()

# se_delisted <- se_delisted[, -empty_columns]
# rm(empty_columns)

# Identify which columns are price columns (except the first three columns)

# price_columns <- which(!stringr::str_detect(colnames(se_delisted), " - "))

price_columns <- which(!(str_ends(colnames(se_delisted), " - OPERATING INCOME") | 
                           str_ends(colnames(se_delisted), " - TOTAL ASSETS") |
                           str_ends(colnames(se_delisted), " - TOTAL LIABILITIES") |
                           str_ends(colnames(se_delisted), " - COMMON SHARES OUTSTANDING") |
                           str_ends(colnames(se_delisted), " - TOT RETURN IND")))
price_columns <- price_columns[-c(1:3)]


# Want to add the ending " - PRICE" to the columns indexed by price_columns

for(i in price_columns){
  # if(!(str_ends(colnames(se_delisted[i]), " - OPERATING INCOME") | 
  #      str_ends(colnames(se_delisted[i]), " - TOTAL ASSETS") |
  #      str_ends(colnames(se_delisted[i]), " - TOTAL LIABILITIES") |
  #      str_ends(colnames(se_delisted[i]), " - COMMON SHARES OUTSTANDING") |
  #      str_ends(colnames(se_delisted[i]), " - TOT RETURN IND"))){
  #   
  #   colnames(se_delisted)[i] <- paste0(colnames(se_delisted[i]), " - PRICE")
  #   
  # }
  colnames(se_delisted)[i] <- paste0(colnames(se_delisted[i]), " - PRICE")
}

rm(price_columns)
# colnames(se_delisted)[1] <- "date"


# Want to remove columns that does not have a mnemonics header (a valid mnemonic in the
# first row of the dataframe)

missing_mnemonic <- which(as.character(se_delisted[1, ]) == "NA")
missing_mnemonic <- missing_mnemonic[-1]

se_delisted <- se_delisted[, -missing_mnemonic]

rm(missing_mnemonic)

# Also want to remove all columns for which there are no data for the requested period.
# The second row contains an error message that starts with "$$"

missing_data <- which(str_detect(se_delisted[2, ] %>% unlist(), "ER"))
se_delisted <- se_delisted[, -missing_data]
rm(missing_data)


# Let's also remove any stocks that do not have six mnemonics in a row. 

mnemonics_vec <- se_delisted[1, 4:ncol(se_delisted)] %>% unlist() %>% unname()

mnemonics_vec <- mnemonics_vec %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  mutate(group_id = cur_group_id()) %>% 
  summarise(n = length(group_id)) %>% 
  filter(n < 6) %>% 
  select(value) %>% 
  rename(company = value) %>% 
  pull(company)

mnemonics_match <- se_delisted[1, 4:ncol(se_delisted)] %>% unlist()

delete_columns <- which(mnemonics_match %in% mnemonics_vec)

# Must add three to all indices since we extracted columns starting at the fourth
# place
delete_columns <- delete_columns + 3
se_delisted <- se_delisted[, -delete_columns]
rm(delete_columns)

# Want to switch all the column names before the hyphen to the corresponding one
# in the Name column. Match on mnemonic. Must first keep only the mnemonics of the
# stocks that remain in the sample. Extract the name and symbol as its own dataframe
# to do manipulation so as not to delete any dates from the dataset by removing mnemonics. 
# Also remove the name and symbol column from se_delisted and the rows exceeding
# the number of dates



df_mnemonics <- se_delisted[, 1:2]


mnemonics <- se_delisted[, 2] %>% drop_na() %>% unlist()
mnemonics_match <- se_delisted[1, 4:ncol(se_delisted)] %>% unlist()
keep_mnemonics <- which(mnemonics %in% mnemonics_match)
mnemonics <- mnemonics[keep_mnemonics]

se_delisted <- se_delisted[, -c(1,2)] 
se_delisted <- se_delisted[1:316, ]

# Must delete all rows that have a Symbol value that is not contained in mnemonics
# Cannot use filter() because we have some duplicated column names. This will be
# fixed when we match names on mnemonics. Hence, we use base R

df_mnemonics <- df_mnemonics[keep_mnemonics, ]

mnemonics_list <- split(df_mnemonics$Name, df_mnemonics$Symbol)

se_delisted_copy <- se_delisted

for(i in 1:length(mnemonics)){
  test_1 <- which(mnemonics_match == mnemonics[i])
  
  # Break in case there is no matching mnemonic
  
  if(length(test_1) == 0){
    next
  }
  # ncol_vec <- length(test_1)
  
  replacement_string <- colnames(se_delisted[test_1 + 1])
  
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
  
  colnames(se_delisted)[test_1 + 1] <- replacement_string
  
  
  # colnames(se_delisted)[test_1 + 3] <- str_replace(colnames(se_delisted)[test_1 + 3],  
  #                                                str_sub(colnames(se_delisted)[test_1 + 3], 
  #                                                        1, 
  #                                                        str_locate(colnames(se_delisted)[test_1 + 3], " -")[1]-1),
  #                                                mnemonics_list[[mnemonics[i]]])
  
}


# Next, to avoid duplication, we add all the mnemonics from the first row
# to the column names. We add it before the string indicating the column type

test_string <- colnames(se_delisted)[2]

str_detect(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))
stringr::str_extract(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))

sub(" - OPERATING INCOME", "", x = test_string)


if(str_detect(test_string, " - OPERATING INCOME")){
  paste0(sub(" - OPERATING INCOME", " test", x = test_string), " - OPERATING INCOME")
}

colnames_aux <- vector(mode = "character", length = length(colnames(se_delisted))-1)

for(i in 2:length(colnames(se_delisted))){
  
  if(str_detect(colnames(se_delisted)[i], " - OPERATING INCOME")){
    colnames_aux[i-1] <- paste0(sub(" - OPERATING INCOME",  paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - OPERATING INCOME")
  } else if((str_detect(colnames(se_delisted)[i], " - TOTAL ASSETS"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL ASSETS", paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - TOTAL ASSETS")
  } else if((str_detect(colnames(se_delisted)[i], " - TOTAL LIABILITIES"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL LIABILITIES", paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - TOTAL LIABILITIES")
  } else if((str_detect(colnames(se_delisted)[i], " - COMMON SHARES OUTSTANDING"))){
    colnames_aux[i-1] <- paste0(sub(" - COMMON SHARES OUTSTANDING", paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - COMMON SHARES OUTSTANDING")
  } else if((str_detect(colnames(se_delisted)[i], " - PRICE"))){
    colnames_aux[i-1] <- paste0(sub(" - PRICE", paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - PRICE")
  } else if((str_detect(colnames(se_delisted)[i], " - TOT RETURN IND"))){
    colnames_aux[i-1] <- paste0(sub(" - TOT RETURN IND", paste0( " ", se_delisted[1, i]), x = colnames(se_delisted)[i]), " - TOT RETURN IND")
  }
  
}

colnames(se_delisted) <- c("date", colnames_aux)
# Check for duplicated column names

duplicated_columns_last <- which(duplicated(colnames(se_delisted)))
duplicated_columns_first <- which(duplicated(colnames(se_delisted), fromLast = TRUE))

# Now we have no duplicated column names

# duplicated_first_df <- se_delisted[, duplicated_columns_first]
# duplicated_last_df <- se_delisted[, duplicated_columns_last]
# 
# cbind(duplicated_first_df, duplicated_last_df) %>% View()

# We will delete some duplicates. First, if any of them have missing data where
# the other has data, this is reason to delete it. Second, if both have data, we
# delete the stock with the smallest market value, should there be a big difference

# delete_mnemonics <- c("W:FABB", "W:KLFA")
# 
# delete_columns <- which(se_delisted[1, ] %in% delete_mnemonics)
# 
# se_delisted <- se_delisted[, -delete_columns]
# 
# se_delisted[, duplicated_columns] %>% View()
# Pivot data to long format

# Can now remove the first row containing only mnemonics and pivot the data

se_delisted <- se_delisted[-1, ]

foo <- 
  se_delisted %>% 
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

se_delisted_long <- foo

# Now we want to add a separate column with mnemonics. The easiest way to do that
# is to transform all the names in df_mnemonics so that they have the mnemonic added
# at the end
df_mnemonics_copy <- df_mnemonics

for(i in 1:nrow(df_mnemonics)){
  df_mnemonics[i, 1] <- paste0(df_mnemonics[i, 1], paste0(" ", df_mnemonics[i, 2]))
}

df_mnemonics <- df_mnemonics %>% 
  rename(company = Name,
         mnemonic = Symbol)

# Next we merge in the mnemonics column

se_delisted_long <- se_delisted_long %>% 
  left_join(df_mnemonics, by = "company")

se_delisted_long <- se_delisted_long %>% 
  select(date, mnemonic, company, op_inc, tot_assets, tot_liabilities, com_shares_outstanding,
         price, tot_ret_index)

# Next, we add the delisting date for all the relevant companies and remove all dates larger
# than the respective delisting dates

delisting_date <- delisting_date %>% 
  rename(company = Name,
         mnemonic = `...2`,
         delisting_date = `INACTIVE DATE`) %>% 
  select(company, mnemonic, delisting_date) 

for(i in 1:nrow(delisting_date)){
  delisting_date[i, 1] <- paste0(delisting_date[i, 1], paste0(" ", delisting_date[i, 2]))
}

delisting_date <- delisting_date %>% 
  select(company, delisting_date)

se_delisted_long <- se_delisted_long %>% 
  left_join(delisting_date, by = "company")

# Remove dates after delisting date. When the delisting date is available, we will base everything
# on that. If that is not available, we will base it on return observations. First we determine where
# the last change in return is, so that this may be the breaking point if we lack an explicit delisting date

se_delisted_long <- se_delisted_long %>% 
  group_by(company) %>% 
  mutate(diff_col = c(NA, diff(tot_ret_index)),
         diff = (diff_col != 0),
         cum_sum = cumsum(ifelse(is.na(diff), 0, diff)),
         cum_sum_2 = c(0, ifelse(diff(cum_sum) == 0, cum_sum + 1, cum_sum))) %>% 
  filter(if (!is.na(unique(delisting_date))) date <= unique(delisting_date)
         else cum_sum_2 < max(cum_sum_2)) %>% 
  ungroup() %>% 
  select(-c(diff_col, diff, cum_sum, cum_sum_2, delisting_date))

se_delisted_long <- se_delisted_long %>% 
  mutate(country = "sweden")

# Save file

save(se_delisted_long, file = "data/rdata/cleaned_data_for_analysis/ff-factors/sweden_delisted_cleaned.Rdata")
