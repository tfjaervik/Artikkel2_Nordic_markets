################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)

################################################################################
#' Load data
################################################################################

dk_active <- read_excel("data/worldscope_datastream/active/worldscope_copenhagen_active_returns_mnemonic.xlsx")


################################################################################
#' Clean data
################################################################################

# Check duplicated names
colnames(dk_active)[1] <- "Name"

dk_active$Name %>% duplicated() %>% which() #only NA values are duplicated

# Check duplicated mnemonics

dk_active$Symbol %>% duplicated() %>% which()

# All are uniquely identified by mnemonics
# In that case we do not need the ISIN. (all duplicates are just NA values)

# Add " MNEMONIC" to all names (later in the script), then all will be uniquely identified. When 
# merging on names later, we could just create a new column without this ending
# granted the all names are unique.

colnames(dk_active)[3] <- "date"

# Does any company names start with WC?

str_starts(dk_active$Name, "WC") %>% sum(na.rm = TRUE)

# No. This means that we can remove all columns that start with "WC", since these 
# do not contain any data

# remove_wc_col <- which(str_starts(colnames(dk_active), "WC"))
remove_wc_col <- str_starts(colnames(dk_active), "WC") # Rewrote so that we do not remove all dataframe when vector is empty

dk_active <- dk_active[, !remove_wc_col]

rm(remove_wc_col)

# Make all columns numeric except "date".

# dk_active <- dk_active %>%
#   mutate(across(!c(Name1, Symbol, date), as.numeric))

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(dk_active) <- str_remove_all(colnames(dk_active), "\\.\\.\\.(.+)")



# Remove all columns that are named "#ERROR"

remove_columns_1 <- which(stringr::str_detect(colnames(dk_active), "#ERROR"))
dk_active <- dk_active[, -remove_columns_1]
rm(remove_columns_1)


# Remove columns without names

# empty_columns <- colnames(dk_active) %>% 
#   is.na() %>%
#   which()

# dk_active <- dk_active[, -empty_columns]
# rm(empty_columns)

# Identify which columns are price columns (except the first three columns)

# price_columns <- which(!stringr::str_detect(colnames(dk_active), " - "))

price_columns <- which(!(str_ends(colnames(dk_active), " - OPERATING INCOME") | 
                           str_ends(colnames(dk_active), " - TOTAL ASSETS") |
                           str_ends(colnames(dk_active), " - TOTAL LIABILITIES") |
                           str_ends(colnames(dk_active), " - COMMON SHARES OUTSTANDING") |
                           str_ends(colnames(dk_active), " - TOT RETURN IND")))
price_columns <- price_columns[-c(1:3)]


# Want to add the ending " - PRICE" to the columns indexed by price_columns

for(i in price_columns){
  # if(!(str_ends(colnames(dk_active[i]), " - OPERATING INCOME") | 
  #      str_ends(colnames(dk_active[i]), " - TOTAL ASSETS") |
  #      str_ends(colnames(dk_active[i]), " - TOTAL LIABILITIES") |
  #      str_ends(colnames(dk_active[i]), " - COMMON SHARES OUTSTANDING") |
  #      str_ends(colnames(dk_active[i]), " - TOT RETURN IND"))){
  #   
  #   colnames(dk_active)[i] <- paste0(colnames(dk_active[i]), " - PRICE")
  #   
  # }
  colnames(dk_active)[i] <- paste0(colnames(dk_active[i]), " - PRICE")
}

rm(price_columns)
# colnames(dk_active)[1] <- "date"


# Want to remove columns that does not have a mnemonics header (a valid mnemonic in the
# first row of the dataframe)

missing_mnemonic <- which(as.character(dk_active[1, ]) == "NA")
missing_mnemonic <- missing_mnemonic[-1] # empty

# dk_active <- dk_active[, -missing_mnemonic]

rm(missing_mnemonic)

# Also want to remove all columns for which there are no data for the requested period.
# The second row contains an error message that starts with "$$"

missing_data <- str_detect(dk_active[2, ] %>% unlist(), "ER")
dk_active <- dk_active[, !missing_data]
rm(missing_data)


# Let's also remove any stocks that do not have six mnemonics in a row. 

mnemonics_vec <- dk_active[1, 4:ncol(dk_active)] %>% unlist() %>% unname()

mnemonics_vec <- mnemonics_vec %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  mutate(group_id = cur_group_id()) %>% 
  summarise(n = length(group_id)) %>% 
  filter(n < 6) %>% 
  select(value) %>% 
  rename(company = value) %>% 
  pull(company)

mnemonics_match <- dk_active[1, 4:ncol(dk_active)] %>% unlist()

delete_columns <- which(mnemonics_match %in% mnemonics_vec)

# Must add three to all indices since we extracted columns starting at the fourth
# place
delete_columns <- delete_columns + 3
dk_active <- dk_active[, -delete_columns]
rm(delete_columns)

# Want to switch all the column names before the hyphen to the corresponding one
# in the Name column. Match on mnemonic. Must first keep only the mnemonics of the
# stocks that remain in the sample. Extract the name and symbol as its own dataframe
# to do manipulation so as not to delete any dates from the dataset by removing mnemonics. 
# Also remove the name and symbol column from dk_active and the rows exceeding
# the number of dates



df_mnemonics <- dk_active[, 1:2]


mnemonics <- dk_active[, 2] %>% drop_na() %>% unlist()
mnemonics_match <- dk_active[1, 4:ncol(dk_active)] %>% unlist()
keep_mnemonics <- which(mnemonics %in% mnemonics_match)
mnemonics <- mnemonics[keep_mnemonics]

dk_active <- dk_active[, -c(1,2)] 
dk_active <- dk_active[1:316, ]

# Must delete all rows that have a Symbol value that is not contained in mnemonics
# Cannot use filter() because we have some duplicated column names. This will be
# fixed when we match names on mnemonics. Hence, we use base R

df_mnemonics <- df_mnemonics[keep_mnemonics, ]

mnemonics_list <- split(df_mnemonics$Name, df_mnemonics$Symbol)

dk_active_copy <- dk_active

for(i in 1:length(mnemonics)){
  test_1 <- which(mnemonics_match == mnemonics[i])
  
  # Break in case there is no matching mnemonic
  
  if(length(test_1) == 0){
    next
  }
  # ncol_vec <- length(test_1)
  
  replacement_string <- colnames(dk_active[test_1 + 1])
  
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
  
  colnames(dk_active)[test_1 + 1] <- replacement_string
  
  
  # colnames(dk_active)[test_1 + 3] <- str_replace(colnames(dk_active)[test_1 + 3],  
  #                                                str_sub(colnames(dk_active)[test_1 + 3], 
  #                                                        1, 
  #                                                        str_locate(colnames(dk_active)[test_1 + 3], " -")[1]-1),
  #                                                mnemonics_list[[mnemonics[i]]])
  
}


# Next, to avoid duplication, we add all the mnemonics from the first row
# to the column names. We add it before the string indicating the column type

test_string <- colnames(dk_active)[2]

str_detect(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))
stringr::str_extract(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))

sub(" - OPERATING INCOME", "", x = test_string)


if(str_detect(test_string, " - OPERATING INCOME")){
  paste0(sub(" - OPERATING INCOME", " test", x = test_string), " - OPERATING INCOME")
}

colnames_aux <- vector(mode = "character", length = length(colnames(dk_active))-1)

for(i in 2:length(colnames(dk_active))){
  
  if(str_detect(colnames(dk_active)[i], " - OPERATING INCOME")){
    colnames_aux[i-1] <- paste0(sub(" - OPERATING INCOME",  paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - OPERATING INCOME")
  } else if((str_detect(colnames(dk_active)[i], " - TOTAL ASSETS"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL ASSETS", paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - TOTAL ASSETS")
  } else if((str_detect(colnames(dk_active)[i], " - TOTAL LIABILITIES"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL LIABILITIES", paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - TOTAL LIABILITIES")
  } else if((str_detect(colnames(dk_active)[i], " - COMMON SHARES OUTSTANDING"))){
    colnames_aux[i-1] <- paste0(sub(" - COMMON SHARES OUTSTANDING", paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - COMMON SHARES OUTSTANDING")
  } else if((str_detect(colnames(dk_active)[i], " - PRICE"))){
    colnames_aux[i-1] <- paste0(sub(" - PRICE", paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - PRICE")
  } else if((str_detect(colnames(dk_active)[i], " - TOT RETURN IND"))){
    colnames_aux[i-1] <- paste0(sub(" - TOT RETURN IND", paste0( " ", dk_active[1, i]), x = colnames(dk_active)[i]), " - TOT RETURN IND")
  }
  
}

colnames(dk_active) <- c("date", colnames_aux)
# Check for duplicated column names

duplicated_columns_last <- which(duplicated(colnames(dk_active)))
duplicated_columns_first <- which(duplicated(colnames(dk_active), fromLast = TRUE))

# Now we have no duplicated column names

# duplicated_first_df <- dk_active[, duplicated_columns_first]
# duplicated_last_df <- dk_active[, duplicated_columns_last]
# 
# cbind(duplicated_first_df, duplicated_last_df) %>% View()

# We will delete some duplicates. First, if any of them have missing data where
# the other has data, this is reason to delete it. Second, if both have data, we
# delete the stock with the smallest market value, should there be a big difference

# delete_mnemonics <- c("W:FABB", "W:KLFA")
# 
# delete_columns <- which(dk_active[1, ] %in% delete_mnemonics)
# 
# dk_active <- dk_active[, -delete_columns]
# 
# dk_active[, duplicated_columns] %>% View()
# Pivot data to long format

# Can now remove the first row containing only mnemonics and pivot the data

dk_active <- dk_active[-1, ]

foo <- 
  dk_active %>% 
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

dk_active_long <- foo

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

dk_active_long <- dk_active_long %>% 
  left_join(df_mnemonics, by = "company")

dk_active_long <- dk_active_long %>% 
  select(date, mnemonic, company, op_inc, tot_assets, tot_liabilities, com_shares_outstanding,
         price, tot_ret_index)

# Add a country column

dk_active_long <- dk_active_long %>% 
  mutate(country = "denmark")

# Save file

save(dk_active_long, file = "data/rdata/cleaned_data_for_analysis/ff-factors/denmark_active_cleaned.Rdata")



