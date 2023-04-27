################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)

################################################################################
#' Load data
################################################################################

fi_active <- read_excel("data/worldscope_datastream/active/worldscope_helsinki_active_returns_mnemonic.xlsx",
                        col_types = c("guess", "guess", "date", rep("guess", times = 849-3)))


################################################################################
#' Clean data
################################################################################

# Check duplicated names
colnames(fi_active)[1] <- "Name"

fi_active$Name %>% duplicated() %>% which() 

# Check duplicated mnemonics

fi_active$Symbol %>% duplicated() %>% which()

# All are uniquely identified by mnemonics
# In that case we do not need the ISIN. 

# Add " MNEMONIC" to all names (later in the script), then all will be uniquely identified. When 
# merging on names later, we could just create a new column without this ending
# granted the all names are unique.

colnames(fi_active)[3] <- "date"

# Does any company names start with WC?

str_starts(fi_active$Name, "WC") %>% sum(na.rm = TRUE)

# No. This means that we can remove all columns that start with "WC", since these 
# do not contain any data

# remove_wc_col <- which(str_starts(colnames(fi_active), "WC"))
remove_wc_col <- str_starts(colnames(fi_active), "WC") # Rewrote so that we do not remove all dataframe when vector is empty

fi_active <- fi_active[, !remove_wc_col]

rm(remove_wc_col)

# Make all columns numeric except "date".

# fi_active <- fi_active %>%
#   mutate(across(!c(Name1, Symbol, date), as.numeric))

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(fi_active) <- str_remove_all(colnames(fi_active), "\\.\\.\\.(.+)")



# Remove all columns that are named "#ERROR"

remove_columns_1 <- which(stringr::str_detect(colnames(fi_active), "#ERROR"))
fi_active <- fi_active[, -remove_columns_1]
rm(remove_columns_1)


# Remove columns without names

# empty_columns <- colnames(fi_active) %>% 
#   is.na() %>%
#   which()

# fi_active <- fi_active[, -empty_columns]
# rm(empty_columns)

# Identify which columns are price columns (except the first three columns)

# price_columns <- which(!stringr::str_detect(colnames(fi_active), " - "))

price_columns <- which(!(str_ends(colnames(fi_active), " - OPERATING INCOME") | 
                           str_ends(colnames(fi_active), " - TOTAL ASSETS") |
                           str_ends(colnames(fi_active), " - TOTAL LIABILITIES") |
                           str_ends(colnames(fi_active), " - COMMON SHARES OUTSTANDING") |
                           str_ends(colnames(fi_active), " - TOT RETURN IND")))
price_columns <- price_columns[-c(1:3)]


# Want to add the ending " - PRICE" to the columns indexed by price_columns

for(i in price_columns){
  # if(!(str_ends(colnames(fi_active[i]), " - OPERATING INCOME") | 
  #      str_ends(colnames(fi_active[i]), " - TOTAL ASSETS") |
  #      str_ends(colnames(fi_active[i]), " - TOTAL LIABILITIES") |
  #      str_ends(colnames(fi_active[i]), " - COMMON SHARES OUTSTANDING") |
  #      str_ends(colnames(fi_active[i]), " - TOT RETURN IND"))){
  #   
  #   colnames(fi_active)[i] <- paste0(colnames(fi_active[i]), " - PRICE")
  #   
  # }
  colnames(fi_active)[i] <- paste0(colnames(fi_active[i]), " - PRICE")
}

rm(price_columns)
# colnames(fi_active)[1] <- "date"


# Want to remove columns that does not have a mnemonics header (a valid mnemonic in the
# first row of the dataframe)

missing_mnemonic <- which(as.character(fi_active[1, ]) == "NA")
missing_mnemonic <- missing_mnemonic[-1] # empty

# fi_active <- fi_active[, -missing_mnemonic]

rm(missing_mnemonic)

# Also want to remove all columns for which there are no data for the requested period.
# The second row contains an error message that starts with "$$"

missing_data <- str_detect(fi_active[2, ] %>% unlist(), "ER")
fi_active <- fi_active[, !missing_data]
rm(missing_data)


# Let's also remove any stocks that do not have six mnemonics in a row. 

mnemonics_vec <- fi_active[1, 4:ncol(fi_active)] %>% unlist() %>% unname()

mnemonics_vec <- mnemonics_vec %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  mutate(group_id = cur_group_id()) %>% 
  summarise(n = length(group_id)) %>% 
  filter(n < 6) %>% 
  select(value) %>% 
  rename(company = value) %>% 
  pull(company)

mnemonics_match <- fi_active[1, 4:ncol(fi_active)] %>% unlist()

delete_columns <- which(mnemonics_match %in% mnemonics_vec)

# Must add three to all indices since we extracted columns starting at the fourth
# place
delete_columns <- delete_columns + 3
fi_active <- fi_active[, -delete_columns]
rm(delete_columns)

# Want to switch all the column names before the hyphen to the corresponding one
# in the Name column. Match on mnemonic. Must first keep only the mnemonics of the
# stocks that remain in the sample. Extract the name and symbol as its own dataframe
# to do manipulation so as not to delete any dates from the dataset by removing mnemonics. 
# Also remove the name and symbol column from fi_active and the rows exceeding
# the number of dates



df_mnemonics <- fi_active[, 1:2]


mnemonics <- fi_active[, 2] %>% drop_na() %>% unlist()
mnemonics_match <- fi_active[1, 4:ncol(fi_active)] %>% unlist()
keep_mnemonics <- which(mnemonics %in% mnemonics_match)
mnemonics <- mnemonics[keep_mnemonics]

fi_active <- fi_active[, -c(1,2)] 
fi_active <- fi_active[1:316, ]

# Must delete all rows that have a Symbol value that is not contained in mnemonics
# Cannot use filter() because we have some duplicated column names. This will be
# fixed when we match names on mnemonics. Hence, we use base R

df_mnemonics <- df_mnemonics[keep_mnemonics, ]

mnemonics_list <- split(df_mnemonics$Name, df_mnemonics$Symbol)

fi_active_copy <- fi_active

# Must change colnames of 2 companies beforehand because it generates an error

# colnames(fi_active)[620:625] <- c("HEMNET GROUP AB PUB - OPERATING INCOME",
#                                   "HEMNET GROUP AB PUB - TOTAL ASSETS",
#                                   "HEMNET GROUP AB PUB - TOTAL LIABILITIES",
#                                   "HEMNET GROUP AB PUB - COMMON SHARES OUTSTANDING",
#                                   "HEMNET GROUP AB PUB - PRICE",
#                                   "HEMNET GROUP AB PUB - TOT RETURN IND")
# 
# colnames(fi_active)[1946:1951] <- c("WESC AB - OPERATING INCOME",
#                                     "WESC AB - TOTAL ASSETS",
#                                     "WESC AB - TOTAL LIABILITIES",
#                                     "WESC AB - COMMON SHARES OUTSTANDING",
#                                     "WESC AB - PRICE",
#                                     "WESC AB - TOT RETURN IND")

# Want to remove all parenthesis in the names, as they are causing errors
colnames(fi_active) <- gsub("\\s*\\([^\\)]+\\)","",as.character(colnames(fi_active))) 

for(i in 1:length(mnemonics)){
  test_1 <- which(mnemonics_match == mnemonics[i])
  
  # Break in case there is no matching mnemonic
  
  if(length(test_1) == 0){
    next
  }
  # ncol_vec <- length(test_1)
  
  replacement_string <- colnames(fi_active[test_1 + 1])
  
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
  
  colnames(fi_active)[test_1 + 1] <- replacement_string
  
  
  # colnames(fi_active)[test_1 + 3] <- str_replace(colnames(fi_active)[test_1 + 3],  
  #                                                str_sub(colnames(fi_active)[test_1 + 3], 
  #                                                        1, 
  #                                                        str_locate(colnames(fi_active)[test_1 + 3], " -")[1]-1),
  #                                                mnemonics_list[[mnemonics[i]]])
  
}


# Next, to avoid duplication, we add all the mnemonics from the first row
# to the column names. We add it before the string indicating the column type

test_string <- colnames(fi_active)[2]

str_detect(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))
stringr::str_extract(test_string, c(" - OPERATING INCOME", " - TOT RETURN IND"))

sub(" - OPERATING INCOME", "", x = test_string)


if(str_detect(test_string, " - OPERATING INCOME")){
  paste0(sub(" - OPERATING INCOME", " test", x = test_string), " - OPERATING INCOME")
}

colnames_aux <- vector(mode = "character", length = length(colnames(fi_active))-1)

for(i in 2:length(colnames(fi_active))){
  
  if(str_detect(colnames(fi_active)[i], " - OPERATING INCOME")){
    colnames_aux[i-1] <- paste0(sub(" - OPERATING INCOME",  paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - OPERATING INCOME")
  } else if((str_detect(colnames(fi_active)[i], " - TOTAL ASSETS"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL ASSETS", paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - TOTAL ASSETS")
  } else if((str_detect(colnames(fi_active)[i], " - TOTAL LIABILITIES"))){
    colnames_aux[i-1] <- paste0(sub(" - TOTAL LIABILITIES", paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - TOTAL LIABILITIES")
  } else if((str_detect(colnames(fi_active)[i], " - COMMON SHARES OUTSTANDING"))){
    colnames_aux[i-1] <- paste0(sub(" - COMMON SHARES OUTSTANDING", paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - COMMON SHARES OUTSTANDING")
  } else if((str_detect(colnames(fi_active)[i], " - PRICE"))){
    colnames_aux[i-1] <- paste0(sub(" - PRICE", paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - PRICE")
  } else if((str_detect(colnames(fi_active)[i], " - TOT RETURN IND"))){
    colnames_aux[i-1] <- paste0(sub(" - TOT RETURN IND", paste0( " ", fi_active[1, i]), x = colnames(fi_active)[i]), " - TOT RETURN IND")
  }
  
}

colnames(fi_active) <- c("date", colnames_aux)
# Check for duplicated column names

duplicated_columns_last <- which(duplicated(colnames(fi_active)))
duplicated_columns_first <- which(duplicated(colnames(fi_active), fromLast = TRUE))

# Now we have no duplicated column names

# duplicated_first_df <- fi_active[, duplicated_columns_first]
# duplicated_last_df <- fi_active[, duplicated_columns_last]
# 
# cbind(duplicated_first_df, duplicated_last_df) %>% View()

# We will delete some duplicates. First, if any of them have missing data where
# the other has data, this is reason to delete it. Second, if both have data, we
# delete the stock with the smallest market value, should there be a big difference

# delete_mnemonics <- c("W:FABB", "W:KLFA")
# 
# delete_columns <- which(fi_active[1, ] %in% delete_mnemonics)
# 
# fi_active <- fi_active[, -delete_columns]
# 
# fi_active[, duplicated_columns] %>% View()
# Pivot data to long format

# Can now remove the first row containing only mnemonics and pivot the data

fi_active <- fi_active[-1, ]


foo <- 
  fi_active %>% 
  pivot_longer(cols = -date) %>% 
  mutate(var_type = str_extract(name, "(OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING|PRICE|TOT RETURN IND)"),
         name = str_remove(name, "- (OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING|PRICE|TOT RETURN IND)")) %>% 
  # group_by(name) %>% 
  # mutate(row = row_number()) %>%  # Create unique identifier to be able to pivot wider
  pivot_wider(id_cols = c(date, name), names_from = var_type, values_from = value,
              values_fn = list) %>% # Added this because of duplicates I do not understand, then unnest later
  unnest(cols = everything()) %>% 
  transmute(date = lubridate::date(date),
            company = str_squish(name),
            op_inc = as.numeric(`OPERATING INCOME`),
            tot_assets = as.numeric(`TOTAL ASSETS`),
            tot_liabilities = as.numeric(`TOTAL LIABILITIES`),
            com_shares_outstanding = as.numeric(`COMMON SHARES OUTSTANDING`),
            price = as.numeric(`PRICE`),
            tot_ret_index = as.numeric(`TOT RETURN IND`)) %>% 
  arrange(company, date)

fi_active_long <- foo

# Now we want to add a separate column with mnemonics. The easiest way to do that
# is to transform all the names in df_mnemonics so that they have the mnemonic added
# at the end
# df_mnemonics_copy <- df_mnemonics
# 
# df_mnemonics$company <- gsub("\\s*\\([^\\)]+\\)","",as.character(df_mnemonics$company)) 
# 
# for(i in 1:nrow(df_mnemonics)){
#   df_mnemonics[i, 1] <- paste0(df_mnemonics[i, 1], paste0(" ", df_mnemonics[i, 2]))
# }
# 
# df_mnemonics <- df_mnemonics %>% 
#   rename(company = Name,
#          mnemonic = Symbol)
# 
# # Next we merge in the mnemonics column
# 
# fi_active_long <- fi_active_long %>% 
#   left_join(df_mnemonics, by = "company")


# We will add the mnemonic by creating a separate column where we insert the last
# word of the company string

fi_active_long <- fi_active_long %>% 
  group_by(company) %>%
  mutate(mnemonic = stringi::stri_extract_last_words(unique(company)))

fi_active_long <- fi_active_long %>% 
  select(date, mnemonic, company, op_inc, tot_assets, tot_liabilities, com_shares_outstanding,
         price, tot_ret_index)

# Add a country column

fi_active_long <- fi_active_long %>% 
  mutate(country = "finland")

# Save file

save(fi_active_long, file = "data/rdata/cleaned_data_for_analysis/ff-factors/finland_active_cleaned.Rdata")



