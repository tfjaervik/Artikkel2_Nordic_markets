################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)
library(zoo)

################################################################################
#' Import data
################################################################################


# Static data

# dk_active_static <- read_excel("data/copenhagen/static_copenhagen_active.xlsx")

dk_active_static <- read_excel("data/copenhagen/static_copenhagen_active_mnemonic.xlsx")
all.equal(dk_active_static$Name, dk_active_static$NAME)
dk_active_static <- dk_active_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))



# dk_delisted_static <- read_excel("data/delisted_companies/copenhagen/static_copenhagen_delisted.xlsx",
#                                  col_types = c("guess", "guess", "guess", "guess",
#                                                "date", "guess"))


dk_delisted_static <- read_excel("data/delisted_companies/copenhagen/static_copenhagen_delisted_mnemonic.xlsx",
                                 col_types = c("guess", "guess", "guess", "guess",
                                               "guess", "guess", "date", "guess"))
dk_delisted_static <- dk_delisted_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))


# se_active_static <- read_excel("data/stockholm/static_stockholm_active.xlsx")

se_active_static <- read_excel("data/stockholm/static_stockholm_active_mnemonic.xlsx")
all.equal(se_active_static$Name, se_active_static$NAME)
se_active_static <- se_active_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))

# se_delisted_static <- read_excel("data/delisted_companies/stockholm/static_stockholm_delisted.xlsx",
#                                  col_types = c("guess", "guess", "guess", "guess",
#                                                "date", "guess"))

se_delisted_static <- read_excel("data/delisted_companies/stockholm/static_stockholm_delisted_mnemonic.xlsx",
                                 col_types = c("guess", "guess", "guess", "guess",
                                              "guess", "guess", "date", "guess"))
se_delisted_static <- se_delisted_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))


# no_active_static <- read_excel("data/oslo/static_oslo_active.xlsx")

no_active_static <- read_excel("data/oslo/static_oslo_active_mnemonic.xlsx")
all.equal(no_active_static$Name, no_active_static$NAME)
no_active_static <- no_active_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))

no_delisted_static <- read_excel("data/delisted_companies/oslo/static_oslo_delisted_mnemonic.xlsx",
                                 col_types = c("guess", "guess", "guess", "guess",
                                              "guess", "guess", "date", "guess"))

no_delisted_static <- no_delisted_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))

# fn_active_static <- read_excel("data/helsinki/static_helsinki_active.xlsx")

fn_active_static <- read_excel("data/helsinki/static_helsinki_active_mnemonic.xlsx")
all.equal(fn_active_static$Name, fn_active_static$NAME)
fn_active_static <- fn_active_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))

fn_delisted_static <- read_excel("data/delisted_companies/helsinki/static_helsinki_delisted_mnemonic.xlsx",
                                 col_types = c("guess", "guess", "guess", "guess",
                                              "guess", "guess", "date", "guess"))

fn_delisted_static <- fn_delisted_static %>% 
  mutate(name = paste0(Name, " ", Symbol)) %>% 
  select(-c(Name, NAME, Symbol, Type))


# Time series data

dk_active <- read_excel("data/copenhagen/worldscope_copenhagen_active.xlsx")
dk_delisted <- read_excel("data/delisted_companies/copenhagen/worldscope_copenhagen_delisted.xlsx")

se_active <- read_excel("data/stockholm/worldscope_stockholm_active.xlsx")
se_delisted <- read_excel("data/delisted_companies/stockholm/worldscope_stockholm_delisted.xlsx")

no_active <- read_excel("data/oslo/worldscope_oslo_active.xlsx")
no_delisted <- read_excel("data/delisted_companies/oslo/worldscope_oslo_delisted.xlsx")

fn_active <- read_excel("data/helsinki/worldscope_helsinki_active.xlsx")
fn_delisted <- read_excel("data/delisted_companies/helsinki/worldscope_helsinki_delisted.xlsx")


################################################################################
#' Clean static data
################################################################################

# Denmark - combine active and delisted companies

dk_active_static <- dk_active_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
        type = `STOCK TYPE`,
        ecur = `EARNINGS CURRENCY`,
        time = `DATE/TIME`)#,
        #name = NAME) 

dk_delisted_static <- dk_delisted_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME)

dk_static <- rbind(dk_active_static, dk_delisted_static) %>% 
  mutate(country = "denmark")

# Sweden - combine active and delisted companies

se_active_static <- se_active_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME) 

se_delisted_static <- se_delisted_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME)

se_static <- rbind(se_active_static, se_delisted_static) %>% 
  mutate(country = "sweden")


# Norway - combine active and delisted companies

no_active_static <- no_active_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME) 

no_delisted_static <- no_delisted_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME)

no_static <- rbind(no_active_static, no_delisted_static) %>% 
  mutate(country = "norway")

# Finland - combine active and delisted companies

fn_active_static <- fn_active_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME) 

fn_delisted_static <- fn_delisted_static %>% 
  #select(-...1) %>% 
  rename(geo = `GEOGRAPHY GROUP`,
         type = `STOCK TYPE`,
         ecur = `EARNINGS CURRENCY`,
         time = `DATE/TIME`)#,
         #name = NAME)

fn_static <- rbind(fn_active_static, fn_delisted_static) %>% 
  mutate(country = "finland")

# Combine all countries into one dataset

nordic_static <- rbind(dk_static, se_static, no_static, fn_static)

# Remove all stocks that are not of instrument type 'EQ'.
# Remove stocks that have not been updated since before 1997.
# Remove NA

nordic_static <- nordic_static %>% 
  filter(type == "EQ") %>% 
  filter(time >= "1997-01-01") %>% 
  na.omit()


# Must remove foreign listings, i.e. remove stocks with a geographical code or currency
# other than expected for the country being screened

# Geographical codes:
# Denmark: 28
# Sweden: 35
# Norway: 34
# Finland: 45

# Currencies:
# Denmark: DK
# Sweden: SK
# Norway: NK
# Finland: E, M




nordic_static <- nordic_static %>% 
  filter(case_when(
    country == "denmark" & ecur == "DK" & geo == "28" ~ TRUE,
    country == "sweden"  & ecur == "SK" & geo == "35" ~ TRUE,
    country == "norway"  & ecur == "NK" & geo == "34" ~ TRUE,
    country == "finland" & ecur %in% c("E") & geo == "45" ~ TRUE,
    .default = FALSE
  ))

# Convert time column to class 'character'

nordic_static <- nordic_static %>% 
  mutate(time = as.character(time))

# Must also remove any places where observations are equal to "NA"

nordic_static <- nordic_static %>% 
  filter(across(.fns = ~ !. == "NA"))

# Save data file for use when screening time series data 

save(nordic_static, file = "data/rdata/cleaned_data_for_analysis/ff-factors/nordic_static.Rdata")

################################################################################
#' Clean time series data
################################################################################

# DENMARK ####

# First give all price columns a different ending

# test_string <- "AP MOELLER MAERSK - TOTAL ASSETS...3"


# str_remove(colnames(dk_active), "\\...*") # Remove all dots and everything after


# Remove all names that does not end with "- ", except the first column


remove_columns <- which(!stringr::str_detect(colnames(dk_active), " - "))
remove_columns <- remove_columns[-1]

dk_active <- dk_active[, -remove_columns]

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(dk_active) <- str_remove(colnames(dk_active), "\\...*")

# Next we have to identify and remove any duplicates. Start by identifying them. These are most likely
# related to A and B stocks

last_duplicates <- which(duplicated(colnames(dk_active)))
first_duplicates <- which(duplicated(colnames(dk_active), fromLast = TRUE))

# Next we need to compare all the duplicated columns with their original columns
# to see if they are equal

equal_duplicates <- vector(mode = "logical", length = length(last_duplicates))


for(i in 1:length(equal_duplicates)){
  equal_duplicates[i] <- all.equal(dk_active[, first_duplicates[i]], 
                                   dk_active[, last_duplicates[i]])
  
}


# All columns with the same names are duplicates, and hence can be safely removed. 


dk_active <- dk_active[, -last_duplicates]

col_names <- colnames(dk_active)

col_names[1] <- "date"

colnames(dk_active) <- col_names


# Make all columns numeric except "date".

dk_active <- dk_active %>%
  mutate(across(!date, as.numeric))



foo <- 
  dk_active %>% 
  pivot_longer(cols = -date) %>% 
  mutate(var_type = str_extract(name, "(OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING)"),
         name = str_remove(name, "- (OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING)")) %>% 
  pivot_wider(id_cols = c(date, name), names_from = var_type, values_from = value) %>% 
  transmute(date = lubridate::date(date),
            company = str_squish(name),
            op_inc = as.numeric(`OPERATING INCOME`),
            tot_assets = as.numeric(`TOTAL ASSETS`),
            tot_liabilities = as.numeric(`TOTAL LIABILITIES`),
            com_shares_outstanding = as.numeric(`COMMON SHARES OUTSTANDING`)) %>% 
  arrange(company, date)

dk_active_long <- foo

# Now we do the same with the delisted companies

dk_delisted_copy <- dk_delisted

remove_columns <- which(!stringr::str_detect(colnames(dk_delisted), " - "))
remove_columns <- remove_columns[-1]

dk_delisted <- dk_delisted[, -remove_columns]

# Remove all dots and everything after in the columns names (ex: "AP MOELLER MAERSK - OPERATING INCOME...2)

colnames(dk_delisted) <- str_remove_all(colnames(dk_delisted), "\\.\\.\\.(.+)")

# Next we have to identify and remove any duplicates. Start by identifying them. These are most likely
# related to A and B stocks

last_duplicates <- which(duplicated(colnames(dk_delisted)))
first_duplicates <- which(duplicated(colnames(dk_delisted), fromLast = TRUE))

# Next we need to compare all the duplicated columns with their original columns
# to see if they are equal

equal_duplicates <- vector(mode = "logical", length = length(last_duplicates))


for(i in 1:length(equal_duplicates)){
  equal_duplicates[i] <- all.equal(dk_delisted[, first_duplicates[i]], 
                                   dk_delisted[, last_duplicates[i]])
  
}



# All columns with the same names are duplicates, and hence can be safely removed. 


dk_delisted <- dk_delisted[, -last_duplicates]

col_names <- colnames(dk_delisted)

col_names[1] <- "date"

colnames(dk_delisted) <- col_names


# Make all columns numeric except "date".

dk_delisted <- dk_delisted %>%
  mutate(across(!date, as.numeric))



foo <- 
  dk_delisted %>% 
  pivot_longer(cols = -date) %>% 
  mutate(var_type = str_extract(name, "(OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING)"),
         name = str_remove(name, "- (OPERATING INCOME|TOTAL ASSETS|TOTAL LIABILITIES|COMMON SHARES OUTSTANDING)")) %>% 
  pivot_wider(id_cols = c(date, name), names_from = var_type, values_from = value) %>% 
  transmute(date = lubridate::date(date),
            company = str_squish(name),
            op_inc = as.numeric(`OPERATING INCOME`),
            tot_assets = as.numeric(`TOTAL ASSETS`),
            tot_liabilities = as.numeric(`TOTAL LIABILITIES`),
            com_shares_outstanding = as.numeric(`COMMON SHARES OUTSTANDING`)) %>% 
  arrange(company, date)


# End of 'Pivot data'---






