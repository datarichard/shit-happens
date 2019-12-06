library(tidyverse)
library(haven)

setwd("/Users/rich/Dropbox/HILDA/shit-happens-paper")

#### Helper functions ####
source('src/GetRaws.R')
source('src/GetMCS.R')
# source('src/GetEventTimes.R')

GetEventTimes <- function(eventcodes, datalist) {
  
  df.list <- list()
  
  for (eventcode in eventcodes) {
    #### Recode data so event = 1, no event = 0, missing = NA ####
    #
    codeq <- paste0(eventcode, 'q', seq(4)) # get the quarterly codes
    #
    # Creating the event data frame...
    GetRaws(datalist, c(eventcode, codeq)) %>% 
      spread(key = code, value = val) %>% 
      # missing values replaced with NA
      mutate_at(eventcode, funs(case_when(. < 0 ~ NA_real_, 
                                          TRUE ~ as.numeric(.)))) %>%
      mutate_at(eventcode, funs(. - 1)) %>%
      mutate_at(codeq, funs(case_when(. < 0 ~ NA_real_,
                                      TRUE ~ as.numeric(.)))) -> df
    
    # rename our column variables to make selection easier
    colnames(df) <- c("xwaveid", "wave", "annual", "post03", 
                      "post06", "post09", "post12")
    
    # set the column order to allow column indexing below
    colorder <- c('xwaveid', 'wave', 'pre36', 'pre24', 'pre12', 'post03', 
                  'post06', 'post09', 'post12', 'post24', 'post36', 'post48')
    
    # Add lead and lag variables based on annual event record
    # pre36 = 2-3 years before event
    # pre24 = 1-2 years before event
    # pre12 = 0-1 years before event
    # post03 = 0-3 months after event
    # post06 = 3-6 months after event
    # post09 = 6-9 months after event
    # post12 = 9-12 months after event
    # post24 = 1-2 years after event
    # post36 = 2-3 years after event
    # post48 = 3-4 years after event
    
    df %>%  
      group_by(xwaveid) %>%
      # The annual event record is used to create yearly leads and lags (pre12 
      # to post36). If the annual event record is missing (even if quarterly 
      # event records are present), then leads and lags will be missing
      mutate( 
        pre12  = dplyr::lead(annual, n = 1, order_by = wave),
        pre24  = dplyr::lead(annual, n = 2, order_by = wave),
        pre36  = dplyr::lead(annual, n = 3, order_by = wave),
        post24 = dplyr::lag(annual, n = 1, order_by = wave),
        post36 = dplyr::lag(annual, n = 2, order_by = wave),
        post48 = dplyr::lag(annual, n = 3, order_by = wave)
      ) %>%
      ungroup() %>%
      select(-annual) %>%
      select(one_of(colorder)) %>%
      mutate(rowsums = rowSums(.[3:12], na.rm = TRUE)) %>%
      filter(rowsums > 0) %>%
      select(-rowsums) -> df
    
    df.list[eventcode] <- list(df)
    
  }
  
  return(df.list)
}


#### Load data ####
filepaths <- list.files(
  path = '~/Dropbox/HILDA/data',
  pattern = '^Combined.*.dta$',
  full.names = TRUE
)

hilda <- list()
for (pathtofile in filepaths) {
  df <- read_dta(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}

#### Get Variables ####
# Get the life events
lifevents <- c('lefnw', 'lefni', 'lefrd', 'lejob', 'leprm', 'lertr', 'lemar',
               'leprg', 'lebth', 'ledsc', 'lesep', 'lemvd', 'leins', 'lepcm',
               'lejls', 'lejlf', 'ledfr', 'ledhm', 'ledrl', 'leinf', 'lercl', 'levio')

events_by_time <- GetEventTimes(lifevents, hilda)
write_rds(events_by_time, 'data/events_by_time.rds')

for (df in events_by_time) {
  print(dim(df))
}
# Store the events to use as covariates
# To be done


# Get outcome variables
mcs <- GetMCS(hilda)

GetRaws(hilda, c(
  'gh9a', # Vitality: feel full of life (lower is better)*
  'gh9b', # Mental H: Been a nervous person (higher is better)
  'gh9c', # Mental H: Felt so down in the dumps (higher is better)
  'gh9d', # Mental H: Felt calm and peaceful (lower is better)*
  'gh9e', # Vitality: Have a lot of energy (lower is better)*
  'gh9f', # Mental H: Felt down (higher is better)
  'gh9g', # Vitality: Felt worn out (higher is better)
  'gh9h', # Mental H: Been happy (lower is better)*
  'gh9i'  # Vitality: Felt tired (higher is better)
  )
) -> gh9

reversed_items <- c('gh9a', 'gh9d', 'gh9e', 'gh9h')

gh9 %>%
  # Recode missing to NA
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  # Reverse score
  mutate_at(reversed_items, ~ 7 - .) -> gh9_items

write_rds(gh9_items, 'data/gh9_items.rds')

gh9_items %>%
  gather(code, val, -xwaveid, -wave) %>%
  # Impute average if less than half missing (see ghmh data dictionary)
  group_by(xwaveid, wave) %>%
  mutate(
    sum_na = sum(is.na(val)),
    mean_na = mean(val, na.rm = TRUE),
    imputed = ifelse(is.na(val) & sum_na < 5, mean_na, val),
    sum_imputed = sum(imputed),
    code = "gh9_sum"
  ) %>% 
  select(xwaveid, wave, code, val = sum_imputed) %>%
  distinct() %>%
  ungroup() -> gh9_imputed

GetRaws(hilda, c('losat', 'ghmh')) %>%
  bind_rows(mcs) %>%
  bind_rows(gh9_imputed) -> outcomes

write_rds(outcomes, 'data/outcomes.rds')

#### Get demographic covariates ####
GetRaws(hilda, c('hgage', 'hhda10', 'edhigh1')) -> covariates

GetRaws(hilda, 'hgsex') %>%
  group_by(xwaveid) %>%
  summarise(sex = round(mean(val))) %>%
  mutate(val = sex - 1,
         code = 'female') %>%
  select(xwaveid, code, val) %>%
  bind_rows(covariates) %>%
  arrange(xwaveid, code, wave) -> demographics

write_rds(demographics, 'data/demographics.rds')
