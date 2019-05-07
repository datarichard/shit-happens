GetMCS <- function(dflist) {
  # This function takes the list of HILDA data frames and calculates the MCS for every viable
  # respondant in the list. It returns a data frame with xwaveid, wave, code and value.
  
  require(dplyr)
  
  sf36codes <- c('ghbp', 'ghgh', 'ghmh', 'ghpf', 'ghre', 'ghrp', 'ghsf', 'ghvt')
  sf36 <- GetRaws(dflist, sf36codes) %>%
    mutate(
      val = case_when(
        val < 0 ~ NA_integer_,
        TRUE ~ as.integer(val)
      )
    ) %>%
    spread(code, val) %>%
    na.omit() %>%
    unite(rowid, xwaveid, wave)
  
  # Convert raw scores to population-based z-scores using ABS means & SD
  sf36popz <- sf36['rowid']
  sf36popz[, 2] <- (sf36['ghpf'] - 83.46290)/23.22864
  sf36popz[, 3] <- (sf36['ghrp'] - 80.28166)/34.83783
  sf36popz[, 4] <- (sf36['ghbp'] - 76.94163)/24.83714
  sf36popz[, 5] <- (sf36['ghgh'] - 71.81575)/20.35165
  sf36popz[, 6] <- (sf36['ghvt'] - 64.47694)/19.77187
  sf36popz[, 7] <- (sf36['ghsf'] - 85.05929)/22.29047
  sf36popz[, 8] <- (sf36['ghre'] - 83.19165)/32.15215
  sf36popz[, 9] <- (sf36['ghmh'] - 75.97772)/16.96210
  sf36.matrix <- as.matrix(sf36popz[, -1])
  rownames(sf36.matrix) <- sf36popz$rowid
  
  # These are the coefficients from ABS
  MCSwABS <- c(ghpf = -0.24358,
               ghrp = -0.13410,
               ghbp = -0.12414,
               ghgh =  0.05271,
               ghvt =  0.27100,
               ghsf =  0.26460,
               ghre =  0.35922,
               ghmh =  0.48753
  )
  
  MCS <- sf36.matrix %*% MCSwABS
  
  # Transform the component score (MCS) to mean 50 and SD 10
  MCS <- (MCS * 10) + 50
  
  # Convert to data frame
  MCS %>%
    as.data.frame() %>%
    rownames_to_column('rowid') %>%
    rename(val = V1) %>%
    mutate(code = 'mcs') %>%
    separate(rowid, c('xwaveid', 'wave'), sep = '_') -> df
  
  return(df)
}
  