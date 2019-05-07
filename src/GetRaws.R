GetRaws <- function(df.list, var.codes) {
  
  # This function takes a list of hilda dataframes (1 for each wave) and hilda
  # variable codes, and returns a tibble in long format of all records, with the 
  # hilda code, xwaveid, waveid, and variable value. If complete == TRUE then
  # only records with valid responses in all variables are returned
  
  require(dplyr)
  
  for (df in df.list) { # for every wave
    waveid <- substr(colnames(df)[2],1,1) # get the waveid (a, b, c, ..p)
    wave.codes <- paste0(waveid, var.codes) # update the variable codes
    wave.codes <- c('xwaveid', wave.codes) # add xwaveid to track individuals
    
    col.index <- which(colnames(df) %in% wave.codes) # find the column number
    df.vars <- df[, col.index] # get the columns
    
    df.vars %<>% 
      # select(wave.codes) %>%  # this errors if no columns found
      gather(code, val, -xwaveid) %>% 
      mutate(wave = waveid) #-> df.vars
    
    if (exists('df.long')) {
      df.long <- suppressWarnings(bind_rows(df.long, df.vars))
    } else {
      df.long <- df.vars
    }
  }
  
  # remove waveid from codes
  df.long %<>%
    filter(!is.na(code)) %>%
    mutate_at('code', funs(substring(code, 2, nchar(code)))) #-> df.long
  
  # # Recode wave as year
  # df.long$wave <- apply(df.long, 1, function(x) (which(letters == x['wave']) + 2000))
  #
  
  return(df.long)
  
}