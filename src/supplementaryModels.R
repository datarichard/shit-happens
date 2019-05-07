supplementaryModels <- function(dependent.df, independent.list, covariates.df, clean = FALSE, balance = FALSE) {
  
  require(dplyr)
  require(plm)
  require(lmtest)
  require(broom)
  
  events <- c('lefnw', 'lefni', 'lefrd', 'lejob', 'leprm', 'lertr', 'lemar',
              'leprg', 'lebth', 'ledsc', 'lesep', 'lemvd', 'leins', 'lepcm',
              'lejls', 'lejlf', 'ledfr', 'ledhm', 'ledrl', 'leinf', 'lercl', 
              'levio')
  
  # Table to store results:
  results <- data_frame()
  
  for (event in events) {
    
    #### Create a model dataframe ####
    if (clean) {
      independent.list[[event]] %>%
        replace_na(list("post00" = 0, 
                        "post03" = 0, 
                        "post06" = 0, 
                        "post09" = 0)) %>%
        na.omit() -> X.df
    } else {
      independent.list[[event]] %>%
        mutate_if(is.numeric, funs(replace_na(., 0))) -> X.df
    }
    
    if (balance) {
      X.df %>%
        mutate(pre = rowSums(.[3:5], na.rm = TRUE),
               post = rowSums(.[6:12], na.rm = TRUE)) %>%
        group_by(xwaveid) %>%
        mutate(presum = sum(pre),
               postsum = sum(post)) %>% 
        filter(presum > 1 & postsum > 1) %>% 
        select(-pre, -post, -presum, -postsum) -> X.df
    }
    
    # Join the target event with the outcome variable and scale
    dependent.df %>% 
      inner_join(X.df, by = c('xwaveid', 'wave')) %>%
      group_by(xwaveid) %>%
      mutate(Y = scale(Y)) %>% # scaling produces NA when no variation
      replace_na(list("Y" = 0)) %>% 
      ungroup() -> vars.df 
    
    # get other events as covariates
    covariate.events = events[which(events != event)]
    covars.df <- select(vars.df, xwaveid, wave)
    for (covar in covariate.events) {
      independent.list[[covar]] %>%
        rename_at(vars(starts_with('p')), ~ paste0(., covar)) %>%
        mutate_if(is.numeric, funs(replace_na(., 0))) %>%
        right_join(covars.df, by = c("xwaveid", "wave")) -> covars.df
    }
    
    covars.df %>%
      mutate_if(is.numeric, funs(replace_na(., 0))) -> covars.df
    
    # join target variables with covariates
    vars.df %>%
      left_join(covars.df, by = c("xwaveid", "wave")) %>%
      left_join(covariates.df, by = c("xwaveid", "wave")) -> full.df
      
    # test for missing covariates
    # full.df %>%
    #   select(xwaveid, edu, seifa) %>%
    #   group_by(xwaveid) %>%
    #   summarise_all(funs(sum(is.na(.)))) %>%
    #   filter(edu > 0 | seifa > 0) -> nacount
    # 
    # if (nrow(nacount) > 0) {
    #   # browser()
    #   message(unique(nacount$xwaveid))
    #   }
    
    # Build the model
    full.df %>%
      ungroup() %>%
      select(-Y, -xwaveid) %>%
      select(-wave, everything()) %>% 
      colnames() -> var.names 
    
    # Formula and model
    full.formula <- as.formula(paste('Y ~', paste(var.names[-1], collapse = ' + ')))
    fe <- plm(full.formula, full.df, index = c('xwaveid'), method = 'within')
    
    # Add details to coefficient table
    n.tb <- tibble(n = colSums(full.df[, 5:13]))
    n.tb$term <- colnames(full.df[, 5:13])
    
    tidy(coeftest(fe, vcov. = vcovHC(fe, type = 'HC1'))) %>%
      filter(term %in% timelevels) %>%
      left_join(n.tb, by = c("term")) %>%
      mutate(df = fe$df.residual,
             N = pdim(fe)$nT$n,
             #r2 = summary(fe)$r.squared[1],
             code = event) %>%
      bind_rows(results) -> results
  }
  
  return(results)
}