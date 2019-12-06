conditionalModels <- function(dependent.df, independent.list, covariates.df, timelevels, events) {
  
  require(dplyr)
  require(plm)
  require(lmtest)
  require(broom)
  
  # Tables and lists to store results:
  coef.tb <- tibble()
  vcov.list <- vector(mode = 'list', length = length(events))
  names(vcov.list) <- events
  
  for (event in events) {
    
    #### Create a model dataframe ####
    independent.list[[event]] %>%
      mutate_if(is.numeric, list( ~replace_na(., 0))) -> X.df
    
    # Join the target event with the outcome variable and scale
    dependent.df %>%
      inner_join(X.df, by = c('xwaveid', 'wave')) %>%
      group_by(xwaveid) %>%
      mutate(Y = scale(Y)) %>% # scaling produces NaN when no variation
      ungroup() %>%            # so replace NaN with 0 and leave NA in
      mutate(Y = ifelse(is.nan(Y), NA, Y)) -> vars.df 
    
    # get other events as covariates
    covariate.events = events[which(events != event)]
    covars.df <- select(vars.df, xwaveid, wave)
    for (covar in covariate.events) {
      independent.list[[covar]] %>%
        rename_at(vars(starts_with('p')), ~ paste0(., covar)) %>%
        mutate_if(is.numeric, list(~ replace_na(., 0))) %>%
        right_join(covars.df, by = c("xwaveid", "wave")) -> covars.df
    }
    
    covars.df %>%
      mutate_if(is.numeric, list(~ replace_na(., 0))) -> covars.df
    
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
    n.tb <- tibble (n = colSums(full.df[, 5:13]))
    n.tb$term <- colnames(full.df[, 5:13])
    
    tidy(coeftest(fe, vcov. = vcovHC(fe, type = 'HC1'))) %>%
      filter(term %in% timelevels) %>%
      left_join(n.tb, by = c("term")) %>%
      mutate(df = fe$df.residual,
             N = pdim(fe)$nT$n,
             #r2 = summary(fe)$r.squared[1],
             code = event) %>%
      bind_rows(coef.tb) -> coef.tb
    
    # Get the variance-covariance matrix
    # vcov.mat <- vcovHC(fe, type = "HC1")[1:9, 1:9] 
    vcov.mat <- vcovHC(fe, type = "sss")[1:9, 1:9]
    vcov.list[[event]] <- vcov.mat
  }
  
  return(list(coefs = coef.tb, covar = vcov.list))
}