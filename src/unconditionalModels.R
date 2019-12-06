unconditionalModels <- function(dependent.df, independent.list, timelevels, events) {
  
  require(dplyr)
  require(plm)
  require(lmtest)
  require(broom)
  
  # Table to store results:
  coefs.df <- tibble()
  resid.df <- tibble()
  vcov.list <- vector(mode = 'list', length = length(events))
  names(vcov.list) <- events
  
  for (event in events) {
    
    #### Create a model dataframe ####
    independent.list[[event]] %>%
      mutate_if(is.numeric, list(~replace_na(., 0))) -> X.df
    
    # Join and scale the dependent variable
    dependent.df %>%
      inner_join(X.df, by = c('xwaveid', 'wave')) %>%
      group_by(xwaveid) %>%
      mutate(Y = scale(Y)) %>% # scaling produces NaN when no variation
      ungroup() %>%            # so replace NaN with NA
      mutate(Y = ifelse(is.nan(Y), NA, Y)) -> vars.df 
    
    vars.df %>%
      select(-wave, everything(), -xwaveid, -Y) %>%
      colnames() -> varnames
    
    formula1 <- as.formula(paste('Y ~', paste(varnames[-1], collapse =' + ')))

    # Get the Fixed Effect estimates
    fe <- plm(formula1, vars.df, index = c('xwaveid'), method = 'within')
    coefs.tb <- tidy(coeftest(fe, vcov. = vcovHC(fe, type = 'HC1')))
    
    # Add details to coefficient table
    n.tb <- tibble (n = colSums(vars.df[, 5:13]))
    n.tb$term <- colnames(vars.df[, 5:13])
    
    coefs.tb$df <- fe$df.residual
    coefs.tb %>%
      filter(term %in% timelevels) %>%
      left_join(n.tb, by = c("term")) %>%
      mutate(df = fe$df.residual,
             N = pdim(fe)$nT$n,
             #r2 = summary(fe)$r.squared[1],
             code = event) %>%
      bind_rows(coefs.df) -> coefs.df
    
    # Get the residuals
    .df <- na.omit(vars.df) # to match the dimensions
    .df$resid <- resid(fe)
    .df$pred <- predict(fe)
    .df %>%
      mutate(code = event) %>%
      select(xwaveid, wave, code, pred, resid) %>%
      bind_rows(resid.df) -> resid.df
    
    # Get the variance-covariance matrix
    # vcov.mat <- vcovHC(fe, type = "HC1")[1:9, 1:9] 
    vcov.mat <- vcovHC(fe, type = "sss")[1:9, 1:9]
    vcov.list[[event]] <- vcov.mat
    
  }

  return(list(coefs = coefs.df, resids = resid.df, covar = vcov.list))

}