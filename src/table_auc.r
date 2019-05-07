table_auc <- function(covar.list, coef.tb) {
  event_codes = unique(coef.tb$code)
  timelevels = c('pre36', 'pre24', 'pre12', 'post00', 'post03', 'post06', 
                 'post09', 'post12', 'post24', 'post36')
  t.delta = c(12, 12, 3, 3, 3, 3, 12, 12, 12)
  
  auc <- vector(mode = "numeric", length = length(event_codes))
  auc.var <- vector(mode = "numeric", length = length(event_codes))
  auc.n <- vector()
  names(auc) <- event_codes
  names(auc.var) <- event_codes
  auc.table <- tibble()
  
  for (dv in names(covar.list)) {
    for (event in event_codes) {
      
      beta.coef <- coef.tb %>%
        filter(code == event & outcome == dv, term %in% timelevels) %>%
        select(term, estimate, outcome)
      
      beta.vcov <- covar.list[[dv]][[event]]
      
      # auc[event] = sum(abs(beta.coef$estimate)*t.delta)
      auc[event] = sum(beta.coef$estimate*t.delta)
      auc.var[event] <- as.numeric(t(t.delta) %*% beta.vcov %*% t.delta)
      auc.n[event] <- unique(coef.tb[coef.tb$code == event & coef.tb$outcome == dv, "N"])
    }
    
    bind_rows(auc, auc.var, auc.n) %>%
      mutate(x = c("auc", "variance", "n")) %>%
      gather(code, val, -x) %>%
      spread(x, val) -> tb
    
    tb$outcome = dv
    auc.table <- bind_rows(auc.table, tb)
    
  }
  
  # auc.table <- mutate(auc.table, code = factor(code, levels = event_codes, ordered = TRUE)) 
  # levels(auc.table$code) <- renamevents
  return(auc.table)
}