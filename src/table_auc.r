table_auc <- function(covar.list, coef.tb) {
  
  #### Calculate AUC ####
  event_codes = unique(coef.tb$code)
  timelevels = c('pre24', 'pre12', 'post03', 'post06', 
                 'post09', 'post12', 'post24', 'post36', 'post48')
  t.delta = c(12, 12, 3, 3, 3, 3, 12, 12, 12)
  
  auc <- vector(mode = "numeric", length = length(event_codes))
  auc.var <- vector(mode = "numeric", length = length(event_codes))
  auc.df <- vector()
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
      auc.df[event] <- unique(coef.tb[coef.tb$code == event & coef.tb$outcome == dv, "N"])
    }
    
    bind_rows(auc, auc.var, auc.df) %>%
      mutate(x = c("auc", "variance", "N")) %>%
      gather(code, val, -x) %>%
      spread(x, val) -> tb
    
    tb$outcome = dv
    auc.table <- bind_rows(auc.table, tb)
    
  }
  
  #### Add p-values ####
  auc.table %>%
    filter(outcome == "cognitive") %>%
    select(code, auc) %>%
    deframe() -> coef_cognitive
  
  auc.table %>%
    filter(outcome == "affective") %>%
    select(code, auc) %>%
    deframe() -> coef_affective
  
  auc.table %>%
    filter(outcome == "cognitive") %>%
    select(code, variance) %>%
    deframe() -> var_cognitive
  
  auc.table %>%
    filter(outcome == "affective") %>%
    select(code, variance) %>%
    deframe() -> var_affective
  
  .num = coef_cognitive - coef_affective
  .den = sqrt(var_cognitive + var_affective)
  
  z_score = .num / .den
  p_2tail = pnorm(abs(z_score), lower.tail = FALSE)*2
  
  auc.table %<>%
    left_join(
      enframe(z_score, name = "code", value = "z_score"), by = "code"
      ) %>%
    left_join(
      enframe(p_2tail, name = "code", value = "p_2tail"), by = "code"
      ) -> auc.table
  
  return(auc.table)
}