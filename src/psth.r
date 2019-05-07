psth <- function(tb, eventnames, title_str) {
  
  xlabels = c(
    "pre24" = "-24", "pre12" = "-12", "post03" = "+3", 
    "post06" = "+6", "post09" = "+9", "post12" = "+12", 
    "post24" = "+24", "post36" = "+36", "post48" = "+48"
  )
  
  tb %>%
    filter(code %in% eventnames) %>%
    group_by(code) %>%
    summarise(N_min = min(n)) %>%
    mutate(
      note = paste('smallest n =', N_min),
      outcome = 'min'
    ) -> annotates

  # tb %>%
  #   mutate(n = ifelse(n > 1000, 1000, n)) -> tb
  
  tb %>%
    filter(code %in% eventnames) %>%
    mutate(
      outcome = recode(outcome, mcs = 'Affective', losat = 'Cognitive'),
      term = factor(term, levels = timelevels, ordered = TRUE),
      code = fct_relevel(code, eventnames)) %>%
    ggplot(aes(x = term, y = estimate, group = outcome, colour = outcome)) +
    geom_point() + 
    geom_line() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.25) +
    geom_vline(xintercept = 2.85, alpha = 0.25) +
    scale_x_discrete(labels = xlabels) +
    facet_wrap(~ code) +
    labs(title = title_str,
         subtitle = 'FE coefficients (sd units) ±90 percent confidence intervals',
         y = '',
         x = 'months pre and post life event') +
    geom_text(data = annotates, aes(x = Inf, y = -Inf, label = note),
              hjust = 1.1, vjust = -.75, colour = 1, size = 3) +
    theme_light()
}