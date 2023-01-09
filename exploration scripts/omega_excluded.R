# Comparison of the internal consistency of BIPQ in the entire available 
# dataset and in the patients included in the analysis

  insert_head()
  
# container ------
  
  inc_omega <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## reading all available data
  ## inverting the negative terms
  
  inc_omega$raw_tbl <- read_excel('./data/ipq_sub.xlsx') %>% 
    select(- Folder) %>% 
    set_names(c('ID', paste0('ipq_q', 1:8)))
  
  for(i in paste0('ipq_q', 1:8)) {
    
    inc_omega$raw_tbl <- inc_omega$raw_tbl %>% 
      mutate(!!i := as.numeric(.data[[i]]))
    
  }
  
  for(i in c('ipq_q3', 'ipq_q4', 'ipq_q7')) {
    
    inc_omega$raw_tbl <- inc_omega$raw_tbl %>% 
      mutate(!!i := abs(10 - .data[[i]]))
    
  }
  
  inc_omega$analysis_tbl[c('all', 'included')] <- 
    list(inc_omega$raw_tbl, 
         filter(inc_omega$raw_tbl, ID %in% cov_data$clear_data$ID)) %>% 
    map(~filter(.x, complete.cases(.x)))
  
# calculation of omegas ------
  
  insert_msg('Omega calculation')
  
  inc_omega$omega_obj <- inc_omega$analysis_tbl %>% 
    map(select, starts_with('ipq_')) %>% 
    map(psych::omega, 
        nfactors = 2, 
        fm = 'ml', 
        title = 'BIPQ', 
        option = 'equal', 
        niter = 1000)
  
# Plotting the omegas -------
  
  insert_msg('Plotting the omega values')
  
  ## plotting data
  
  inc_omega$omega_summary <- inc_omega$omega_obj %>% 
    map(~.x$omega.group) %>% 
    map(mutate, 
        factor = c('total IP score', 
                   'emotion/concern/consequences', 
                   'control/coherence')) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x, subset = .y)) %>% 
    as_tibble
  
  ## plot
  
  inc_omega$omega_plot <- inc_omega$omega_summary %>% 
    ggplot(aes(x = total, 
               y = factor, 
               fill = subset)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    geom_text(aes(label = signif(total, 2)), 
              size = 2.75, 
              color = 'white', 
              hjust = 1.4, 
              position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(all = 'darkolivegreen4', 
                                 included = 'steelblue'), 
                      name = 'Analysis status') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'BIPQ coherence', 
         x = expression(omega))
  
# END ------
  
  insert_tail()