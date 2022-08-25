# Relationship between common influential factors and sub1 value

  insert_head()

# container -------

  ipq_factors <- list()
  
# globals -----

  insert_msg('Globals')
  
  ipq_factors$variables <- an$cmm_lexicon %>% 
    filter(variable %in% unique(c(an$cmm_variables$ipq_total, 
                                  an$conf_variables))) %>% 
    dlply(.(what))
  
# Serial comparisons and correlations -----
  
  insert_msg('Serial comparisons')
  
  ## descriptive stats
  
  ipq_factors$desc_stats <- ipq_factors$variables$eff_size$variable %>% 
    map(~explore(cov_data$clear_data %>% 
                   filter(!is.na(.data[[.x]])), 
                 split_factor = .x, 
                 variables = 'ipq_total', 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map2(., ipq_factors$variables$eff_size$levels, 
         ~set_names(.x, c('variable', .y))) %>% 
    set_names(ipq_factors$variables$eff_size$variable)
  
  ## comparisons
  
  ipq_factors$factor_test <- 
    list(split_factor = ipq_factors$variables$eff_size$variable, 
         types = ipq_factors$variables$eff_size$eff_type) %>% 
    pmap_dfr(compare_variables, 
             cov_data$clear_data, 
             variables = 'ipq_total', 
             what = 'eff_size', 
             ci = FALSE, 
             pub_styled = TRUE) %>% 
    mutate(split_factor = ipq_factors$variables$eff_size$variable, 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
  ## Spearman's correlations 
  
  ipq_factors$corr_test <- ipq_factors$variables$correlation$variable %>% 
    map_dfr(~correlate_variables(cov_data$clear_data[c('ipq_total', .x)] %>% 
                                   filter(complete.cases(.)), 
                                 variables = c('ipq_total', .x), 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 ci = FALSE, 
                                 pub_styled = TRUE)) %>% 
    safely(mutate)(plot_cap = paste(eff_size, significance, sep = ', '), 
                   plot_cap = stri_replace(plot_cap, 
                                           fixed = 'rho', 
                                           replacement = '\u03C1')) %>% 
    .$result
  
# Plotting the comparisons ------
  
  insert_msg('Violin plots')
  
  ipq_factors$violin_plots <- 
    map2(ipq_factors$factor_test$split_factor, 
         ipq_factors$factor_test$plot_cap, 
         ~plot_variable(cov_data$clear_data %>% 
                          filter(!is.na(.data[[.x]])), 
                        split_factor = .x, 
                        variable = 'ipq_total', 
                        type = 'violin', 
                        plot_title = translate_var(.x, out_value = 'label_long'), 
                        plot_subtitle = .y, 
                        y_lab = translate_var('ipq_total'), 
                        x_lab = translate_var(.x), 
                        cust_theme = globals$common_theme)) %>% 
    map(~.x + 
          scale_fill_brewer() + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .))) %>% 
    set_names(ipq_factors$factor_test$split_factor)
  
# Plotting the correlations -----
  
  insert_msg('Plotting the correlations')
  
  ipq_factors$corr_plots <- 
    map2(ipq_factors$corr_test$variable2, 
         ipq_factors$corr_test$plot_cap, 
         ~plot_correlation(cov_data$clear_data[c('ipq_total', .x)] %>% 
                             filter(complete.cases(.)), 
                           variables = c(.x, 'ipq_total'), 
                           type = 'correlation',
                           plot_title = translate_var(.x, out_value = 'label_long'), 
                           plot_subtitle = .y, 
                           x_lab = translate_var(.x, out_value = 'axis_lab'), 
                           y_lab = translate_var('ipq_total', out_value = 'axis_lab'), 
                           show_trend = FALSE, 
                           cust_theme = globals$common_theme)) %>% 
    map(~.x +
          geom_smooth(method = 'lm', 
                      formula = y ~ x + I(x^2)) +
          labs(tag = .x$labels$tag %>% 
                 paste0('\n', .))) %>% 
    set_names(ipq_factors$corr_test$variable2)
  
# END -----
  
  insert_tail()

