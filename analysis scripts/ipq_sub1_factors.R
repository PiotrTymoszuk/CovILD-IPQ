# Relationship between common influential factors and sub1 value 
# (subscore for emotion/concern/consequences)

  insert_head()

# container -------

  sub1_factors <- list()
  
# globals -----

  insert_msg('Globals')
  
  sub1_factors$variables <- an$cmm_lexicon %>% 
    filter(variable %in% unique(c(an$cmm_variables$ipq_sub1, 
                                  an$conf_variables))) %>% 
    dlply(.(what))

# Serial comparisons and correlations -----
  
  insert_msg('Serial comparisons')
  
  ## descriptive stats
  
  sub1_factors$desc_stats <- sub1_factors$variables$eff_size$variable %>% 
    map(~explore(cov_data$clear_data %>% 
                   filter(!is.na(.data[[.x]])), 
                 split_factor = .x, 
                 variables = 'ipq_sub1', 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map2(., sub1_factors$variables$eff_size$levels, 
         ~set_names(.x, c('variable', .y))) %>% 
    set_names(sub1_factors$variables$eff_size$variable)
  
  ## comparisons
  
  sub1_factors$factor_test <- 
    list(split_factor = sub1_factors$variables$eff_size$variable, 
         types = sub1_factors$variables$eff_size$eff_type) %>% 
    pmap_dfr(compare_variables, 
             cov_data$clear_data, 
             variables = 'ipq_sub1', 
             what = 'eff_size', 
             ci = FALSE, 
             exact = FALSE, 
             pub_styled = TRUE) %>% 
    format_test_jps %>% 
    mutate(split_factor = sub1_factors$variables$eff_size$variable, 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
  ## Spearman's correlations 
  
  sub1_factors$corr_test <- sub1_factors$variables$correlation$variable %>% 
    map_dfr(~correlate_variables(cov_data$clear_data[c('ipq_sub1', .x)] %>% 
                                   filter(complete.cases(.)), 
                                 variables = c('ipq_sub1', .x), 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 ci = FALSE, 
                                 pub_styled = TRUE)) %>% 
    format_test_jps(correlation = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Plotting the comparisons ------
  
  insert_msg('Violin plots')
  
  sub1_factors$violin_plots <- 
    map2(sub1_factors$factor_test$split_factor, 
         sub1_factors$factor_test$plot_cap, 
         ~plot_variable(cov_data$clear_data %>% 
                          filter(!is.na(.data[[.x]])), 
                        split_factor = .x, 
                        variable = 'ipq_sub1', 
                        type = 'violin', 
                        plot_title = exchange(.x,
                                              dict = globals$var_lexicon, 
                                              value = 'label_long'), 
                        plot_subtitle = .y, 
                        y_lab = exchange('ipq_sub1', 
                                         dict = globals$var_lexicon), 
                        x_lab = exchange(.x, 
                                         dict = globals$var_lexicon), 
                        point_hjitter = 0, 
                        cust_theme = globals$common_theme, 
                        x_n_labs = TRUE)) %>% 
    map(~.x + 
          scale_fill_brewer() + 
          theme(plot.tag = element_blank())) %>% 
    set_names(sub1_factors$factor_test$split_factor)
  
# Plotting the correlations -----
  
  insert_msg('Plotting the correlations')
  
  sub1_factors$corr_plots <- 
    map2(sub1_factors$corr_test$variable2, 
         sub1_factors$corr_test$plot_cap, 
         ~plot_correlation(cov_data$clear_data[c('ipq_sub1', .x)] %>% 
                             filter(complete.cases(.)), 
                           variables = c(.x, 'ipq_sub1'), 
                           type = 'correlation',
                           plot_title = exchange(.x, 
                                                 dict = globals$var_lexicon, 
                                                 value = 'label_long'), 
                           plot_subtitle = .y, 
                           x_lab = exchange(.x, 
                                            dict = globals$var_lexicon, 
                                            value = 'axis_lab'), 
                           y_lab = exchange('ipq_sub1', 
                                            dict = globals$var_lexicon, 
                                            value = 'axis_lab'), 
                           show_trend = FALSE, 
                           point_hjitter = 0, 
                           point_wjitter = 0, 
                           cust_theme = globals$common_theme)) %>% 
    map(~.x +
          geom_smooth(method = 'lm', 
                      formula = y ~ x + I(x^2)) +
          labs(subtitle = paste(.x$labels$subtitle, 
                                .x$label$tag, 
                                sep = ', ')) + 
          theme(plot.tag = element_blank())) %>% 
    set_names(sub1_factors$corr_test$variable2)
  
# END -----
  
  insert_tail()

