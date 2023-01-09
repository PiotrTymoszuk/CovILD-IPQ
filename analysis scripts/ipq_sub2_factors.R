# Relationship between common influential factors and sub2 value 
# (control/coherence)
#
# Note: there were no factor identified by modeling!

  insert_head()

# container -------

  sub2_factors <- list()
  
# globals -----

  insert_msg('Globals')
  
  sub2_factors$variables <- an$cmm_lexicon %>% 
    filter(variable %in% unique(c(an$cmm_variables$ipq_sub2, 
                                  an$conf_variables))) %>% 
    dlply(.(what))
  
  ## n numbers to be presented in the X axis
  
  sub2_factors$n_numbers <- sub2_factors$variables$eff_size$variable %>% 
    map(~count(mod$multi_tbl, .data[[.x]])) %>% 
    set_names(sub2_factors$variables$eff_size$variable)
  
  sub2_factors$x_axes <- sub2_factors$n_numbers %>% 
    map(~map2_chr(.x[[1]], .x[[2]], paste, sep = '\nn = '))
  
# Serial comparisons and correlations -----
  
  insert_msg('Serial comparisons')
  
  ## descriptive stats
  
  sub2_factors$desc_stats <- sub2_factors$variables$eff_size$variable %>% 
    map(~explore(cov_data$clear_data %>% 
                   filter(!is.na(.data[[.x]])), 
                 split_factor = .x, 
                 variables = 'ipq_sub2', 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map2(., sub2_factors$variables$eff_size$levels, 
         ~set_names(.x, c('variable', .y))) %>% 
    set_names(sub2_factors$variables$eff_size$variable)
  
  ## comparisons
  
  sub2_factors$factor_test <- 
    list(split_factor = sub2_factors$variables$eff_size$variable, 
         types = sub2_factors$variables$eff_size$eff_type) %>% 
    pmap_dfr(compare_variables, 
             cov_data$clear_data, 
             variables = 'ipq_sub2', 
             what = 'eff_size', 
             ci = FALSE, 
             exact = FALSE, 
             pub_styled = TRUE) %>% 
    mutate(split_factor = sub2_factors$variables$eff_size$variable, 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
  ## Spearman's correlations 
  
  sub2_factors$corr_test <- sub2_factors$variables$correlation$variable %>% 
    map_dfr(~correlate_variables(cov_data$clear_data[c('ipq_sub2', .x)] %>% 
                                   filter(complete.cases(.)), 
                                 variables = c('ipq_sub2', .x), 
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
  
  sub2_factors$violin_plots <- 
    map2(sub2_factors$factor_test$split_factor, 
         sub2_factors$factor_test$plot_cap, 
         ~plot_variable(cov_data$clear_data %>% 
                          filter(!is.na(.data[[.x]])), 
                        split_factor = .x, 
                        variable = 'ipq_sub2', 
                        type = 'violin', 
                        plot_title = translate_var(.x, out_value = 'label_long'), 
                        plot_subtitle = .y, 
                        y_lab = translate_var('ipq_sub2'), 
                        x_lab = translate_var(.x), 
                        point_hjitter = 0, 
                        cust_theme = globals$common_theme)) %>% 
    map2(sub2_factors$x_axes, 
        ~.x + 
          scale_x_discrete(labels = .y) + 
          scale_fill_brewer() + 
          theme(plot.tag = element_blank())) %>% 
    set_names(sub2_factors$factor_test$split_factor)
  
# Plotting the correlations -----
  
  insert_msg('Plotting the correlations')
  
  sub2_factors$corr_plots <- 
    map2(sub2_factors$corr_test$variable2, 
         sub2_factors$corr_test$plot_cap, 
         ~plot_correlation(cov_data$clear_data[c('ipq_sub2', .x)] %>% 
                             filter(complete.cases(.)), 
                           variables = c(.x, 'ipq_sub2'), 
                           type = 'correlation',
                           plot_title = translate_var(.x, out_value = 'label_long'), 
                           plot_subtitle = .y, 
                           x_lab = translate_var(.x, out_value = 'axis_lab'), 
                           y_lab = translate_var('ipq_sub2', out_value = 'axis_lab'), 
                           show_trend = FALSE, 
                           point_hjitter = 0, 
                           point_wjitter = 0, 
                           cust_theme = globals$common_theme)) %>% 
    map(~.x +
          geom_smooth(method = 'lm', 
                      formula = y ~ x + I(x^2)) +
          labs(subtitle = paste(.x$labels$subtitle, 
                                .x$labels$tag, 
                                sep = ', ')) + 
          theme(plot.tag = element_blank())) %>% 
    set_names(sub2_factors$corr_test$variable2)
  
# END -----
  
  insert_tail()