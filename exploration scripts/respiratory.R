# Characteristic of the patients with and without respiratory comorbidity

  insert_head()

# container ------

  respi <- list()

# analysis globals -----

  insert_msg('Analysis globals')

  ## test and plot types

  respi$eff_type <- expl$variables %>% 
    map(~map_chr(.x, 
                 ~ifelse(is.numeric(cov_data$clear_data[[.x]]), 
                         'wilcoxon_r', 'cramer_v')))

  respi$plot_type <- expl$variables %>% 
    map(~map_chr(.x, 
                 ~ifelse(is.numeric(cov_data$clear_data[[.x]]), 
                         'violin', 'stack')))

  ## parallel backend

  plan('multisession')
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  respi$desc_stats <- expl$variables %>% 
    map(~explore(cov_data$clear_data, 
                 split_factor = 'respi_comorb', 
                 variables = .x, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(cov_data$clear_data$respi_comorb)))
  
# Checking for significance of the differences ------
  
  insert_msg('Checking for differences between the strata')
  
  respi$test_results <- list(variables = expl$variables, 
                             types = respi$eff_type) %>% 
    future_pmap(compare_variables,
                cov_data$clear_data, 
                split_factor = 'respi_comorb', 
                what = 'eff_size', 
                ci = FALSE, 
                exact = FALSE, 
                pub_styled = TRUE, 
                adj_method = 'none', 
                .options = furrr_options(seed = TRUE)) %>% 
    map(format_test_jps) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting -------
  
  insert_msg('Plotting')
  
  respi[paste0(names(expl$variables), '_plots')] <- names(expl$variables) %>% 
    map(function(var_class) list(variable = expl$variables[[var_class]], 
                                 plot_title = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'label_long'), 
                                 plot_subtitle = respi$test_results[[var_class]]$plot_cap, 
                                 y_lab = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'axis_lab'), 
                                 type = respi$plot_type[[var_class]]) %>% 
          pmap(plot_variable, 
               cov_data$clear_data, 
               split_factor = 'respi_comorb', 
               scale = 'percent', 
               x_lab = 'Respiratory comorbidity', 
               cust_theme = globals$common_theme) %>% 
          map(~.x + 
                labs(tag = .x$labels$tag %>% 
                       stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                       paste0('\n', .))) %>% 
          set_names(expl$variables[[var_class]]))
  
# END ------
  
  insert_tail()