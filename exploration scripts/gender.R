# Comparison of baseline characteristics, recovery and mental health feattures 
# between females and males.
#
# Participants meeting the inclusion criteria are analyzed. Non-parameteric 
# testing since multiple numeric variables are non-normally distributed

  insert_head()
  
# container ------
  
  gender <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## test and plot types
  
  gender$eff_type <- expl$variables %>% 
    map(~map_chr(.x, 
                 ~ifelse(is.numeric(cov_data$clear_data[[.x]]), 
                         'wilcoxon_r', 'cramer_v')))
  
  gender$plot_type <- expl$variables %>% 
    map(~map_chr(.x, 
                 ~ifelse(is.numeric(cov_data$clear_data[[.x]]), 
                         'violin', 'stack')))
  
  ## parallel backend
  
  plan('multisession')
  
# descriptive stats -------
  
  insert_msg('Descriptive statistics')
  
  gender$desc_stats <- expl$variables %>% 
    map(~explore(cov_data$clear_data, 
                 split_factor = 'sex', 
                 variables = .x, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(cov_data$clear_data$sex)))
  
# Testing for differences between the genders -------
  
  insert_msg('Testing for differences between the genders')
  
  gender$test_results <- list(variables = expl$variables, 
                              types = gender$eff_type) %>% 
    future_pmap(compare_variables,
                cov_data$clear_data, 
                split_factor = 'sex', 
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
  
  gender[paste0(names(expl$variables), '_plots')] <- names(expl$variables) %>% 
    map(function(var_class) list(variable = expl$variables[[var_class]], 
                                 plot_title = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'label_long'), 
                                 plot_subtitle = gender$test_results[[var_class]]$plot_cap, 
                                 y_lab = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'axis_lab'), 
                                 type = gender$plot_type[[var_class]]) %>% 
          pmap(plot_variable, 
               cov_data$clear_data, 
               split_factor = 'sex', 
               scale = 'percent', 
               x_lab = '', 
               cust_theme = globals$common_theme) %>% 
          map(~.x + 
                labs(tag = .x$labels$tag %>% 
                       stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                       paste0('\n', .))) %>% 
          set_names(expl$variables[[var_class]]))
  
# END ------
  
  insert_tail()