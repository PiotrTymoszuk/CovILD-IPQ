# Comparison of the CovILD subset included in the analysis with the participants
# excluded from the analysis because of variable or BIPQ missingness or lacking
# COVID-19 sequelae at the one-year follow-up.
#
# Done with non-parametric tests since multiple numeric features are 
# non-normally distributed.

  insert_head()
  
# container ------
  
  inc_bias <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## analysis table
  
  inc_bias$analysis_tbl <- cov_data$raw_data %>% 
    mutate(analysis_status = ifelse(ID %in% cov_data$clear_data$ID, 
                                    'included', 'excluded'), 
           analysis_status = factor(analysis_status, 
                                    c('included', 'excluded')))
  
  ## analysis variables
  
  inc_bias$variables <- expl$variables[c('baseline', 'fup', 'lab')] %>% 
    map(~.x[.x %in% names(inc_bias$analysis_tbl)])
  
  inc_bias$eff_type <- inc_bias$variables %>% 
    map(~map_chr(.x, 
             ~ifelse(is.numeric(inc_bias$analysis_tbl[[.x]]), 
                'wilcoxon_r', 'cramer_v')))
  
  inc_bias$plot_type <- inc_bias$variables %>% 
    map(~map_chr(.x, 
                 ~ifelse(is.numeric(inc_bias$analysis_tbl[[.x]]), 
                         'violin', 'stack')))
  
  ## parallel backend
  
  plan('multisession')
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  inc_bias$desc_stats <- inc_bias$variables %>% 
    map(~explore(inc_bias$analysis_tbl, 
                 split_factor = 'analysis_status', 
                 variables = .x, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(inc_bias$analysis_tbl$analysis_status)))
  
# Testing for differences between the analysis inclusion strata ------
  
  insert_msg('Testing for differences')
  
  inc_bias$test_results <- list(variables = inc_bias$variables, 
                                types = inc_bias$eff_type) %>% 
    future_pmap(compare_variables,
                inc_bias$analysis_tbl, 
                split_factor = 'analysis_status', 
                what = 'eff_size', 
                ci = FALSE, 
                exact = FALSE, 
                pub_styled = TRUE, 
                adj_method = 'none', 
                .options = furrr_options(seed = TRUE)) %>% 
    map(format_test_jps) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')

  inc_bias[c('baseline_plots', 
             'fup_plots', 
             'lab_plots')] <- names(inc_bias$variables) %>% 
    map(function(var_class) list(variable = inc_bias$variables[[var_class]], 
                                 plot_title = inc_bias$variables[[var_class]] %>% 
                                   translate_var(out_value = 'label_long'), 
                                 plot_subtitle = inc_bias$test_results[[var_class]]$plot_cap, 
                                 y_lab = inc_bias$variables[[var_class]] %>% 
                                   translate_var(out_value = 'axis_lab'), 
                                 type = inc_bias$plot_type[[var_class]]) %>% 
          pmap(plot_variable, 
               inc_bias$analysis_tbl, 
               split_factor = 'analysis_status', 
               scale = 'percent', 
               x_lab = 'analysis status', 
               cust_theme = globals$common_theme) %>% 
          map(~.x + 
                labs(tag = .x$labels$tag %>% 
                       stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                       paste0('\n', .))) %>% 
          set_names(inc_bias$variables[[var_class]]))

# END ----
  
  plan('sequential')
  
  insert_tail()