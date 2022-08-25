# Baseline clinical characteristic of the study cohort and recovery status 
# at the one-year follow-up

  insert_head()
  
# container -----
  
  cohort <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  cohort$analysis_tbl <- rbind(cov_data$clear_data %>% 
                                 mutate(subset_var = 'cohort'), 
                               cov_data$clear_data %>% 
                                 mutate(subset_var = cat_WHO)) %>% 
    mutate(subset_var = factor(subset_var, c('cohort', 'A', 'HM', 'HS')))
  
  plan('multisession')
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  cohort$desc_stats <- expl$variables %>% 
    future_map(~explore(cohort$analysis_tbl, 
                 split_factor = 'subset_var', 
                 variables = .x, 
                 what = 'table', 
                 pub_styled = TRUE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(cohort$analysis_tbl$subset_var)))
  
# Testing for differences between the severity strata groups -------
  
  insert_msg('Testing')
  
  cohort$test_results <- list(variables = expl$variables, 
                              types = expl$test_type) %>% 
    future_pmap(compare_variables,
                cov_data$clear_data, 
                split_factor = 'cat_WHO', 
                what = 'test', 
                ci = FALSE, 
                pub_styled = TRUE, 
                adj_method = 'none', 
                .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots of the psych variables in the severity groups ------
  
  insert_msg('Plots of the psych variable values in the severity groups')
  
  cohort$psy_plots <- list(variable = expl$variables$psych, 
                           plot_title = expl$variables$psych %>% 
                             translate_var(out_value = 'label_long'), 
                           plot_subtitle = cohort$test_results$psych$plot_cap, 
                           y_lab = expl$variables$psych %>% 
                             translate_var(out_value = 'axis_lab')) %>% 
    pmap(plot_variable, 
         cov_data$clear_data, 
         split_factor = 'cat_WHO', 
         type = 'violin', 
         x_lab = 'COVID-19 severity', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .)) + 
          scale_fill_manual(values = globals$sev_colors[1:3])) %>% 
    set_names(expl$variables$psych)
  
# END -----
  
  plan('sequential')
  
  insert_tail()