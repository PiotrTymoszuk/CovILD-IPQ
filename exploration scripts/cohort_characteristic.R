# Baseline clinical characteristic of the study cohort and recovery status 
# at the one-year follow-up.
#
# Descriptive statistics for the entire cohort and COVID-19 severity strata
# are presented. Differences between the cohorts are investigated with 
# non parametric Chi-squared and Kruskal-Wallis test (multiple numeric 
# variables are non-normally distributed).


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
    map(format_test_jps) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots of variables in the severity groups ------
  
  insert_msg('Plots of variable values in the severity groups')
  
  cohort[paste0(names(expl$variables), '_plots')] <- names(expl$variables) %>% 
    map(function(var_class) list(variable = expl$variables[[var_class]], 
                                 plot_title = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'label_long'), 
                                 plot_subtitle = cohort$test_results[[var_class]]$plot_cap, 
                                 y_lab = expl$variables[[var_class]] %>% 
                                   translate_var(out_value = 'axis_lab'), 
                                 type = expl$plot_type[[var_class]]) %>% 
          pmap(plot_variable, 
               cov_data$clear_data, 
               split_factor = 'cat_WHO', 
               scale = 'percent', 
               x_lab = 'COVID-19 severity', 
               cust_theme = globals$common_theme) %>% 
          map(~.x + 
                labs(tag = .x$labels$tag %>% 
                       stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                       paste0('\n', .))) %>% 
          set_names(expl$variables[[var_class]]))
  
# Symptoms in the severity groups, a summary plot -------
  
  insert_msg('Symptom frequency plot')
  
  ## symptom variables, ordered by their effect size of the difference
  ## between the severity groups
  
  cohort$symptoms <- c('fatigue_sympt', 
                       'dyspnoe_sympt', 
                       'cough_sympt', 
                       'sleep_sympt', 
                       'night_sweat_sympt', 
                       'anosmia_sympt', 
                       'derma_sympt', 
                       'gastro_sympt', 
                       'hair_loss_sympt', 
                       'Chalder_FS_bimodal')
  
  cohort$sympt_tests <- cohort$test_results$fup %>% 
    filter(variable %in% cohort$symptoms) %>% 
    arrange(desc(eff_size))
  
  cohort$symptoms <- cohort$sympt_tests$variable
  
  ## labels with p values
  
  cohort$symptom_labs <- cohort$sympt_tests %>% 
    mutate(var_lab = translate_var(variable, out_value = 'label'), 
           var_lab = stri_replace(var_lab, fixed = ' ', replacement = '\n'), 
           var_lab = stri_replace(var_lab, fixed = ' (', replacement = '\n('),
           var_lab = stri_replace(var_lab, 
                                  fixed = 'Gastrointestinal', 
                                  replacement = 'GI'), 
           var_lab = stri_replace(var_lab, 
                                  fixed = 'sympt.', 
                                  replacement = 'symptoms'), 
           var_lab = stri_replace(var_lab, 
                                  fixed = 'bimodal', 
                                  replacement = 'bi.'), 
           var_lab = paste(var_lab, significance, sep = '\n'))
  
  cohort$symptom_labs <- set_names(cohort$symptom_labs$var_lab, 
                                   cohort$symptom_labs$variable)
  
  ## stack plot
  
  cohort$symptom_plot <- 
    draw_freq_panel(cov_data$clear_data, 
                    variables = cohort$symptoms, 
                    split_factor = 'cat_WHO', 
                    labeller = as_labeller(cohort$symptom_labs), 
                    plot_title = 'PSS and COVID-19 severity', 
                    y_lab = 'COVID-19 severity', 
                    x_lab = '% of severity strata', 
                    cust_theme = globals$common_theme) + 
    scale_fill_manual(values = c(no = 'cornsilk', 
                                 yes = 'steelblue4'), 
                      labels = c(no = 'absent', 
                                 yes = 'present'), 
                      name = '') + 
    scale_y_discrete(labels = globals$sev_labels)
  
# END -----
  
  plan('sequential')
  
  insert_tail()