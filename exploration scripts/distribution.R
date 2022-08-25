# Distribution tests and plots for numeric variables ------

  insert_head()

# container ------

  distr_tests <- list()
  
# globals ------
  
  insert_msg('Globals setup')
  
  distr_tests$num_vars <- map2(expl$variables, 
                               expl$var_numeric, 
                              ~.x[.y]) %>% 
    reduce(c)
  
  plan('multisession')
  
# normality testing: identity transformation -------
  
  insert_msg('Normality testing, identity transformation')
  
  ## Shapiro - Wilk tests
  
  distr_tests$norm_test <- explore(cov_data$clear_data, 
                                   variables = distr_tests$num_vars, 
                                   what = 'normality', 
                                   pub_styled = TRUE)
  
  ## QQ plots
  
  distr_tests$norm_plots <- explore(cov_data$clear_data, 
                                    variables = distr_tests$num_vars, 
                                    what = 'plots', 
                                    type = 'qq', 
                                    cust_theme = globals$common_theme) %>% 
    map2(., translate_var(distr_tests$num_vars), 
         ~.x + 
           labs(title = .y, 
                subtitle = .x$labels$tag %>% 
                  stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                  stri_replace(fixed = 'Complete: ', replacement = '')) + 
           theme(plot.tag = element_blank()))

# normality testing: log transformation ------
  
  insert_msg('Normality testing, log transformation')
  
  ## Shapiro - Wilk tests
  
  distr_tests$norm_test_log <- 
    explore(cov_data$clear_data %>% 
              map_dfc(function(x) if(is.numeric(x)) log(x + 1) else x), 
            variables = distr_tests$num_vars, 
            what = 'normality', 
            pub_styled = TRUE)
  
  ## QQ plots
  
  distr_tests$norm_plots_log <- 
    explore(cov_data$clear_data %>% 
              map_dfc(function(x) if(is.numeric(x)) log(x + 1) else x), 
            variables = distr_tests$num_vars, 
            what = 'plots', 
            type = 'qq', 
            cust_theme = globals$common_theme) %>% 
    map2(., translate_var(distr_tests$num_vars), 
         ~.x + 
           labs(title = paste(.y, 'log transformed', sep = ', '), 
                subtitle = .x$labels$tag %>% 
                  stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                  stri_replace(fixed = 'Complete: ', replacement = '')) + 
           theme(plot.tag = element_blank()))
  
# normality testing: sqrt transformation ------
  
  insert_msg('Normality testing, sqrt transformation')
  
  ## Shapiro - Wilk tests
  
  distr_tests$norm_test_sqrt <- 
    explore(cov_data$clear_data %>% 
              map_dfc(function(x) if(is.numeric(x)) sqrt(x) else x), 
            variables = distr_tests$num_vars, 
            what = 'normality', 
            pub_styled = TRUE)
  
  ## QQ plots
  
  distr_tests$norm_plots_sqrt <- 
    explore(cov_data$clear_data %>% 
              map_dfc(function(x) if(is.numeric(x)) sqrt(x) else x), 
            variables = distr_tests$num_vars, 
            what = 'plots', 
            type = 'qq', 
            cust_theme = globals$common_theme) %>% 
    map2(., translate_var(distr_tests$num_vars), 
         ~.x + 
           labs(title = paste(.y, 'sqrt transformed', sep = ', '), 
                subtitle = .x$labels$tag %>% 
                  stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                  stri_replace(fixed = 'Complete: ', replacement = '')) + 
           theme(plot.tag = element_blank()))
  
# EOV testing between the severity strata ------
  
  insert_msg('EOV testing between the severity strata')
  
  distr_tests$eov_test <- compare_variables(cov_data$clear_data,
                                            split_factor = 'cat_WHO', 
                                            variables = distr_tests$num_vars, 
                                            what = 'variance', 
                                            pub_styled = TRUE)
  
# Mean/variance comparison: possible Poisson distribution -------
  
  insert_msg('Mean - variance comparison')
  
  ## mean and variance
  
  distr_tests$mean_var <-  get_mvr(cov_data$clear_data, 
                                   variables = distr_tests$num_vars)
  
  ## 95% CI for mean and variance obtained by bootstrap
  
  set.seed(123)

  distr_tests$boot_tbl <- 1:1000 %>% 
    map(~sample(1:nrow(cov_data$clear_data), 
               nrow(cov_data$clear_data), 
               replace = TRUE)) %>% 
    map(~cov_data$clear_data[.x, ])
  
  distr_tests$mvr_boot <- distr_tests$boot_tbl %>% 
    future_map(~get_mvr(.x, variables = distr_tests$num_vars)[c('variable', 'mvr')]) %>% 
    map(column_to_rownames, 'variable') %>% 
    reduce(cbind) %>% 
    as.matrix
  
  distr_tests$mvr_boot <- distr_tests$num_vars %>% 
    map(~distr_tests$mvr_boot[.x, ]) %>% 
    map_dfr(~tibble(mvr_mean = mean(.x), 
                    mvr_lower = quantile(.x, 0.025), 
                    mvr_upper = quantile(.x, 0.975))) %>% 
    mutate(variable = distr_tests$num_vars)
  
  distr_tests$mean_var <- left_join(distr_tests$mean_var, 
                                    distr_tests$mvr_boot, 
                                    by = 'variable')
  
# Plotting mean to variance ratios ------
  
  insert_msg('Plotting the mean - variance ratios')
  
  distr_tests$mvr_plot <- distr_tests$mean_var %>% 
    map_dfc(~ifelse(is.infinite(.x), NA, .x)) %>% 
    ggplot(aes(x = mvr, 
               y = reorder(variable, mvr))) + 
    geom_errorbarh(aes(xmin = mvr_lower, 
                       xmax = mvr_upper), 
                   height = 0, 
                   color = 'steelblue') + 
    geom_point(shape = 16, 
               size = 2, 
               color = 'steelblue') + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    scale_y_discrete(labels = translate_var(distr_tests$num_vars)) + 
    scale_x_continuous(trans = 'log') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Variable mean and variance', 
         subtitle = '95%CI: bootstrap, n = 1000, percentile method', 
         x = 'mean MVR \u00B1 95%CI')
  
# END -----
  
  plan('sequential')
  
  distr_tests$boot_tbl <- NULL
  distr_tests$mvr_boot <- NULL
  
  distr_tests <- compact(distr_tests)
  
  insert_tail()