# Modeling with Bayesian Lasso

  insert_head()
  
# container -----
  
  blass_mod <- list()
  
  registerDoParallel(7)
  
# model construction -------
  
  insert_msg('Model construction')
  
  ## tune grid
  
  blass_mod$tune_grid <- data.frame(sparsity = seq(0.3, 0.7, by = 0.1))
  
  ## models
  
  set.seed(1234)
  
  blass_mod$models <- mod$multi_form %>% 
    map(train, 
        data = mod$multi_tbl, 
        method = 'blasso', 
        metric = 'RMSE', 
        trControl = mod$tr_control, 
        tuneGrid = blass_mod$tune_grid) %>% 
    map(as_caretx)
  
# Residuals -----
  
  insert_msg('Model residuals')
  
  blass_mod$resid_tbl <- blass_mod$models %>% 
    map(resid)
  
  ## normality testing
  
  blass_mod$resid_norm <- blass_mod$resid_tbl %>% 
    map(~map(.x, 
             explore, 
             what = 'normality', 
             variables = '.resid') %>% 
          map2_dfr(., names(.), ~mutate(.x, mod_type = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## residual plots
  
  blass_mod$resid_plots <- list(x = blass_mod$models, 
                                plot_title = names(blass_mod$models) %>% 
                                  translate_var %>% 
                                  map(paste, c('training', 'CV'), sep = ', ')) %>% 
    pmap(plot, 
         type = 'diagnostic', 
         cust_theme = globals$common_theme) %>% 
    transpose
  
# Fit stats ------
  
  insert_msg('Fit stats')
  
  ## stat table
  
  blass_mod$cv_fit_stats <- blass_mod$models %>% 
    map(summary, plain = TRUE) %>% 
    map2(., names(.), 
             ~mutate(.x, response = .y)) %>% 
    map(mutate, prediction = factor(prediction, c('train', 'cv')))
  
  blass_mod$plot_cap <- blass_mod$cv_fit_stats %>% 
    map(as.data.frame) %>% 
    map(~paste0('R\u00B2 = ', signif(.x[4, 2], 2), 
                ', RMSE = ', signif(.x[3, 2], 2))) %>% 
    map(~paste0(.x, ', total n = ', nrow(mod$multi_tbl)))
  
  ## summary plots for R-squared and RMSE
  
  blass_mod$cv_fit_plots <- 
    plot_rsq_rmse(reduce(blass_mod$cv_fit_stats, rbind), 
                  plot_subtitle = 'multi-variate Bayesian LASSO modeling') %>% 
    map2(., c(0.7, 1.1), 
         ~.x + 
           expand_limits(x = .y) + 
           scale_y_discrete(labels = translate_var(names(blass_mod$models))))
  
# Calibration plots ------
  
  insert_msg('Plotting fitted vs outcome')
  
  blass_mod$fit_plots <- list(x = blass_mod$models, 
                              plot_title = names(blass_mod$models) %>% 
                                translate_var %>% 
                                map(paste, c('training', 'CV'), sep = ', ')) %>% 
    pmap(plot, 
         type = 'fit', 
         cust_theme = globals$common_theme) %>% 
    map(~map(.x, ~.x + 
               labs(subtitle = .x$labels$subtitle %>% 
                      stri_replace(fixed = 'sq', replacement = '\u00B2'), 
                    x = 'outcome', 
                    y = 'fitted')))
  
# Model coefs: median values -----
  
  insert_msg('Model coefficients')
  
  blass_mod$inference <- blass_mod$models %>% 
    map(~.x$finalModel$beta) %>% 
    map(as.data.frame) %>% 
    map(~map_dbl(.x, median)) %>% 
    map2(., names(.), 
         ~tibble(response = .y, 
                 est_number = names(.x), 
                 estimate = .x))
  
  ## appending with the parameter names
  
  blass_mod$inference <- blass_mod$models %>% 
    map(~colnames(.x$finalModel$X)) %>% 
    map2(., blass_mod$inference, 
         ~cbind(parameter = .x, 
                .y)) %>% 
    map(filter, estimate != 0) %>% 
    map(as_tibble)
  
  ## levels and level n numbers
  
  blass_mod$inference <- get_var_level(blass_mod$inference)
  
# Estimate plots ------
  
  insert_msg('Bubble plots with the model estimates')
  
  blass_mod$estimate_plots <- 
    list(est_data = blass_mod$inference, 
         plot_title = translate_var(names(blass_mod$inference)) %>% 
           paste('1-year follow-up', sep = ', '), 
         plot_subtitle = blass_mod$plot_cap[names(blass_mod$inference)]) %>% 
    pmap(est_bubble, 
         x_lab = expression('normalized ' * beta[BayesLASSO]))
  
# END -----
  
  stopImplicitCluster()