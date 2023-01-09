# LASSO modeling 

  insert_head()
  
# container -----
  
  lasso_mod <- list()
  
# globals ------
  
  insert_msg('Globals setup')
  
  ## response vectors and explanatory variable matrices
  
  lasso_mod$response <- mod$responses %>% 
    map(~mod$multi_tbl[[.x]])

  lasso_mod$variables <- model.matrix(~., data = mod$multi_tbl[mod$variables])
  
  ## alpha and n repetitions
  
  lasso_mod$alpha <- 1
  lasso_mod$n_rep <- 200
  
  ## folds for CV
  
  set.seed(1234)
  
  lasso_mod$folds <- 1:lasso_mod$n_rep %>% 
    map(~createFolds(y = lasso_mod$response[[1]], 
                     k = 10, 
                     list = FALSE, 
                     returnTrain = TRUE)) %>% 
    set_names(paste0('rep_', 1:lasso_mod$n_rep))

  ## parallel backends
  
  plan('multisession')
  
# lambda finding -------
  
  insert_msg('Lambda finding')
  
  ## model construction
  
  lasso_mod$lambda_tune <- lasso_mod$response %>% 
    map(function(resp) lasso_mod$folds %>% 
          future_map(~cv.glmnet(x = lasso_mod$variables, 
                                y = resp, 
                                foldid = .x, 
                                type.measure = 'default', 
                                family = 'gaussian', 
                                alpha = lasso_mod$alpha), 
                     .options = furrr_options(seed = TRUE)))

  ## extracting the optimal lambdas
  
  lasso_mod$lambda_tbl <- lasso_mod$lambda_tune %>% 
    map(~map(.x, 
             ~as_tibble(.x[c('lambda', 'cvm', 'cvup', 'cvlo', 
                             'lambda.min', 'lambda.1se')]))) %>% 
    map(~map_dfr(.x, filter, lambda == lambda.1se))
  
  lasso_mod$opt_lambda <- lasso_mod$lambda_tbl %>% 
    map(filter, cvm == min(cvm)) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  lasso_mod$tune_grids <- lasso_mod$opt_lambda$lambda %>% 
    map(~data.frame(alpha = lasso_mod$alpha, 
                    lambda = .x)) %>% 
    set_names(names(lasso_mod$lambda_tbl))
  
# Building the training model and CV with caret ------
  
  insert_msg('Building caret models and CV')
  
  set.seed(1234)
  
  registerDoParallel(cl = 7)

  lasso_mod$models <- list(form =  mod$multi_form, 
                         tuneGrid = lasso_mod$tune_grids) %>% 
    pmap(train, 
         data = mod$multi_tbl, 
         method = 'glmnet', 
         metric = 'RMSE', 
         trControl = mod$tr_control) %>% 
    map(as_caretx)
  
  stopImplicitCluster()
  
# Model assumptions -----
  
  insert_msg('Model assumptions')
  
  ## normality of residuals
  
  lasso_mod$resid_tbl <- lasso_mod$models %>% 
    map(resid)
  
  lasso_mod$resid_norm <- lasso_mod$resid_tbl %>% 
    map(~map(.x, 
             explore, 
             what = 'normality', 
             variables = '.resid') %>% 
          map2_dfr(., names(.), ~mutate(.x, mod_type = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))

  ## residual plots
  
  lasso_mod$resid_plots <- list(x = lasso_mod$models, 
                              plot_title = names(lasso_mod$models) %>% 
                                translate_var %>% 
                                map(paste, c('training', 'CV'), sep = ', ')) %>% 
    pmap(plot, 
         type = 'diagnostic', 
         cust_theme = globals$common_theme) %>% 
    transpose

# Fit stats -------
  
  insert_msg('Fit statistics')
  
  ## stat table
  
  lasso_mod$cv_fit_stats <- lasso_mod$models %>% 
    map(summary, plain = TRUE) %>% 
    map2(., names(.), 
             ~mutate(.x, response = .y)) %>% 
    map(mutate, prediction = factor(prediction, c('train', 'cv')))
  
  lasso_mod$plot_cap <- lasso_mod$cv_fit_stats %>% 
    map(as.data.frame) %>% 
    map(~paste0('R\u00B2 = ', signif(.x[4, 2], 2), 
                ', RMSE = ', signif(.x[3, 2], 2))) %>% 
    map(~paste0(.x, ', total n = ', nrow(mod$multi_tbl)))
  
  ## summary plots for R-squared and RMSE
  
  lasso_mod$cv_fit_plots <- 
    plot_rsq_rmse(reduce(lasso_mod$cv_fit_stats, rbind), 
                  plot_subtitle = 'multi-variate LASSO modeling') %>% 
    map2(., c(0.7, 1.1), 
         ~.x + 
           expand_limits(x = .y) + 
           scale_y_discrete(labels = translate_var(names(lasso_mod$models))))

# Model calibration -----  
  
  insert_msg('Plotting fitted vs outcome')
  
  lasso_mod$fit_plots <- list(x = lasso_mod$models, 
                              plot_title = names(lasso_mod$models) %>% 
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
  
# Model estimates ------
  
  insert_msg('Model estimates')
  
  lasso_mod$inference <- map2(lasso_mod$models, 
                            lasso_mod$opt_lambda$lambda, 
                            ~coef(.x$finalModel, s = .y)) %>% 
    map(~as.data.frame(as.matrix(.x))) %>% 
    map(rownames_to_column, 'parameter') %>% 
    map(as_tibble) %>% 
    map(set_names, c('parameter', 'estimate')) %>% 
    map(filter, estimate != 0) %>% 
    get_var_level
  
# Bubble plots with the estimate values ------
  
  insert_msg('Bubble plots with the estimate values')
  
  lasso_mod$estimate_plots <- 
    list(est_data = lasso_mod$inference, 
         plot_title = translate_var(names(lasso_mod$inference)) %>% 
           paste('1-year follow-up', sep = ', '), 
         plot_subtitle = lasso_mod$plot_cap[names(lasso_mod$inference)]) %>% 
    pmap(est_bubble, 
         x_lab = expression('normalized ' * beta[LASSO]))

# END ------
  
  plan('sequential')
  
  insert_tail()