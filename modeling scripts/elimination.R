# Multi-variate modeling with backward elimination of the terms

  insert_head()

# container -----
  
  eli_mod <- list()
  
  registerDoParallel()
  
# Full model construction -----
  
  insert_msg('Building full models')
  
  eli_mod$full_models <- list(formula = mod$multi_form) %>% 
    pmap(make_lm, 
         data = mod$multi_tbl, 
         mod_fun = lm, 
         family = NULL)
  
# Model optimization -------
  
  insert_msg('Model optimization')
  
  eli_mod$opt_models <- eli_mod$full_models %>% 
    map(~safely(step)(.x, 
                      step_fun = MASS::stepAIC, 
                      direction = 'both', 
                      trace = FALSE, 
                      k = log(lmqc::nobs(.x)))) %>% 
    map(~.x$result) %>% 
    compact

# Model assumptions ------
  
  insert_msg('Model assumptions: normality testing and plots')
  
  ## tabular results
  
  eli_mod$resid_tbl <- eli_mod$opt_models %>% 
    map(resid, type.predict = 'response')
  
  ## Shapiro - Wilk test on residuals
  
  eli_mod$resid_norm <- eli_mod$resid_tbl %>% 
    map(explore, 
        what = 'normality', 
        variables = '.resid') %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## diagnostic plots
  
  eli_mod$resid_plots <- eli_mod$opt_models %>% 
    map(plot, 
        type = 'residuals', 
        cust_theme = globals$common_theme, 
        type.predict = 'response')
  
# Model fit stats -------
  
  insert_msg('Model fit stats')
  
  eli_mod$fit_stats <- eli_mod$opt_models %>% 
    map_dfr(summary, type = 'fit') %>% 
    mutate(plot_cap = paste0('R\u00B2 = ', signif(raw_rsq, 2), 
                             ', RMSE = ', signif(rmse, 2), 
                             ', total n = ', n_complete))
  
# Model inference -----
  
  insert_msg('Model inference')
  
  eli_mod$inference <- eli_mod$opt_models %>% 
    map(summary, type = 'inference')
  
# Forest plots ------
  
  insert_msg('Forest plots')
  
  eli_mod$forest_plots <- list(x = map(eli_mod$inference, 
                                       mutate, 
                                       variable = translate_var(variable)), 
                               plot_title = names(eli_mod$inference) %>% 
                                 translate_var %>% 
                                 paste('1-year follow-up', sep = ', '), 
                               plot_subtitle = eli_mod$fit_stats$plot_cap) %>% 
    pmap(plot_forest, 
         x_lab = expression('normalized ' * beta * ' \u00B1 95%CI'), 
         cust_theme = globals$common_theme, 
         cutpoint = 0, 
         hide_baseline = TRUE)
  
  for(i in names(eli_mod$forest_plots)) {
    
    eli_mod$forest_plots[[i]]$data <- eli_mod$forest_plots[[i]]$data %>% 
      mutate(y_ax = stri_replace(y_ax, fixed = ': yes', replacement = ''), 
             y_ax = reorder(y_ax, estimate))
    
    eli_mod$forest_plots[[i]] <- eli_mod$forest_plots[[i]] + 
      scale_y_discrete(limits = sort(eli_mod$forest_plots[[i]]$data$y_ax))

  }
  
# Percents explained deviance associated with explanatory vars ------
  
  insert_msg('Percentages of explained deviance')
  
  ## deviances
  
  eli_mod$deviances <- eli_mod$opt_models %>% 
    map(anova) %>% 
    map(mutate, 
        eff_size = cut(frac_explained, 
                       c(-Inf, 0.01, 0.09, 0.25, Inf), 
                       c('none', 'weak', 'moderate', 'large')))
  
  ## plots
  
  eli_mod$dev_plots <- map2(eli_mod$deviances %>% 
                              map(mutate, 
                                variable = translate_var(variable)) %>% 
                              map(filter, variable != 'Residuals'), 
                            names(eli_mod$inference) %>% 
                              translate_var %>% 
                              paste('1-year follow-up', sep = ', '), 
                            ~ggplot(.x, 
                                    aes(x = frac_explained, 
                                        y = reorder(variable, frac_explained), 
                                        fill = eff_size)) + 
                              geom_bar(stat = 'identity', 
                                       color = 'black') + 
                              geom_vline(xintercept = 0.01, 
                                         linetype = 'dashed') + 
                              geom_vline(xintercept = 0.09, 
                                         linetype = 'dashed') + 
                              geom_vline(xintercept = 0.25, 
                                         linetype = 'dashed') + 
                              scale_fill_manual(values = mod$size_colors, 
                                                name = 'Effect size') + 
                              globals$common_theme + 
                              theme(axis.title.y = element_blank()) + 
                              labs(title = .y, 
                                   subtitle = 'fraction explained variance', 
                                   x = 'fraction total variance'))
  
# Cross-validation ------
  
  insert_msg('Cross-validation via caret')
  
  ## caret models
  
  set.seed(1234)
  
  eli_mod$caret_models <- 
    list(form = map(eli_mod$opt_models, 
                    formula)) %>% 
    pmap(train, 
         data = mod$multi_tbl, 
         method = 'lm', 
         metric = 'RMSE', 
         trControl = mod$tr_control) %>% 
    map(as_caretx)
  
# Cross-validation fit stats ------
  
  insert_msg('Cross-validation fit stats')
  
  ## fit stats
  
  eli_mod$cv_fit_stats <- eli_mod$caret_models %>% 
    map(summary, plain = TRUE) %>% 
    map2(., names(.), 
         ~mutate(.x, response = .y)) %>% 
    map(mutate, prediction = factor(prediction, c('train', 'cv')))
  
  ## R-squared and RMSE plots
  
  eli_mod$cv_fit_plots <- 
    plot_rsq_rmse(reduce(eli_mod$cv_fit_stats, rbind) %>% 
                    mutate(estimate = ifelse(estimate < 0, 0, estimate)), 
                  plot_subtitle = 'multi-variate linear modeling, backward elimination') %>%
    map2(., c(0.65, 1.1), 
         ~.x + 
           expand_limits(x = .y) + 
           scale_y_discrete(labels = translate_var(names(eli_mod$caret_models))))

# Calibration plots: response vs fitted -----
  
  insert_msg('Calibration plots, response vs fitted')
  
  eli_mod$fit_plots <- list(x = eli_mod$caret_models, 
                            plot_title = names(eli_mod$caret_models) %>% 
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
  
# END -----
  
  stopImplicitCluster()
  
  insert_tail()