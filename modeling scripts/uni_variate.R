# Uni-variate modeling

  insert_head()
  
# container ------
  
  uni_mod <- list()
  
# model construction: normalized and non-normalized models -------
  
  insert_msg('Model construction')
  
  ## without normalization
  
  uni_mod$models <- mod$responses  %>% 
    map(function(resp) set_names(mod$variables, 
                                 mod$variables) %>% 
          map(~make_lm(data = mod$multi_tbl, 
                       response = resp, 
                       indep_variable = .x, 
                       mod_fun = lm, 
                       family = NULL)))
  
# model assumptions ------
  
  insert_msg('Model assumptions')
  
  uni_mod$assumptions <- uni_mod$models %>% 
    map(~map(.x, 
             summary, 
             type = 'assumptions', 
             type.predict = 'response') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y))) %>% 
    map2(., names(.), ~mutate(.x, response = .y))

# model fit statistics -----
  
  insert_msg('Model fit stats')
  
  uni_mod$fit_stats <- uni_mod$models %>% 
    map(~map(.x, 
             summary, 
             type = 'fit') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y))) %>% 
    map(mutate, 
        rsq_eff_size = cut(raw_rsq, 
                           c(-Inf, 0.01, 0.09, 0.25, Inf), 
                           c('none', 'weak', 'moderate', 'large')))
  
# model inference for normalized and non-normalized models ------
  
  insert_msg('Model inference')
  
  uni_mod$inference <- uni_mod$models %>% 
    map(~map(.x, 
             summary, 
             type = 'inference'))

  ## filling up with the baseline levels
  
  uni_mod$inference <- uni_mod$inference %>% 
    map(~map2_dfr(., names(.), 
                  ~mutate(.x, 
                          level_mod = levels(cov_data$clear_data[[.y]])))) %>% 
    map(mutate, 
        level_mod = ifelse(is.na(level_mod), 
                           0, level_mod))

# Plots of R squares ------
  
  insert_msg('R square value plots')
  
  uni_mod$rsq_plots <- uni_mod$fit_stats %>% 
    map2(., translate_var(names(.)), 
         ~ggplot(.x, 
                 aes(x = raw_rsq, 
                     y = reorder(variable, raw_rsq), 
                     fill = rsq_eff_size)) + 
           geom_bar(stat = 'identity', 
                    color = 'black') + 
           geom_vline(xintercept = 0.01, 
                      linetype = 'dashed') + 
           geom_vline(xintercept = 0.09, 
                      linetype = 'dashed') + 
           geom_vline(xintercept = 0.25, 
                      linetype = 'dashed') + 
           scale_y_discrete(labels = translate_var(globals$variables)) + 
           scale_x_continuous(limits = c(0, 0.45)) + 
           scale_fill_manual(values = mod$size_colors, 
                             name = 'Effect size') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = paste(.y, '1-year follow-up', sep = ', '), 
                subtitle = 'univariate linear modeling', 
                x = expression('raw R'^2)))
  
# Forest plots with significant factors, normalized estimates ------
  
  insert_msg('Forest plots')
  
  ## variables of interest
  
  uni_mod$signif_factors <- uni_mod$inference %>% 
    map(filter,
        parameter != '(Intercept)', 
        p_value < 0.05) %>% 
    map(~unique(.x$variable))
  
  ## plotting tables
  
  uni_mod$forest_results <- map2(uni_mod$inference, 
                                 uni_mod$signif_factors, 
                                 ~filter(.x, 
                                         variable %in% .y, 
                                         parameter != '(Intercept)')) %>% 
    map(mutate, 
        variable = translate_var(variable))
  
  ## Forest plots
  
  uni_mod$forest_plots <- 
    list(x =  uni_mod$forest_results,
         plot_title = names(uni_mod$forest_results) %>% 
           translate_var %>% 
           paste('1-year follow-up', sep = ', ')) %>% 
    pmap(plot_forest, 
         p_value = 'p_value', 
         plot_subtitle = paste('univariate linear modeling, total n =', 
                               nrow(mod$multi_tbl)), 
         x_lab = expression('normalized ' * beta * ' \u00B1 95% CI'), 
         hide_baseline = TRUE, 
         cutpoint = 0, 
         cust_theme = globals$common_theme)
  
  ## hiding the yes/no level labels for binary explanatory variables

  for(i in names(uni_mod$forest_plots)) {
    
    uni_mod$forest_plots[[i]]$data <- uni_mod$forest_plots[[i]]$data %>% 
      mutate(y_ax = stri_replace(y_ax, fixed = ': yes', replacement = ''))
    
  }
  
# END -----
  
  rm(i)
  
  insert_tail()