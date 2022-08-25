# Venn plots with the common influnetial factors and plots with the 
# response-wise performance measures

  insert_head()
  
# container -----
  
  mod_plots <- list()
  
# Performance measures: R-squared and RMSE ------
  
  insert_msg('Performance measures')
  
  ## data
  
  mod_plots$perf_data <- list(eln_mod = eln_mod, 
                              lasso_mod = lasso_mod, 
                              blass_mod = blass_mod) %>% 
    map(~.x$cv_fit_stats) %>% 
    map(reduce, rbind) %>% 
    map(filter, statistic %in% c('RMSE', 'rsq')) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x, 
                     method = factor(.y, c(names(an$methods), 'eli_mod')), 
                     response = factor(response, an$responses)))
  
  ## plotting 
  
  mod_plots$perf_plots <- list(cv_stats =  mod_plots$perf_data %>% 
                                 dlply(.(response)), 
                               plot_subtitle = an$responses %>% 
                                 translate_var %>% 
                                 paste('1-year follow-up', sep = ', ')) %>% 
    pmap(plot_rsq_rmse,
         y_factor = 'method') %>% 
    map(~map(.x, 
             ~.x + scale_y_discrete(labels = c(an$methods, 
                                               c(eli_mod = 'Backward elimination')))))
  
# Numbers of selected factors: Venn plots -----
  
  insert_msg('Numbers of selected factors')
  
  mod_plots$venn_plots <- 
    list(data = map(an$variables, 
                    ~set_names(.x, an$methods[names(.x)])), 
         plot_title = an$responses %>% 
           translate_var %>% 
           paste('1-year follow-up', sep = ', ')) %>% 
    pmap(plot_n_venn, 
         plot_subtitle = 'variables with non-zero coefficients', 
         plot_tag = paste0('variables: n = ', 
                           length(mod$variables), 
                           ', observations: n = ', 
                           nrow(mod$multi_tbl)), 
         fill_color = set_names(an$method_colors, 
                                an$methods), 
         fontScale = 3, 
         panel = FALSE)

# END -----
  
  insert_tail()