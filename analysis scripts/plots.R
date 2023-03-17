# Venn plots with the common influential factors and plots with the 
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
                                 exchange(dict = globals$var_lexicon) %>% 
                                 paste('1-year follow-up', sep = ', ')) %>% 
    pmap(plot_rsq_rmse,
         y_factor = 'method') %>% 
    map(~map(.x, 
             ~.x + scale_y_discrete(labels = c(an$methods, 
                                               c(eli_mod = 'Backward elimination')))))
  
# Numbers of selected factors: Venn plots -----
  
  insert_msg('Numbers of selected factors')
  
  ## working with safely, since there are likely no factors found
  ## for the control/coherence subscore
  
  mod_plots$venn_plots <- 
    list(data = map(an$variables, 
                    ~set_names(.x, an$methods[names(.x)])), 
         plot_title = an$responses %>% 
           exchange(dict = globals$var_lexicon)) %>% 
    pmap(safely(plot_n_venn),  
         fill_color = set_names(an$method_colors, 
                                an$methods), 
         subset_names = an$methods[names(an$variables$ipq_total)], 
         fontScale = 4, 
         panel = FALSE, 
         legend_position = 'bottom', 
         comp_heights = c(0.08, 0, 0.92, 0))
  
  mod_plots$venn_plots <- mod_plots$venn_plots %>% 
    map(~.x$result)

# END -----
  
  insert_tail()