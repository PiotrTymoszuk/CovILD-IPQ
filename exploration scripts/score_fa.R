# Factor analysis of the BIPQ items. No pre-processing.
# The number of factors (k = 3) was chosen based on the PCA results 
# (see the scree plot)

  insert_head()
  
# container -----
  
  ipq_fa <- list()
  
# globals -----
  
  insert_msg('Globals: analysis table')
  
  ipq_fa$analysis_tbl <- cov_data$clear_data %>% 
    select(ID, starts_with('ipq_q')) %>% 
    column_to_rownames('ID') %>% 
    set_names(globals$ipq_sublabs[names(.)])
  
# FA: 3 factor ------
  
  insert_msg('3D factor analysis')
  
  ## FA object
  
  ipq_fa$fa_obj <- reduce_data(ipq_fa$analysis_tbl, 
                               kdim = 3, 
                               red_fun = 'fa', 
                               scores = 'regression')
  
# Score and loadings plots --------
  
  insert_msg('Plotting the score and loadings plots')
  
  ## FA regression scores
  
  ipq_fa$score_plot <- plot(ipq_fa$fa_obj, 
                            type = 'scores', 
                            cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'FA regression scores', 
         subtitle = '3D factor analysis', 
         tag = paste('\nn =', nrow(ipq_fa$analysis_tbl)))
  
  ## loadings
  
  ipq_fa$loadings_plot <- plot(ipq_fa$fa_obj, 
                                type = 'loadings', 
                                cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'FA loadings', 
         subtitle = '3D factor analysis', 
         tag = paste('\nn =', nrow(ipq_fa$analysis_tbl)))
  
# END ------
  
  insert_tail()