# Correlation of the IPQ items, sum and subscores with other psy variables

  insert_head()
  
# container -----
  
  psy_cor <- list()
  
# globals ------
  
  insert_msg('Globals: variables')
  
  psy_cor$var1 <- 
    expl$variables$ipq[!expl$variables$ipq %in% c('ipq_sub1', 'ipq_sub2')]
  
  psy_cor$var2 <- globals$psy_variables
  
  psy_cor$pairs <- psy_cor$var1 %>% 
    map(function(var1) map(psy_cor$var2, ~c(var1, .x))) %>% 
    unlist(recursive = FALSE)
  
  psy_cor$pair_names <- psy_cor$pairs %>% 
    map_chr(paste, collapse = '_')
  
  psy_cor$pairs <- set_names(psy_cor$pairs, 
                             psy_cor$pair_names)
  
# serial correlation, Spearman test -----
  
  insert_msg('Spearman correlation')
  
  psy_cor$corr_results <- psy_cor$pairs %>% 
    map_dfr(~correlate_variables(cov_data$clear_data[.x] %>% 
                                   filter(complete.cases(.)), 
                                 variables = .x, 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 ci = TRUE)) %>% 
    mutate(highlight = ifelse(p_value < 0.05, 'bold', 'plain'), 
           significant = ifelse(p_value < 0.05, 'p < 0.05', 'ns'), 
           significant = factor(significant, c('p < 0.05', 'ns')))
  
# Plotting the correlation matrix -------
  
  insert_msg('Plotting of the correlation matrix')
  
  psy_cor$corr_mtx_plot <- psy_cor$corr_results %>% 
    mutate(variable1 = ifelse(variable1 %in% names(globals$ipq_sublabs), 
                              globals$ipq_sublabs[variable1], 
                              translate_var(variable1))) %>% 
    ggplot(aes(x = variable1, 
               y = variable2, 
               size = abs(estimate), 
               fill = estimate)) + 
    geom_point(shape = 21, 
               color = 'black') + 
    geom_text(aes(fontface = highlight, 
                  label = signif(estimate, 2), 
                  color = significant), 
              size = 2.75, 
              vjust = -1.5) + 
    scale_y_discrete(limits = psy_cor$var2, 
                     labels = translate_var(psy_cor$var2)) + 
    scale_size_area(max_size = 5, 
                    limits = c(0, 1), 
                    name = expression('abs(' * rho * ')')) + 
    scale_color_manual(values = c('p < 0.05' = 'black', 
                                  'ns' = 'gray60'), 
                       name = 'Significance') + 
    scale_fill_gradient2(low = 'steelblue', 
                         mid = 'white', 
                         high = 'coral3', 
                         midpoint = 0, 
                         limits = c(-1, 1), 
                         name = expression(rho)) + 
    globals$common_theme + 
    theme(axis.title = element_blank(), 
          axis.text.x = element_text(hjust = 0, 
                                     vjust = 0.5, 
                                     angle = -90)) + 
    labs(title = 'Correlation of psych varaibles', 
         subtitle = 'Spearman correlation, significant effects labeled in bold', 
         tag = 'n = 97')
  
# END ------
  
  insert_tail()