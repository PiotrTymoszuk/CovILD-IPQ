# IPQ score coherence with MdDonald's omega.
#
# The choice of the consistency statistic was motivated by multi-dimensionality 
# of the BIPQ tool with two independent components identified by factor analysis

  insert_head()
  
# container ------
  
  ipq_co <- list()
  
# globals ------
  
  insert_msg('Analysis tables and variable pairs')
  
  ipq_co$analysis_tbl <- list(ipq_total = paste0('ipq_q', 1:8), 
                              ipq_sub1 = paste0('ipq_q', c(1, 2, 5, 6, 8)), 
                              ipq_sub2 = paste0('ipq_q', c(3, 4, 7))) %>% 
    map(~select(cov_data$clear_data, 
                ID, 
                all_of(.x))) %>%
    map(~filter(.x, complete.cases(.x))) %>% 
    map(select, - ID)
  
  ipq_co$pairs <- ipq_co$analysis_tbl %>%
    map(names) %>% 
    map(~combn(.x, 2, simplify = FALSE))
  
# pair-wise correlations of the sub-scores, Spearman test ------
  
  insert_msg('Pair-wise correlation coefs')
  
  ipq_co$corr_results <- 
    list(data = ipq_co$analysis_tbl, 
         var_lst = ipq_co$pairs) %>% 
    pmap(function(data, var_lst) map_dfr(var_lst, 
                                         ~correlate_variables(data, 
                                                              variables = .x, 
                                                              type = 'spearman', 
                                                              ci = TRUE, 
                                                              pub_styled = TRUE))) %>% 
    map(mutate, 
        eff_size = stri_replace(eff_size, fixed = 'rho', replacement = '\u03C1'), 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# single correlation plots ------
  
  insert_msg('Single correlation plots')
  
  ipq_co$corr_plots <- 
    list(data = ipq_co$analysis_tbl, 
         var_lst = ipq_co$pairs, 
         pt_colors = globals$ipq_colors, 
         stats = ipq_co$corr_results) %>% 
    pmap(function(data, var_lst, pt_colors, stats) map2(set_names(var_lst, 
                                                                  map(var_lst, paste, collapse = '_')), 
                                                        stats$plot_cap,  
                                                        ~plot_correlation(data = data, 
                                                                          variables = .x, 
                                                                          type = 'correlation', 
                                                                          point_color = pt_colors, 
                                                                          cust_theme = globals$common_theme, 
                                                                          plot_title = paste(globals$ipq_sublabs[.x[1]], 
                                                                                             globals$ipq_sublabs[.x[2]], 
                                                                                             sep = ', '), 
                                                                          plot_subtitle = paste0(.y, 
                                                                                                 ', n = ', 
                                                                                                 nrow(data)), 
                                                                          x_lab = globals$ipq_sublabs[.x[1]], 
                                                                          y_lab = globals$ipq_sublabs[.x[2]], 
                                                                          show_trend = FALSE) + 
                                                          geom_smooth(method = 'gam')))
  
  
# calculation of the Cronbach's alpha -----
  
  insert_msg('Alpha calculation')
  
  ipq_co$alpha_obj <- ipq_co$analysis_tbl %>% 
    map(psych::alpha, 
        n.iter = 1000)
  
  ipq_co$alpha_stats <- ipq_co$alpha_obj %>% 
    map(~mutate(.x$total, 
                lower_ci = .x$boot.ci[1], 
                upper_ci = .x$boot.ci[3], 
                plot_cap = paste0('\u03B1 = ', 
                                  signif(raw_alpha, 2), 
                                  ' [', 
                                  signif(lower_ci, 2), 
                                  ' - ', 
                                  signif(upper_ci, 2), 
                                  ']'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y)) %>% 
    as_tibble
  
# Calculation of the omegas -------
  
  insert_msg('Calculation of the omegas')
  
  ipq_co$omega_obj <- 
    psych::omega(ipq_co$analysis_tbl$ipq_total, 
                 nfactors = 2, 
                 fm = 'ml', 
                 title = 'BIPQ', 
                 option = 'equal', 
                 niter = 1000)

# visualization of the correlation matrix ------
  
  insert_msg('Plotting the correlation matrix')
  
  ## plotting data
  
  ipq_co$corr_data <- 
    list(data = ipq_co$analysis_tbl, 
         var_lst = ipq_co$pairs) %>% 
    pmap(function(data, var_lst) map_dfr(var_lst, 
                                         ~correlate_variables(data, 
                                                              variables = .x, 
                                                              type = 'spearman', 
                                                              ci = TRUE, 
                                                              pub_styled = FALSE))) %>% 
    map(select, variable1, variable2, estimate, p_value) %>% 
    map(mutate, 
        highlight = ifelse(p_value < 0.05, 'bold', 'plain'), 
        significant = ifelse(p_value < 0.05, 'p < 0.05', 'ns'), 
        significant = factor(significant, c('p < 0.05', 'ns')))
  
  ## plots
  
  ipq_co$corr_mtx_plots <-
    map2(ipq_co$corr_data, 
         map(ipq_co$analysis_tbl, names), 
         ~ggplot(.x, 
                 aes(x = variable1, 
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
           scale_x_discrete(limits = .y, 
                            labels = globals$ipq_sublabs) + 
           scale_y_discrete(limits = .y, 
                            labels = globals$ipq_sublabs) + 
           globals$common_theme + 
           theme(axis.title = element_blank(), 
                 axis.text.x = element_text(hjust = 0, 
                                            vjust = 0.5, 
                                            angle = -90)))
  
  ## appending the plots with titles n numbers and omegas
  
  ipq_co$corr_mtx_plots <- 
    list(x = ipq_co$corr_mtx_plots, 
         y = exchange(names(ipq_co$corr_mtx_plots),
                      dict = globals$var_lexicon), 
         z = paste0('\u03C9 = ', 
                    signif(ipq_co$omega_obj$omega.group$total, 2), 
                    ', n = ', 
                    map_dbl(ipq_co$analysis_tbl, nrow))) %>% 
    pmap(function(x, y, z) x + 
           labs(title = y, 
                subtitle = z))
  
# A common summary plot with the faceting by the BIPQ components ------
  
  insert_msg('A common bubble plot')
  
  ipq_co$summ_omega_lab <- 
    map2_chr(c('total (Q1 - Q8)', 
               'emotion/concern/consequences (Q1/2/3/6/8)', 
               'lacking control/coherence (Q3/Q4/Q7)'), 
             signif(ipq_co$omega_obj$omega.group$total, 2), 
             paste, sep = ': \u03C9 = ') %>% 
    paste(collapse = '\n')
  
  ipq_co$corr_summ_plot <- ipq_co$corr_mtx_plots$ipq_total + 
    scale_x_discrete(limits = c('ipq_q1', 'ipq_q2', 
                                'ipq_q5', 'ipq_q6', 
                                'ipq_q8', 
                                'ipq_q3', 'ipq_q4', 'ipq_q7') , 
                     labels = globals$ipq_sublabs) + 
    scale_y_discrete(limits = c('ipq_q1', 'ipq_q2', 
                                'ipq_q5', 'ipq_q6', 
                                'ipq_q8', 
                                'ipq_q3', 'ipq_q4', 'ipq_q7') , 
                     labels = globals$ipq_sublabs) + 
    labs(title = 'BIPQ tool', 
         subtitle = ipq_co$summ_omega_lab)
  
# END ------
  
  insert_tail()