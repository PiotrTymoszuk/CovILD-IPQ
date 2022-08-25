# Clustering of the study participants in respect to the IPQ-influencing factors
# the algorithm: chosen by comparing variances and CV errors

  insert_head()
  
# container -----
  
  ipq_clust <- list()
  
# globals: variables, analysis table and the best algorithm -----
  
  insert_msg('Globals setup')
  
  ## clustering object, renaming
  
  ipq_clust$clust_obj <- ipq_clust_dev$algos$pam_euclidean
  
  ipq_clust$clust_obj$clust_assignment <- 
    ipq_clust$clust_obj$clust_assignment %>% 
    mutate(clust_id = car::recode(clust_id, "'1' = '#2'; '2' = '#1'; '3' = '#3'"), 
           clust_id = factor(clust_id))

  ## clustering variables
  
  ipq_clust$clust_variables <- paste0('ipq_q', 1:8)

  ipq_clust$clust_lexicon <- 
    tibble(variable = c(ipq_clust$clust_variables, 
                        'ipq_total', 'ipq_sub1', 'ipq_sub2')) %>% 
    mutate(numeric = map_lgl(variable,  
                             ~is.numeric(cov_data$clear_data[[.x]])), 
           test_type = ifelse(numeric, 'kruskal_test', 'chisq_test'), 
           plot_type = ifelse(numeric, 'violin', 'stack'))
  
  ipq_clust[c('fct_vars', 'num_vars')] <- dlply(ipq_clust$clust_lexicon, 
                                                'numeric', 
                                                function(x) x$variable)
  
  ## analysis table with the clustering variables, the total IPQ
  ## and sub-scores
  
  ipq_clust$analysis_tbl <- 
    cov_data$clear_data[c('ID', ipq_clust$clust_variables)] %>% 
    right_join(ipq_clust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    left_join(cov_data$clear_data[c('ID', 'ipq_total', 'ipq_sub1', 'ipq_sub2')], 
              by = 'ID') %>% 
    as_tibble
  
  ## plotting order for heat maps and radial plots: by p value
  
  ipq_clust$plot_order <- c('ipq_q3', 
                            'ipq_q4', 
                            'ipq_q7', 
                            'ipq_q1', 
                            'ipq_q2', 
                            'ipq_q5', 
                            'ipq_q6', 
                            'ipq_q8')
  
# characteristic of the cluster structure ------
  
  insert_msg('Characteristic of the cluster structure')
  
  ## diagnostic plots
  
  ipq_clust$diagnostic_plots <- plot(ipq_clust$clust_obj, 
                                     cust_theme = globals$common_theme)
  
  ipq_clust$diagnostic_plots$heat_map <- 
    plot(ipq_clust$clust_obj, 
         type = 'heat_map', 
         cust_theme = globals$common_theme) + 
    theme(axis.text = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks = element_blank())
  
  ipq_clust$diagnostic_plots$dist_mds <- 
    plot(ipq_clust$clust_obj, 
         type = 'component',
         with = 'distance', 
         red_fun = 'mds', 
         cust_theme = globals$common_theme) + 
    scale_fill_manual(values = globals$clust_colors)
  
  ipq_clust$diagnostic_plots$umap <- 
    plot(ipq_clust$clust_obj, 
         type = 'component',
         with = 'data', 
         red_fun = 'umap', 
         kdim = 2, 
         cust_theme = globals$common_theme, 
         random_state = 123) + 
    scale_fill_manual(values = globals$clust_colors)
  
# Levels of clustering variables and IPQ score in the clusters ------
  
  insert_msg('Clustering features and IPQ in the clusters')
  
  ## descriptive stats
  
  ipq_clust$desc_stats <- 
    explore(ipq_clust$analysis_tbl, 
            split_factor = 'clust_id', 
            variables = ipq_clust$clust_lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 
                paste0('Cluster #', 
                       1:length(levels(ipq_clust$clust_obj$clust_assignment$clust_id)))))
  
  ## testing
  
  ipq_clust$test_results <- 
    compare_variables(ipq_clust$analysis_tbl, 
                      split_factor = 'clust_id', 
                      variables = ipq_clust$clust_lexicon$variable, 
                      what = 'test',
                      types = ipq_clust$clust_lexicon$test_type, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

  ## plot labels for the ribbon plot
  
  ipq_clust$ribbon_labs <- ipq_clust$test_results %>% 
    mutate(plot_lab = ifelse(variable %in% ipq_clust$clust_variables, 
                             globals$ipq_sublabs[variable], 
                             translate_var(variable)), 
           plot_lab = stri_replace(plot_lab, fixed = ' ', replacement = '\n'), 
           plot_lab = paste(plot_lab, significance, sep = '\n'))
  
  ipq_clust$ribbon_labs <- set_names(ipq_clust$ribbon_labs$plot_lab, 
                                     ipq_clust$ribbon_labs$variable)
  
  
  ## plots 
  
  ipq_clust$comp_plots <- list(variable = ipq_clust$clust_lexicon$variable, 
                               type = ipq_clust$clust_lexicon$plot_type, 
                               plot_title = translate_var(ipq_clust$clust_lexicon$variable, 
                                                          out_value = 'label_long'), 
                               plot_subtitle = ipq_clust$test_results$plot_cap, 
                               y_lab = translate_var(ipq_clust$clust_lexicon$variable, 
                                                     out_value = 'axis_lab')) %>% 
    pmap(plot_variable, 
         ipq_clust$analysis_tbl, 
         split_factor = 'clust_id', 
         x_lab = 'cluster', 
         scale = 'percent', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                 paste0('\n', .))) %>% 
    set_names(ipq_clust$clust_lexicon$variable)
  
  ## manual adjustment
  
  ipq_clust$comp_plots[ipq_clust$fct_vars] <- 
    ipq_clust$comp_plots[ipq_clust$fct_vars] %>% 
    map(~.x + scale_fill_brewer())
  
  ipq_clust$comp_plots[ipq_clust$num_vars] <- 
    ipq_clust$comp_plots[ipq_clust$num_vars] %>% 
    map(~.x + scale_fill_manual(values = globals$clust_colors))

# Clustering feature heat map ------
  
  insert_msg('Clustering feature heat map')
  
  ipq_clust$ft_heat_map <- 
    plot_clust_hm(ipq_clust$clust_obj, 
                  plot_title = 'Clustering by IPQ items', 
                  cust_theme = globals$common_theme, 
                  x_lab = 'participant', 
                  fill_lab = 'IPQ item score') + 
    scale_y_discrete(limits = rev(ipq_clust$plot_order), 
                     labels = globals$ipq_sublabs)
  
  ipq_clust$ft_heat_map <-
    ipq_clust$ft_heat_map + 
    labs(tag = paste0('\n', ipq_clust$ft_heat_map$labels$tag))

# Ribbon plot -----
  
  insert_msg('Ribbon plot')
  
  ipq_clust$ft_ribbon_plot <- ipq_clust$clust_obj %>% 
    model.frame %>% 
    rownames_to_column('ID') %>% 
    left_join(ipq_clust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    draw_stat_panel(variables = ipq_clust$clust_variables, 
                    split_factor = 'clust_id', 
                    stat = 'mean', 
                    err_stat = '2se', 
                    form = 'line', 
                    alpha = 0.25, 
                    plot_title = 'Clustering factor levels', 
                    plot_subtitle = 'Mean \u00B1 2\u00D7SEM', 
                    plot_tag = ipq_clust$ft_heat_map$labels$tag, 
                    cust_theme = globals$common_theme) + 
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    scale_color_manual(values = globals$clust_colors, 
                      name = 'Cluster') + 
    scale_fill_manual(values = globals$clust_colors, 
                       name = 'Cluster') + 
    scale_y_discrete(limits = rev(ipq_clust$plot_order), 
                     labels = ipq_clust$ribbon_labs) + 
    theme(axis.title = element_blank(), 
          axis.line = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.y = element_blank(), 
          axis.text.x = element_text()) + 
    coord_polar(theta = 'y', clip = 'off')
  
  for(i in seq(0, 10, by = 2)) {
    
    ipq_clust$ft_ribbon_plot <- ipq_clust$ft_ribbon_plot + 
      annotate('text', 
               label = i, 
               y = 0.5, 
               x = i, 
               color = 'gray60', 
               size = 2.75)
    
  }
  
# END -----
  
  rm(i)
  
  insert_tail()