# Clustering by the IPQ items

  insert_head()

# container -----

  ipq_clust_dev <- list()

# globals ------
  
  insert_msg('Globals setup')
  
  ## analysis table
  
  ipq_clust_dev$analysis_tbl <- 
    cov_data$clear_data[c('ID', paste0('ipq_q', 1:8))] %>% 
    column_to_rownames('ID')
  
  ## distances
  
  ipq_clust_dev$dist <- c('euclidean', 'manhattan')
  
# clustering algorithms ------
  
  insert_msg('Clustering algorithms')
  
  ipq_clust_dev$algos[paste0('hcl_', ipq_clust_dev$dist)] <- 
    ipq_clust_dev$dist %>% 
    map(~hcluster(ipq_clust_dev$analysis_tbl, 
                  distance_method = .x, 
                  k = 3, 
                  hc_method = 'ward.D2', 
                  seed = 1234))
  
  ipq_clust_dev$algos[paste0('kmeans_', ipq_clust_dev$dist)] <- 
    ipq_clust_dev$dist %>% 
    map(~kcluster(ipq_clust_dev$analysis_tbl, 
                  distance_method = .x, 
                  k = 3, 
                  clust_fun = 'kmeans', 
                  seed = 1234))
  
  ipq_clust_dev$algos[paste0('pam_', ipq_clust_dev$dist)] <- 
    ipq_clust_dev$dist %>% 
    map(~kcluster(ipq_clust_dev$analysis_tbl, 
                  distance_method = .x, 
                  k = 3, 
                  clust_fun = 'pam', 
                  seed = 1234))
  
# comparing the clustering variances ------
  
  insert_msg('Clustering variances')
  
  ipq_clust_dev$variance <- ipq_clust_dev$algos %>% 
    map(clustTools::var) %>% 
    map_dbl(~.x$frac_var) %>% 
    tibble(method = names(.), 
           frac_var = .)
  
# cross-validation -----
  
  insert_msg('Cross-validation')
  
  ipq_clust_dev$cv <- ipq_clust_dev$algos %>% 
    map(cv, 
        nfolds = 10, 
        kNN = 7, 
        resolve_ties = TRUE) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x$summary, method = .y))
  
# common comparison results, plotting ----
  
  insert_msg('Comparison results')
  
  ## data
  
  ipq_clust_dev$algo_test <- 
    left_join(ipq_clust_dev$variance, 
              ipq_clust_dev$cv, 
              by = 'method') %>% 
    mutate(cv_accuracy = 1 - mean_error, 
           method_lab = paste(toupper(stri_extract(method, 
                                                   regex = 'hcl|kmeans|pam')), 
                              stri_trans_totitle(stri_extract(method, 
                                                              regex = paste(ipq_clust_dev$dist, 
                                                                            collapse = '|')))))
  
  ## bar plot
  
  ipq_clust_dev$algo_plot <- ipq_clust_dev$algo_test %>% 
    pivot_longer(cols = c(frac_var, cv_accuracy), 
                 names_to = 'statistic', 
                 values_to = 'value') %>% 
    ggplot(aes(x = value, 
               y = reorder(method_lab, value), 
               fill = statistic)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    geom_text(aes(label = signif(value, 2), 
                  color = statistic), 
              size = 2.75, 
              hjust = -0.5, 
              position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(cv_accuracy = 'indianred4', 
                                 frac_var = 'steelblue'), 
                      labels = c(cv_accuracy = 'CV accuracy', 
                                 frac_var = 'clustering variance'), 
                      name = '') + 
    scale_color_manual(values = c(cv_accuracy = 'indianred4', 
                                  frac_var = 'steelblue'), 
                       labels = c(cv_accuracy = 'CV accuracy', 
                                  frac_var = 'clustering variance'), 
                       name = '') + 
  expand_limits(x = 1) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Performance of clustering algorithms', 
         subtitle = 'Sum of square ratio and 10-fold cross-validation', 
         x = 'statistic value')
  
# END ------
  
  insert_tail()