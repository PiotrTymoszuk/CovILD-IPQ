# Principal component analysis of the IPQ sub-scores. Median centering

  insert_head()
  
# container -----
  
  ipq_pca <- list()
  
# globals -----
  
  insert_msg('Globals: analysis table')
  
  ipq_pca$analysis_tbl <- cov_data$clear_data %>% 
    select(ID, starts_with('ipq_q')) %>% 
    column_to_rownames('ID') %>% 
    center_data(type = 'median', 
                complete_cases = TRUE) %>% 
    set_names(globals$ipq_sublabs[names(.)])
  
# PCA: 4 dimensions ------
  
  insert_msg('4D PCA')
  
  ## PCA object
  
  ipq_pca$pca_obj <- reduce_data(ipq_pca$analysis_tbl, 
                                 kdim = 4, 
                                 red_fun = 'pca')
  
# PCA plots ------
  
  insert_msg('Base PCA plots')

  ## scree plot
  
  ipq_pca$scree_plot <- plot(ipq_pca$pca_obj, 
                             type = 'scree', 
                             cust_theme = globals$common_theme) + 
    expand_limits(y = 0) + 
    labs(title = 'PCA variance per component', 
         subtitle = '4D PCA, median centering', 
         tag = paste('\nn =', nrow(ipq_pca$analysis_tbl)))
  
  ## PCA scores
  
  ipq_pca$score_plot <- plot(ipq_pca$pca_obj, 
                             type = 'scores', 
                             cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'PCA scores', 
         subtitle = '4D PCA, median centering', 
         tag = paste('\nn =', nrow(ipq_pca$analysis_tbl)))
  
  ## loadings
  
  ipq_pca$loadings_plot <- plot(ipq_pca$pca_obj, 
                                type = 'loadings', 
                                cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'PCA loadings', 
         subtitle = '4D PCA, median centering', 
         tag = paste('\nn =', nrow(ipq_pca$analysis_tbl)))
  
# Plotting the eigenvectors for each component ------
  
  insert_msg('Eigenvector plots')
  
  ipq_pca$eigen_plots[c('PC1', 'PC2', 'PC3','PC4')] <- 
    list(x = c('comp_1', 'comp_2', 'comp_3', 'comp_4'), 
         y = c(globals$ipq_colors[2:3], 
               c('gray60', 'plum4')), 
         z = c('PC1: control, emotion', 
               'PC2: control', 
               'PC3: coherence', 
               'PC4: emotion')) %>% 
    pmap(function(x, y, z) ggplot(ipq_pca$pca_obj$loadings, 
                                  aes(x = .data[[x]], 
                                      y = reorder(variable, .data[[x]]))) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = y) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = z, 
                subtitle = '4D PCA, median centering', 
                tag = paste('\nn =', nrow(ipq_pca$analysis_tbl)), 
                x = 'Eigenvector'))
  
# END ------
  
  insert_tail()

  
  
  