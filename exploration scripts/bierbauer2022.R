# Dimensionality analysis (factor analysis) an reliability of the IPQ tool 
# in Long COVID individuals from the study of Bierbauer et al. 
# (https://osf.io/bwvxg)

  insert_head()
  
# container -------
  
  lc_ipq <- list()
  
# Analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## analysis table with the 'larger' IPQ dimensions
  ## inverting the personal control, treatment control and coherence dimensions
  ## (lacking personal control, lacking treatment control, lacking coherence)
  ## scaling the identity (37, the maximal symptom number is 5, 0 is 0)
  
  lc_ipq$variables <- 
    c('IPQ consequences', 
      'IPQ duration', 
      'IPQ cyclical', 
      'IPQ lacking personal control', 
      'IPQ lacking treatment control', 
      'IPQ identity/# symptoms', 
      'IPQ lacking coherence', 
      'IPQ emotional representation')
  
  lc_ipq$analysis_tbl <- 
    read_csv('./data/data_illnessperception_bierbauer2022.csv') %>% 
    mutate(participant_id = paste0('id_', 1:nrow(.)), 
           lack_personalcontrol = 5 - personalcontrol, 
           lack_treatmentcontrol = 5 - treatmentcontrol, 
           lack_coherence = 5 - coherence) %>% 
    select(participant_id, 
           consequence, 
           duration, 
           cyclical,
           lack_personalcontrol, 
           lack_treatmentcontrol, 
           identity, 
           lack_coherence, 
           emotionalrep) %>% 
    filter(complete.cases(.)) %>% 
    mutate(identity = identity * 5/37) %>% 
    set_names(c('participant_id', 
                lc_ipq$variables)) %>% 
    column_to_rownames('participant_id')
  
# Median-centered PCA to identify the factor number ------
  
  insert_msg('PCA')
  
  ## PCA object
  
  lc_ipq$pca_obj <- reduce_data(center_data(lc_ipq$analysis_tbl), 
                                kdim = 6, 
                                red_fun = 'pca')
  
  ## scree plot: 3 - 4 factors seem to be appropriate
  
  lc_ipq$pca_scree_plot <- plot(lc_ipq$pca_obj, 
                                type = 'scree', 
                                cust_theme = globals$common_theme) +  
    expand_limits(y = 0) + 
    labs(title = 'PCA variance per component, Bierbauer et al. 2022', 
         subtitle = '6D PCA, median centering', 
         tag = paste('\nn =', nrow(lc_ipq$analysis_tbl)))
  
  ## PCA scores
  
  lc_ipq$pca_score_plot <- plot(lc_ipq$pca_obj, 
                                type = 'scores', 
                                cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'PCA scores, Bierbauer et al. 2022', 
         subtitle = '6D PCA, median centering', 
         tag = paste('\nn =', nrow(lc_ipq$analysis_tbl)))
  
  ## PCA loadings
  
  lc_ipq$pca_loadings_plot <- plot(lc_ipq$pca_obj, 
                                   type = 'loadings', 
                                   cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'PCA loadings, Bierbauer et al. 2022', 
         subtitle = '6D PCA, median centering', 
         tag = paste('\nn =', nrow(lc_ipq$analysis_tbl)))
  
# Factor analysis ------
  
  insert_msg('Factor analysis')
  
  ## no pre-processing of the data
  ## 3 dimensions as identified by the PCA scree plot
  
  lc_ipq$fa_obj <- reduce_data(center_data(lc_ipq$analysis_tbl), 
                                kdim = 4, 
                                red_fun = 'fa')
  
  ## loadings plot
  
  lc_ipq$fa_loadings_plot <- plot(lc_ipq$fa_obj, 
                                  type = 'loadings', 
                                  cust_theme = globals$common_theme) + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    labs(title = 'FA loadings, Bierbauer et al. 2022', 
         subtitle = '4D factor analysis', 
         tag = paste('\nn =', nrow(ipq_fa$analysis_tbl)))
  
  ## plots of loadings per dimension
  
  lc_ipq$fa_factor_plots <- 
    list(var = paste0('comp_', 1:4), 
         title = c('Lacking control/duration/lacking coherence', 
                   'Emotion/identity/consequences', 
                   'Lacking cohernce/emotion/cyclical', 
                   'Duration/emotion/lacking control'), 
         perc_var = var(lc_ipq$fa_obj)$perc_var) %>% 
    pmap(function(var, title, perc_var) lc_ipq$fa_obj$loadings %>% 
           ggplot(aes(x = .data[[var]], 
                      y = reorder(variable, .data[[var]]), 
                      fill = factor(sign(.data[[var]])))) + 
           geom_bar(stat = 'identity', 
                    color = 'black') + 
           scale_fill_manual(values = c('-1' = 'coral3', 
                                        '1' = 'firebrick')) + 
           guides(fill = 'none') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = title, 
                subtitle = paste0('4D factor analysis, ', 
                                  signif(perc_var, 3), 
                                  '% variance'), 
                x = 'Loadings value')) %>% 
    set_names(paste0('comp_', 1:4))
  
# Clustering of the participants by the IPQ dimensions -------
  
  insert_msg('Clustering of the participants')
  
  ## with HCL/Manhattan (best stability in CV)
  
  lc_ipq$clust_obj <-  hcluster(lc_ipq$analysis_tbl %>% 
                                  center_data('median'), 
                                distance_method = 'manhattan', 
                                k = 3)
  
  lc_ipq$clust_obj$clust_assignment <- 
    lc_ipq$clust_obj$clust_assignment %>% 
    mutate(clust_id = paste0('#', clust_id), 
           clust_id = factor(clust_id))
  
  ## variance and cross-validation
  
  lc_ipq$clust_var <- var(lc_ipq$clust_obj)
  
  lc_ipq$cv <- cv(lc_ipq$clust_obj, 
                  nfolds = 10, 
                  kNN = 7, 
                  resolve_ties = TRUE)
  
  ## diagnostic plots
  
  lc_ipq$clust_plots <- plot(lc_ipq$clust_obj, 
                             cust_theme = globals$common_theme)
  
  lc_ipq$dist_heat_map <- plot(lc_ipq$clust_obj, 
                               type = 'heat_map', 
                               cust_theme = globals$common_theme)
  
  lc_ipq$clust_umap <- plot(lc_ipq$clust_obj, 
                            type = 'components', 
                            with = 'data', 
                            red_fun = 'umap', 
                            cust_theme = globals$common_theme)
  
# Differences between the clusters in the IPQ dimensions -------
  
  insert_msg('Differences between the clusters')
  
  ## analysis table
  
  lc_ipq$clust_data <- list(lc_ipq$clust_obj %>% 
                              model.frame %>% 
                              rownames_to_column('patient_id'), 
                            lc_ipq$clust_obj$clust_assignment %>% 
                              set_names(c('patient_id', 'clust_id'))) %>% 
    reduce(left_join, by = 'patient_id') %>% 
    as_tibble
  
  ## n numbers
  
  lc_ipq$clust_n <- ngroups(lc_ipq$clust_obj)
  
  lc_ipq$clust_tag <- 
    map2_chr(lc_ipq$clust_n$clust_id, 
             lc_ipq$clust_n$n, 
             paste, sep = ', n = ') %>% 
    paste(collapse = ', ')
  
  ## descriptive stats
  
  lc_ipq$desc_stats <- 
    explore(lc_ipq$clust_data, 
            split_factor = 'clust_id', 
            variables = lc_ipq$variables, 
            what = 'table', 
            pub_styled = TRUE) %>%
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', paste0('clust_', 1:3)))
  
  ## Kruskal-Wallis tests and plot axis labels with p values
  
  lc_ipq$test_results <- 
    compare_variables(lc_ipq$clust_data, 
                      variables = lc_ipq$variables, 
                      split_factor = 'clust_id', 
                      what = 'test', 
                      types = 'kruskal_test', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    format_test_jps
  
  lc_ipq$ax_labs <- lc_ipq$test_results %>% 
    mutate(ax_lab = paste(variable, significance, 
                          sep = '\n'))

  lc_ipq$ax_labs <- set_names(lc_ipq$ax_labs$ax_lab, 
                              lc_ipq$test_results$variable)
  
  ## ribbon plot
  
  lc_ipq$ribbon_plot <- 
    draw_stat_panel(data = lc_ipq$clust_data, 
                    variables = lc_ipq$variables, 
                    split_factor = 'clust_id',
                    stat = 'mean', 
                    err_stat = '2se', 
                    cust_theme = globals$common_theme, 
                    form = 'line', 
                    plot_title = 'IP clusters, Bierbauer et al. 2022', 
                    plot_subtitle = 'mean Z-score, 2 \u00D7 SEM', 
                    alpha = 0.25, 
                    plot_tag = paste0('\n', lc_ipq$clust_tag)) + 
    scale_y_discrete(labels = lc_ipq$ax_labs) + 
    scale_fill_manual(values = c('#1' = 'coral3', 
                                 '#2' = 'darkolivegreen3', 
                                 '#3' = 'steelblue3'), 
                      name = 'IP cluster') + 
    scale_color_manual(values = c('#1' = 'coral3', 
                                  '#2' = 'darkolivegreen3', 
                                  '#3' = 'steelblue3'), 
                       name = 'IP cluster') + 
    coord_polar(theta = 'y', 
                clip = 'off') + 
    theme(axis.line = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text.y = element_blank())
  
# Correlation analysis -------  
  
  insert_msg('Correlation analysis')
  
  ## correlation of the items/Spearman
  
  lc_ipq$pairs <- combn(lc_ipq$variables, m = 2, simplify = FALSE)
  
  lc_ipq$corr_test <- lc_ipq$pairs %>% 
    map_dfr(~correlate_variables(lc_ipq$analysis_tbl, 
                                 variables = .x, 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 pub_styled = FALSE)) %>% 
    mutate(highlight = ifelse(p_value < 0.05, 'bold', 'plain'), 
           significant = ifelse(p_value < 0.05, 'p < 0.05', 'ns'), 
           significant = factor(significant, c('p < 0.05', 'ns')))
  
  ## correlogram
  
  lc_ipq$corr_plot <- lc_ipq$corr_test %>% 
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
                         limits = c(-0.5, 0.5), 
                         name = expression(rho)) + 
    scale_x_discrete(limits = lc_ipq$variables) + 
    scale_y_discrete(limits = lc_ipq$variables) + 
    globals$common_theme + 
    guides(size = 'none') + 
    theme(axis.title = element_blank(), 
          axis.text.x = element_text(hjust = 0, 
                                     vjust = 0.5, 
                                     angle = -90)) + 
    labs(title = 'Correlation analysis, IPQ, Bierbauer et al. 2022', 
         subtitle = 'Spearman correlation')
  
# END ------
  
  insert_tail()