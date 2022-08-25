# Paper and supplement figures

  insert_head()
  
# containers -----
  
  paper_fig <- list()
  suppl_fig <- list()
  
# Figure 1: study CONSORT diagram ------
  
  insert_msg('Figure 1: study consort')
  
  paper_fig$consort <- 
    plot_grid(ggdraw() + 
                draw_image('./study consort/consort_diagram.png')) %>% 
    as_figure(label = 'figure_1_consort', 
              ref_name = 'consort', 
              caption = 'Flow diagram of study enrollment and analysis inclusion process.', 
              w = 90, 
              h = 90 * 2766/1728)
  
# Figure 2: Key factors impacting on disease perception ------
  
  insert_msg('Figure 2: key factors impacting on disease perception')
  
  ## top panel: Venn plot
  
  paper_fig$key_factors$upper_panel <- 
    plot_grid(mod_plots$venn_plots$ipq_total$venn, 
              plot_grid(mod_plots$venn_plots$ipq_total$legend_grob, 
                        ggdraw() + 
                          draw_text(paste(an$cmm_variables$ipq_total %>% 
                                            translate_var(out_value = 'label_long'), 
                                          collapse = '\n'), 
                                    hjust = 0, 
                                    x = 0.1, 
                                    size = 8), 
                        ncol = 2, 
                        rel_widths = c(0.4, 0.6)), 
              ncol = 2, 
              rel_widths = c(0.5, 0.5))
  
  ## middle panel: violin plots
  
  paper_fig$key_factors$middle_panel <- 
    ipq_factors$violin_plots[c('fatigue_sympt', 
                               'respi_comorb', 
                               'hair_loss_sympt')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  ## bottom panel: correlations
  
  paper_fig$key_factors$bottom_panel <- 
    plot_grid(ipq_factors$corr_plots$Chalder_FS, 
              ncol = 3, 
              align = 'hv')

  ## entire figure
  
  paper_fig$key_factors <- plot_grid(paper_fig$key_factors$upper_panel, 
                                     ggdraw(), 
                                     paper_fig$key_factors$middle_panel, 
                                     paper_fig$key_factors$bottom_panel, 
                                     nrow = 4, 
                                     rel_heights = c(1, 0.05, 1, 1), 
                                     labels = c('A', '', 'B', 'C'), 
                                     label_size = 10) %>% 
    as_figure(label = 'figure_2_key_factors', 
              ref_name = 'key_factors', 
              caption = 'Key factors associated with disease perception one year after COVID-19.', 
              w = 180, 
              h = 220)
  
# Figure 3: illness perception heterogeneity -------
  
  insert_msg('Figure 3: individuals at risk of aberrant illness perception')
  
  ## top panel, cluster characteristic
  
  paper_fig$ipq_clusters$top_panel <- 
    plot_grid(ggdraw(), 
              ipq_clust$ft_ribbon_plot + 
                theme(plot.tag = element_blank(), 
                      legend.position = 'none'), 
              plot_grid(get_legend(ipq_clust$ft_ribbon_plot), 
                        ggdraw() + 
                          draw_text(ipq_clust$ft_ribbon_plot$labels$tag %>% 
                                      stri_replace_all(fixed = ', ', 
                                                       replacement = '\n'), 
                                    size = 8, 
                                    hjust = 0, 
                                    x = 0.1), 
                        nrow = 2), 
              ncol = 3, 
              rel_widths = c(0.1, 0.6, 0.3))
  
  ## bottom panels: symptoms and fatigue
  
  paper_fig$ipq_clusters$middle_panel <- 
    c(list(ipq_clust$comp_plots$ipq_total), 
      clust_chara$fup_plots[c('sympt_number', 
                              'Chalder_FS')]) %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          labs(x = 'IP cluster')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  paper_fig$ipq_clusters$bottom_panel <- 
    clust_chara$fup_plots[c('Chalder_FS_bimodal', 
                            'fatigue_sympt', 
                            'sleep_sympt')] %>%
    map(~.x + 
          scale_fill_manual(values = c(no = 'steelblue', 
                                       yes = 'coral3'), 
                            name = '') + 
          labs(x = 'IP cluster'))
  
  paper_fig$ipq_clusters$bottom_panel <- 
    paper_fig$ipq_clusters$bottom_panel %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    plot_grid(., 
              get_legend(paper_fig$ipq_clusters$bottom_panel[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  
  ## entire figure
  
  paper_fig$ipq_clusters <- plot_grid(paper_fig$ipq_clusters$top_panel, 
                                      paper_fig$ipq_clusters$middle_panel, 
                                      paper_fig$ipq_clusters$bottom_panel, 
                                      nrow = 3, 
                                      rel_heights = c(1.2, 0.8, 1), 
                                      labels = LETTERS, 
                                      label_size = 10) %>% 
    as_figure(label = 'figure_3_clustering', 
              ref_name = 'risk_clusters', 
              caption = 'Heterogeneity of illness perception and residual symptoms one year after COVID-19.', 
              w = 180, 
              h = 220)
  
# Supplementary Figure S1: score coherence ------
  
  insert_msg('Figure S1: score coherence')
  
  suppl_fig$coherence <- ipq_co$corr_mtx_plots$ipq_total %>% 
    as_figure(label = 'figure_s1_score_coherence',
              ref_name = 'coherence', 
              caption = 'Coherence of the illness perception score.', 
              w = 180, 
              h = 150)
  
# Supplementary Figure S2: model construction, training and CV performance -----
  
  insert_msg('Figure S2: training and CV performance')
  
  suppl_fig$perfomance <- mod_plots$perf_plots$ipq_total %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.title.position = 'plot', 
                plot.margin = ggplot2::margin(t = 8, l = 3, unit = 'mm'))) %>% 
    map2(., c(1, 1), 
         ~.x + expand_limits(x = .y)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(mod_plots$perf_plots$ipq_total[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s2_model_performance', 
              ref_name = 'performance', 
              caption = 'Multi-parameter model performance.', 
              w = 180, 
              h = 110)
  
# Supplementary Figure S3 - S5: model estimates------
  
  insert_msg('Figure S3 - S5: model estimates')
  
  suppl_fig[c('eln_est', 
              'lasso_est', 
              'blasso_est')] <- 
    map2(list(eln_mod, lasso_mod, blass_mod), 
         list(scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[ElasticNet])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[LASSO])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[BayesianLASSO]))), 
         ~.x$estimate_plots$ipq_total + pap$est_fills + .y) %>% 
    map2(., an$methods, 
         ~.x + labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', '))) %>% 
    list(x = ., 
         h = c(210, 170, 80), 
         ref_name = c('eln_est', 'lasso_est', 'blasso_est'), 
         label = c('figure_s3_elnet_estimates', 
                   'figure_s4_lasso_estimates',
                   'figure_s5_blasso_estimates'), 
         caption = c('Non-zero coefficient estimates of the Elastic Net multi-parameter model.', 
                     'Non-zero coefficient estimates of the LASSO multi-parameter model.', 
                     'Non-zero coefficient estimates of the Bayesian LASSO multi-parameter model.')) %>% 
    pmap(as_figure, 
         w = 180)
  
# Supplementary Figure S6: age, sex, IP and cardiopulmonary findings -----
  
  insert_msg('Figure S6: IP, age, sex and cardiopulmonary findings')
  
  suppl_fig$cardiopulmo <- ipq_factors$violin_plots[c('sex', 
                                                      'lufo_red', 
                                                      'ct_severity_any', 
                                                      'diastolic_dysf')] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.title.position = 'plot')) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(plot_grid(ipq_factors$corr_plots$age, 
                        ncol = 2, 
                        rel_widths = c(0.9, 1.1)), 
              nrow = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s6_cardiopulmo', 
              ref_name = 'cardiopulmo', 
              caption = 'Age, sex and cardiopulmonary abnormalities at the one-year follow-up and illness perception.', 
              w = 180, 
              h = 160)
  
# Supplementary Figure S7: choice of the clustering algorithm ------
  
  insert_msg('Figure S7: Clustering algorithm choice')
  
  suppl_fig$clust_dev$upper_panel <- 
    plot_grid(ipq_clust_dev$algo_plot + 
                theme(legend.position = 'none'), 
              plot_grid(get_legend(ipq_clust_dev$algo_plot), 
                        ggdraw() + 
                          draw_text(ipq_clust$diagnostic_plots$wss$labels$tag, 
                                    size = 8, 
                                    x = 0.1, 
                                    hjust = 0), 
                        nrow = 2), 
              ncol = 2, 
              rel_widths = c(0.8, 0.2))
  
  suppl_fig$clust_dev$bottom_panel <- 
    ipq_clust$diagnostic_plots[c('wss', 'heat_map')] %>% 
    map(~.x + 
          theme(legend.position = 'bottom', 
                plot.tag = element_blank()) + 
          labs(subtitle = stri_replace(.x$labels$subtitle, 
                                       fixed = 'euclidean', 
                                       replacement = 'Euclidean') %>% 
                 stri_replace(regex = ', ward.*', 
                              replacement = ''))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('B', 'C'), 
              label_size = 10)
  
  suppl_fig$clust_dev <- plot_grid(suppl_fig$clust_dev$upper_panel, 
                                   suppl_fig$clust_dev$bottom_panel, 
                                   nrow = 2, 
                                   labels = c('A', ''), 
                                   label_size = 10) %>% 
    as_figure(label = 'figure_s7_cluster_development', 
              ref_name = 'clust_dev', 
              caption = 'Choice of the optimal clustering algorithm and the cluster number.', 
              w = 180, 
              h = 190)
  
# Saving the figures ------
  
  insert_msg('Saving the figures on the disc')
  
  paper_fig %>% 
    walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
  suppl_fig %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()
  