# Supplementary figures for the manuscript and figures for the Reviewers

  insert_head()

# container ------

  suppl_fig <- list()
  rev_fig <- list()
  
# Figure S1: frequency of symptoms in the severity strata ------
  
  insert_msg('Figure S1: frequency of symptoms in the severity strata')
  
  ## left panel
  
  suppl_fig$sev_symptoms$left <- 
    plot_grid(sympt_kinet$plots$all + 
                theme(legend.position = 'none'), 
              get_legend(sympt_kinet$plots$all), 
              nrow = 3)
  
  ## right panel: symptom frequencies in the severity strata
  
  suppl_fig$sev_symptoms$right <-
    plot_grid(cohort$symptom_plot + 
                theme(legend.position = 'none', 
                      strip.text = element_text(size = 7)), 
              plot_grid(get_legend(cohort$symptom_plot), 
                        ggdraw() + 
                          draw_text(paste0('ambulatory: n = ', 
                                           table(cov_data$clear_data$cat_WHO)[1], 
                                           '\nmoderate: n = ', 
                                           table(cov_data$clear_data$cat_WHO)[2], 
                                           '\nsevere: n = ', 
                                           table(cov_data$clear_data$cat_WHO)[3]), 
                                    size = 8, 
                                    hjust = 0,
                                    vjust = 0.7, 
                                    x = 0.1), 
                        ncol = 2), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## the entire figure
  
  suppl_fig$sev_symptoms <- 
    plot_grid(suppl_fig$sev_symptoms$left, 
              suppl_fig$sev_symptoms$right, 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10)
  
  suppl_fig$sev_symptoms <- suppl_fig$sev_symptoms %>% 
    as_figure(label = 'figure_s1_severity_symptoms', 
              ref_name = 'sev_symptoms', 
              caption = paste('Kinetic of persistent symptoms and', 
                              'frequency of persistent symptoms', 
                              'in the COVID-19 severity strata at the', 
                              'one-year follow-up.'), 
              w = 180, 
              h = 220)
  
# Figure S2: overlap between sequelae -------
  
  insert_msg('Figure S2: sequelae overlap')

  suppl_fig$overlap <- 
    plot_grid(fup_overlap$nvenn_plot) %>% 
    as_figure(label = 'figure_s2_sequelae_overlap', 
              ref_name = 'overlap', 
              caption = paste('Overlap between persistent symptoms and', 
                              'cardiopulmonary findings at the one-year', 
                              'follow-up.'), 
              w = 180, 
              h = 120)
  
# Figure S3: factor analysis of the BIPQ -------
  
  insert_msg('Figure S3: BIPQ factor analysis')
  
  suppl_fig$factor_analysis <- 
    plot_grid(ipq_fa$loadings_plot + 
                labs(title = 'BIPQ item loadings', 
                     subtitle = paste(ipq_fa$loadings_plot$labels$subtitle, 
                                      ', n = ', nrow(ipq_fa$analysis_tbl))) + 
                theme(plot.tag = element_blank())) %>% 
    as_figure(label = 'figure_s3_factor_analysis', 
              ref_name = 'factor_analysis', 
              caption = paste('Factor analysis of the BIPQ tool items.'), 
              w = 120, 
              h = 100)
  
# Figure S4: consistency of the BIPQ -------
  
  insert_msg('Figure S4: BIPQ consistency')
  
  suppl_fig$coherence <- plot_grid(ipq_co$corr_summ_plot + 
                                     guides(alpha = 'none', 
                                            color = 'none')) %>% 
    as_figure(label = 'figure_s4_score_coherence',
              ref_name = 'coherence', 
              caption = 'Coherence of the illness perception score.', 
              w = 180, 
              h = 150)
  
# Figure S5: model construction, training and CV performance -----
  
  insert_msg('Figure S5: training and CV performance')
  
  suppl_fig$perfomance <- mod_plots$perf_plots[c('ipq_total', 
                                                 'ipq_sub1', 
                                                 'ipq_sub2')] %>% 
    map2(c('Total IP score', 
           'Emotion, concern, consequences', 
           'Lacking control, coherence'), 
         function(plot, title) map2(plot, list(c(0, 0.8), c(0, 1.2)),  
                                   ~.x + 
                                     scale_x_continuous(limits = .y) + 
                                     labs(title = title))) %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + 
          scale_y_discrete(labels = c(eln_mod = 'ElasticNet', 
                                      lasso_mod = 'LASSO', 
                                      blass_mod = 'Bayesian\nLASSO')) + 
          theme(legend.position = 'none', 
                plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2,
              align = 'hv', 
              labels = c('A', '', 
                         'B', '', 
                         'C', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(mod_plots$perf_plots$ipq_total[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05)) %>% 
    as_figure(label = 'figure_s5_model_performance', 
              ref_name = 'performance', 
              caption = paste('Performance and reproducibility of', 
                              'regularized multi-parameter models.'), 
              w = 180, 
              h = 210)
  
# Figures S6 - S11: model estimates -------
  
  insert_msg('Figures S6 - S11: model estimates')
  
  ## plot lists
  
  suppl_fig[c('eln_est_total', 
              'lasso_est_total', 
              'blasso_est_total')] <- 
    map2(list(eln_mod, lasso_mod, blass_mod), 
         list(scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[ElasticNet])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[LASSO])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[BayesianLASSO]))), 
         ~.x$estimate_plots$ipq_total + pap$est_fills + .y)
  
  suppl_fig[c('eln_est_sub1', 
              'lasso_est_sub1', 
              'blasso_est_sub1')] <- 
    map2(list(eln_mod, lasso_mod, blass_mod), 
         list(scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[ElasticNet])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[LASSO])), 
              scale_x_continuous(limits = pap$est_limits, 
                                 name = expression('normalized ' * beta[BayesianLASSO]))), 
         ~.x$estimate_plots$ipq_sub1 + pap$est_fills + .y)
  
  ## construction of the figures
  
  suppl_fig[c('eln_est_total', 
              'lasso_est_total', 
              'blasso_est_total', 
              'eln_est_sub1', 
              'lasso_est_sub1', 
              'blasso_est_sub1')] <- suppl_fig[c('eln_est_total', 
                                                 'lasso_est_total', 
                                                 'blasso_est_total', 
                                                 'eln_est_sub1', 
                                                 'lasso_est_sub1', 
                                                 'blasso_est_sub1')] %>% 
    map2(., rep(an$methods, 2), 
         ~.x + labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', '))) %>% 
    list(x = ., 
         h = c(120, 80, 90, 210, 210, 150), 
         ref_name = c('eln_est_total', 
                     'lasso_est_total', 
                     'blasso_est_total', 
                     'eln_est_sub1', 
                     'lasso_est_sub1', 
                     'blasso_est_sub1'), 
         label = c('figure_s6_elnet_estimates_total', 
                   'figure_s7_lasso_estimates_total',
                   'figure_s8_blasso_estimates_total', 
                   'figure_s9_elnet_estimates_sub1', 
                   'figure_s10_lasso_estimates_sub1',
                   'figure_s11_blasso_estimates_sub1'), 
         caption = c(paste('Non-zero coefficient estimates of the', 
                            'Elastic Net multi-parameter model of the', 
                            'total illness perception score at the', 
                            'one-year follow-up.'), 
                     paste('Non-zero coefficient estimates of the', 
                           'LASSO multi-parameter model of the', 
                           'total illness perception score at the', 
                           'one-year follow-up.'), 
                     paste('Non-zero coefficient estimates of the', 
                           'Bayesian LASSO multi-parameter model of the', 
                           'total illness perception score at the', 
                           'one-year follow-up.'), 
                     paste('Non-zero coefficient estimates of the', 
                           'Elastic Net multi-parameter model of the', 
                           'emotion/concern/consequences illness perception', 
                           'component at the one-year follow-up.'), 
                     paste('Non-zero coefficient estimates of the', 
                           'LASSO multi-parameter model of the', 
                           'emotion/concern/consequences illness perception', 
                           'component at the one-year follow-up.'), 
                     paste('Non-zero coefficient estimates of the', 
                           'Bayesian LASSO multi-parameter model of the', 
                           'emotion/concern/consequences illness perception', 
                           'component at the one-year follow-up.'))) %>% 
    pmap(as_figure, 
         w = 180)
  
# Figure S12: effects of age, sex and cardiopulomnary abnormalities ------
  
  insert_msg('Figure S12: age, sex and cardiopulmonary findings, total score')
  
  suppl_fig$cardiopulmo_total <- 
    c(ipq_factors$corr_plots['age'], 
      ipq_factors$violin_plots[c('sex', 
                                 'lufo_red', 
                                 'ct_severity_any', 
                                 'diastolic_dysf')]) %>% 
    map(~.x + 
          labs(y = 'Total IP score') + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    as_figure(label = 'figure_s12_cardiopulmo_total', 
              ref_name = 'cardiopulmo_total', 
              caption = paste('Age, sex, cardiopulmonary abnormalities', 
                              'and persistent symptoms at the one-year', 
                              'follow-up and the total illness', 
                              'perception score.'), 
              w = 180, 
              h = 120)
  
# Figure S13: effects of age, sex and cardiopulomnary abnormalities, subscores ------
  
  insert_msg('Figure S13: age, sex and cardiopulmonary findings, subscores')
  
  suppl_fig$cardiopulmo_sub <- 
    c(sub1_factors$corr_plots['age'], 
      sub1_factors$violin_plots[c('sex', 
                                  'lufo_red', 
                                  'ct_severity_any', 
                                  'diastolic_dysf')], 
      list(ggdraw()), 
      sub2_factors$corr_plots['age'], 
      sub2_factors$violin_plots[c('sex', 
                                  'lufo_red', 
                                  'ct_severity_any', 
                                  'diastolic_dysf')]) %>% 
    map2(c(rep('Emotion/concern/consequences', 5), 
           '', 
           rep('Lacking control/coherence', 5)), 
         ~.x + 
           labs(y = .y) + 
           theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              labels = c('A', '', '', 
                         '', '', '',
                         'B', '', '', 
                         '', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s13_cardiopulmo_subscores', 
              ref_name = 'cardiopulmo_sub', 
              caption = paste('Age, sex, cardiopulmonary abnormalities', 
                              'and persistent symptoms at the one-year', 
                              'follow-up and the illness perception components.'), 
              w = 180, 
              h = 230)
  
# Figure S14: interaction effects for the total illness perception score ------
  
  insert_msg('Figure S14: interaction effects, total illness perception')
  
  suppl_fig$interaction_total <- 
    c(inter$violin_plots_ct$ipq_total[c('diastolic_dysf', 
                                        'Chalder_FS_bimodal')], 
      inter$cor_plots_ct$ipq_total['Chalder_FS'], 
      list(legend = ggdraw()), 
      inter$cor_plots_lufo$ipq_total['Chalder_FS'], 
      inter$cor_plots_dia$ipq_total['Chalder_FS']) %>% 
    map(~.x + 
          expand_limits(y = 65) + 
          labs(y = 'Total IP score') + 
          theme(legend.position = 'none', 
                strip.text = element_text(size = 7)))
  
  suppl_fig$interaction_total$legend <- 
    get_legend(inter$violin_plots_ct$ipq_total[[1]])
  
  suppl_fig$interaction_total <- suppl_fig$interaction_total %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', '', '', 
                         'B', 'C'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s14_interaction_total', 
              ref_name = 'interaction_total', 
              caption = paste('Interactions of diastolic dysfunction,', 
                              'fatigue and fatigue intensity with', 
                              'cardiopulmonary findings', 
                              'follow-up affecting total illness perception', 
                              'at the one-year follow-up.'), 
              w = 180, 
              h = 225)
  
# Figure S15 - S16: interaction effects for the emotion/concern/consequences -------
  
  insert_msg('Figure S15 - S16: interaction effects for emotion/concern/consequences')
  
  ## plot lists
  
  suppl_fig$interaction_sub1_ct <- 
    c(inter$violin_plots_ct$ipq_sub1[c('diastolic_dysf', 
                                        'Chalder_FS_bimodal')], 
      inter$cor_plots_ct$ipq_total['Chalder_FS'], 
      list(legend = ggdraw()))
  
  suppl_fig$interaction_sub1_lufo_dia <- 
    c(inter$violin_plots_ct$ipq_sub1[c('sympt_number_class', 
                                       'Chalder_FS_bimodal')], 
      inter$cor_plots_lufo$ipq_total['Chalder_FS'], 
      list(legend = ggdraw()), 
      inter$cor_plots_dia$ipq_total['Chalder_FS'])
  
  suppl_fig[c('interaction_sub1_ct', 
              'interaction_sub1_lufo_dia')] <- 
    suppl_fig[c('interaction_sub1_ct', 
                'interaction_sub1_lufo_dia')] %>% 
    map(~map(.x, 
             ~.x + 
               expand_limits(y = 50) + 
               labs(y = 'Emotion/concern/consequences') + 
               theme(legend.position = 'none', 
                     strip.text = element_text(size = 7))))
  
  suppl_fig$interaction_sub1_ct$legend <- 
    get_legend(inter$violin_plots_ct$ipq_sub1[[1]])
  
  suppl_fig$interaction_sub1_lufo_dia$legend <- 
    get_legend(inter$violin_plots_lufo$ipq_sub1[[1]])
  
  ## the figures
  
  suppl_fig[c('interaction_sub1_ct', 
              'interaction_sub1_lufo_dia')] <- 
    list(plotlist =  suppl_fig[c('interaction_sub1_ct', 
                                 'interaction_sub1_lufo_dia')], 
         labels = list(NULL, 
                       c('A', '', '', '', 'B'))) %>% 
    pmap(plot_grid, 
         ncol = 2, 
         align = 'tblr', 
         label_size = 10)
  
  suppl_fig[c('interaction_sub1_ct', 
              'interaction_sub1_lufo_dia')] <- 
    suppl_fig[c('interaction_sub1_ct', 
                'interaction_sub1_lufo_dia')] %>% 
    list(x = ., 
         label = c('figure_s15_sub1_ct', 
                   'figure_s16_sub1_dia'), 
         ref_name = c('interaction_sub1_ct', 
                      'interaction_sub1_lufo_dia'), 
         caption = c(paste('Interactions of diastolic dysfunction,', 
                           'fatigue and fatigue scoring with chest CT', 
                           'abnormality affecting the', 
                           'emotion/concern/consequences component', 
                           'of illness perception.'), 
                     paste('Interactions of symptom number,',  
                           'fatigue and fatigue scoring with LFT', 
                           'abnormality and diastolic dysfunction', 
                           'affecting the', 
                           'emotion/concern/consequences component', 
                           'of illness perception.')), 
         h = c(150, 225)) %>% 
    pmap(as_figure, 
         w = 180)

# Figure S17: Cluster development and quality control --------
  
  insert_msg('Figure S17: cluster development')
  
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
    as_figure(label = 'figure_s17_cluster_development', 
              ref_name = 'clust_dev', 
              caption = paste('Choice of the optimal clustering algorithm', 
                              'and the cluster number.'), 
              w = 180, 
              h = 190)
  
# Figure R1: performance of backwards elimination -----
  
  insert_msg('Figure R1: performance of backwards elimination')
  
  rev_fig$eli_performance <- 
    map2(eli_mod$cv_fit_plots, 
         c(1.15, 1.3), 
         ~.x + 
           scale_y_discrete(labels = c('ipq_total' = 'total IP score', 
                                       'ipq_sub1' = 'emotion\nconcern\nconsequences', 
                                       'ipq_sub2' = 'lacking\ncontrol\ncoherence')) +  
           expand_limits(x = .y) + 
           theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(eli_mod$cv_fit_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_r1_elimination_performance', 
              ref_name = 'eli_performance', 
              caption = paste('Performance and reproducibility of', 
                              'backward elimination multi-parameter models.'), 
              w = 180, 
              h = 80)  

# Saving the figures on the disc -----
  
  insert_msg('Saving the figures on the disc')
  
  suppl_fig %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
  rev_fig %>% 
    walk(pickle, 
         path = './paper/reviewer figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END ------
  
  insert_tail()
  