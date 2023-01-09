# Main figures of the manuscript

  insert_head()
  
# containers -----
  
  paper_fig <- list()

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
  
  ## Venn plots are present and the key factors for the total illness
  ## perception score, the emotion/concern/consequences 
  ## and the control/coherence sub-scores (no factors identified, actually) 
  ## are shown
  
  paper_fig$key_factors$left_panel <- 
    plot_grid(mod_plots$venn_plots$ipq_total$venn, 
              ggdraw() + 
                draw_text(paste(an$cmm_variables$ipq_total %>% 
                                  translate_var(out_value = 'label'), 
                                collapse = '\n'), 
                          hjust = 0, 
                          x = 0.1, 
                          size = 8), 
              ncol = 2, 
              rel_widths = c(0.57, 0.43))
  
  paper_fig$key_factors$right_panel <- 
    plot_grid(mod_plots$venn_plots$ipq_sub1$venn, 
              ggdraw() + 
                draw_text(paste(an$cmm_variables$ipq_sub1 %>% 
                                  translate_var(out_value = 'label'), 
                                collapse = '\n'), 
                          hjust = 0, 
                          x = 0.1, 
                          size = 8), 
              ncol = 2, 
              rel_widths = c(0.57, 0.43))
  
  ## the entire figure
  
  paper_fig$key_factors <- 
    plot_grid(paper_fig$key_factors$left_panel + 
                theme(plot.margin = ggplot2::margin(t = 0.3, 
                                                    l = 0.25, 
                                                    unit = 'cm')), 
              paper_fig$key_factors$right_panel + 
                theme(plot.margin = ggplot2::margin(t = 0.3, 
                                                    l = 0.25, 
                                                    unit = 'cm')), 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(plot_grid(mod_plots$venn_plots$ipq_sub1$legend_grob, 
                        ggdraw() + 
                          draw_text(paste0('variables: n = ', 
                                           length(mod$variables), 
                                           ', observations: n = ', 
                                           nrow(mod$multi_tbl)), 
                                    size = 8)), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_2_key_factors', 
              ref_name = 'key_factors', 
              caption = paste('Key factors associated with disease', 
                              'perception one year after COVID-19.'), 
              w = 180, 
              h = 73)

# Figure 3: influential factors, total BIPQ -------  
  
  insert_msg('Figure 3: influential factors, total BIPQ')
  
  paper_fig$total_bipq <- 
    c(ipq_factors$violin_plots[c('sympt_number_class', 
                                 'fatigue_sympt')], 
      ipq_factors$corr_plots['Chalder_FS']) %>% 
    map(~.x + 
          labs(y = 'Total IP score') + 
          scale_y_continuous(limits = c(0, 60), 
                             breaks = seq(0, 60, by = 20)) + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    as_figure(label = 'figure_3_factors_total_ipq', 
              ref_name = 'total_bipq', 
              caption = paste('Persistent symptom number,', 
                              'reduced physical performance,', 
                              'fatigue and the total illness', 
                              'perception scoring'), 
              w = 180, 
              h = 70)
  
# Figure 4: influential factors, emotion/concern/consequences ------
  
  insert_msg('Figure 4: influential factors, emotion/concern/consequences')
  
  paper_fig$sub1_bipq <- list(## symptoms: 
                              sub1_factors$violin_plots$sympt_number_class, 
                              sub1_factors$corr_plots$Chalder_FS, 
                              sub1_factors$violin_plots$Chalder_FS_bimodal, 
                              sub1_factors$violin_plots$fatigue_sympt, 
                              sub1_factors$violin_plots$hair_loss_sympt, 
                              sub1_factors$violin_plots$cough_sympt, 
                              ## other one-year features
                              sub1_factors$violin_plots$rehabilitation, 
                              sub1_factors$violin_plots$diastolic_dysf,
                              sub1_factors$violin_plots$CRP_class, 
                              ## comorbidities
                              sub1_factors$violin_plots$respi_comorb, 
                              sub1_factors$violin_plots$weight_class) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(0, 50), 
                             breaks = seq(0, 40, by = 20)) + 
          labs(y = 'Emotion/concern/consequencessource') + 
          theme(legend.position = 'none')) %>%
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              labels = c('A', '', '', 
                         '', '', '', 
                         'B', '', '', 
                         'C', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_4_factors_emotion_concern_consequences_ipq', 
              ref_name = 'sub1_bipq', 
              caption = paste('Key factors influencing the', 
                              'emotional representation, concern', 
                              'and consequences component of illness perception.'), 
              w = 180, 
              h = 230)
  
# Figure 5: characteristic of the clusters --------
  
  insert_msg('Figure 5: Characteristic of the clusters')
  
  ## upper panel: radial plot of the BIPQ levels in the clusters
  
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
              rel_widths = c(0.05, 0.7, 0.25))
  
  ## bottom panel: levels of the total BIPQ 
  ## and the sub-scores of emotion/concern/consequences and control/coherence
  
  paper_fig$ipq_clusters$bottom_panel <- 
    clust_chara$ipq_plots[c('ipq_total', 
                            'ipq_sub1', 
                            'ipq_sub2')] %>% 
    map(~.x + 
          labs(title = stri_replace(.x$labels$title, 
                                    regex = '\\s{1}\\(.*', 
                                    replacement = '')) + 
          theme(legend.position = 'none', 
                plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  ## the entire figure
  
  paper_fig$ipq_clusters <- plot_grid(paper_fig$ipq_clusters$top_panel, 
                                      paper_fig$ipq_clusters$bottom_panel, 
                                      nrow = 2, 
                                      rel_heights = c(1.2, 0.8), 
                                      labels = LETTERS, 
                                      label_size = 10) %>% 
    as_figure(label = 'figure_5_ipq_clusters', 
              ref_name = 'ipq_clusters', 
              caption = paste('Clusters of illness perception'), 
              w = 180, 
              h = 170)
  
# Figure 6: course of COVID and sequelae in the clusters ------
  
  insert_msg('Figure 6: COVID-19 course and sequelae in the clusters')
  
  ## upper panel: COVID-19 severity and rehabilitation
  
  paper_fig$clust_cov$upper_panel <- 
    list(clust_chara$baseline_plots$cat_WHO + 
           scale_fill_manual(values = globals$sev_colors[c('A', 'HM', 'HS')]) + 
           theme(legend.position = 'bottom'), 
         clust_chara$baseline_plots$WHO + 
           theme(legend.position = 'none'), 
         clust_chara$fup_plots$rehabilitation + 
           scale_fill_manual(values = c(no = 'steelblue', 
                                        yes = 'coral3'), 
                             name = '') + 
           theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: symptoms and cardiopulmonary residuals
  ## CT severity score
  
  paper_fig$clust_cov$bottom_panel <- 
    plot_grid(clust_chara$sequelae_plot + 
                theme(legend.position = 'none'),
              plot_grid(get_legend(clust_chara$sequelae_plot + 
                                     theme(legend.position = 'bottom')), 
                        ggdraw() + 
                          draw_text(map2_chr(pap$n_clust[[1]], 
                                             pap$n_clust[[2]], 
                                             paste, sep = ': n = ') %>% 
                                      paste(collapse = ', '), 
                                    size = 8, 
                                    hjust = 0.5), 
                        ncol = 2), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    plot_grid(plot_grid(clust_chara$fup_plots$ct_severity_score + 
                          theme(legend.position = 'none'), 
                        ncol = 1, 
                        nrow = 2, 
                        rel_heights = c(1.1, 0.9)), 
              ncol = 2, 
              rel_widths = c(2.03, 0.97), 
              labels = c('B', 'C'), 
              label_size = 10)
  
  ## entire figures
  
  paper_fig$clust_cov <- plot_grid(paper_fig$clust_cov$upper_panel, 
                                   paper_fig$clust_cov$bottom_panel, 
                                   nrow = 2, 
                                   rel_heights = c(0.8, 1.2), 
                                   labels = c('A', ''), 
                                   label_size = 10) %>% 
    as_figure(label = 'figure_6_cluster_covid_course_fup', 
              ref_name = 'clust_cov', 
              caption = paste('Course of COVID-19, rehabilitation and', 
                              'one-year follow-up sequelae of COVID-19', 
                              'in the illness perception clusters.'), 
              w = 180, 
              h = 185)
  
# Figure 7: persistent symptoms in the clusters --------  
  
  insert_msg('Figure 7: COVID-19 symptoms in the clusters')
  
  ## left panel, summary of symptom frequencies
  
  paper_fig$clust_symptoms$left_panel <- 
    plot_grid(clust_chara$symptom_plot + 
                theme(legend.position = 'none', 
                      strip.text = element_text(size = 7)),
              plot_grid(get_legend(clust_chara$symptom_plot + 
                                     theme(legend.position = 'bottom')), 
                        ggdraw() + 
                          draw_text(map2_chr(pap$n_clust[[1]], 
                                             pap$n_clust[[2]], 
                                             paste, sep = ': n = ') %>% 
                                      paste(collapse = ', '), 
                                    size = 8, 
                                    hjust = 0.5), 
                        ncol = 2), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05))
  
  ## right panel: symptom counts and measures of fatigue
  
  paper_fig$clust_symptoms$right_panel <- 
    clust_chara$fup_plots[c('sympt_number', 
                            'Chalder_FS')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              nrow = 4, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figure
  
  paper_fig$clust_symptoms <- 
    plot_grid(paper_fig$clust_symptoms$left_panel, 
              paper_fig$clust_symptoms$right_panel, 
              ncol = 2, 
              rel_widths = c(2, 1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_7_cluster_symptoms', 
              ref_name = 'clust_symptoms', 
              caption = paste('Persistent COVID-19-related symptoms at', 
                              'the one-year follow-up in the illness', 
                              'perception clusters.'), 
              w = 180, 
              h = 220)
  
# Saving the figures ------
  
  insert_msg('Saving the figures on the disc')
  
  paper_fig %>% 
    walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()
  