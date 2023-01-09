# Overlap between the cardiopulmonary findings and persistent symptoms
# at the one-year follow-up

  insert_head()
  
# container -------
  
  fup_overlap <- list()
  
# globals ---- 
  
  insert_msg('Analysis globals')
  
  fup_overlap$analysis_sets <- 
    cov_data$clear_data[c('sympt_present', 
                          'ct_severity_any', 
                          'lufo_red', 
                          'diastolic_dysf')] %>% 
    map_dfc(~.x == 'yes') %>% 
    set_names(c('PSS', 
                'CT findings', 
                'LFT abnormality', 
                'TTE abnormality'))
  
  fup_overlap$pairs <- combn(c('PSS', 
                               'CT findings', 
                               'LFT abnormality', 
                               'TTE abnormality'), 
                             m = 2, 
                             simplify = FALSE)
  
# Calculating the intersection sizes ------
  
  insert_msg('Intersection sizes')
  
  ## pairwise intersections
  
  fup_overlap$intersections <- fup_overlap$pairs %>% 
    set_names(map_chr(fup_overlap$pairs, paste, collapse = '_')) %>% 
    map(~fup_overlap$analysis_sets[.x]) %>% 
    map(reduce, `*`) %>% 
    map_dbl(sum) %>% 
    map2_dfr(., names(.), 
             ~tibble(pair = .y, 
                     count = .x)) %>% 
    mutate(variable1 = stri_split_fixed(pair, 
                                        pattern = '_', 
                                        simplify = TRUE)[, 1], 
           variable2 = stri_split_fixed(pair, 
                                        pattern = '_', 
                                        simplify = TRUE)[, 2])
  
  ## the intersection between all sequelae
  
  fup_overlap$all_intersection <-  fup_overlap$analysis_sets %>% 
    reduce(`*`) %>% 
    sum
  
  ## common table
  
  fup_overlap$intersections <- 
    rbind(fup_overlap$intersections, 
          tibble(pair = 'all', 
                 count = fup_overlap$all_intersection, 
                 variable1 = NA, 
                 variable2 = NA)) %>% 
    mutate(percent = count/nrow(fup_overlap$analysis_sets) * 100)

# Venn plot -------
  
  insert_msg('Venn plot')
  
  fup_overlap$venn_plot <- ggvenn(fup_overlap$analysis_sets, 
                                  columns = c('PSS', 
                                              'CT findings', 
                                              'LFT abnormality', 
                                              'TTE abnormality'), 
                                  fill_color = c('darkolivegreen4', 
                                                 'coral3', 
                                                 'plum3', 
                                                 'gray60'), 
                                  set_name_size = 2.75, 
                                  set_name_color = c('darkolivegreen4', 
                                                     'coral4', 
                                                     'plum4', 
                                                     'gray40'), 
                                  text_size = 2.75) + 
    labs(title = paste('Overlap between COVID-19 sequelae at', 
                       'the one-year follow-up')) + 
    theme(plot.title = element_text(size = 8, face = 'bold'))
  
# nVenn plot -------
  
  insert_msg('nVenn plot')
  
  fup_overlap$nvenn_list <- fup_overlap$analysis_sets %>% 
    map(~cov_data$clear_data$ID[.x])
  
  fup_overlap$nvenn_plot <- 
    plot_n_venn(data = fup_overlap$nvenn_list, 
                fill_color = c('darkolivegreen', 
                               'coral', 
                               'plum', 
                               'gray'), 
                subset_names = c('PSS', 
                                 'CT findings', 
                                 'LFT abnormality', 
                                 'TTE abnormality'), 
                plot_title = paste('Overlap between COVID-19 sequelae at', 
                                   'the one-year follow-up'), 
                plot_leg_ratio = c(0.8, 0.2))
  
# END -----
  
  insert_tail()