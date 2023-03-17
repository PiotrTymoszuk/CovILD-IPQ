# Kinetic of the persistent symptoms in the study collective

  insert_head()
  
# container -----
  
  sympt_kinet <- list()
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  sympt_kinet$variables <- 
    names(cov_data$sympt_data)[stri_detect(names(cov_data$sympt_data), 
                                           fixed = 'sympt')]
  
  sympt_kinet$variables <- set_names(sympt_kinet$variables, 
                                     sympt_kinet$variables)

  ## analysis table with longitudinal symptom data for 
  ## all and the symptomatic individuals 
  ## specific symptom present at the one-year FUP)
  
  sympt_kinet$all_tbl <- cov_data$sympt_data %>% 
    mutate(time = car::recode(time, 
                              "1 = '2'; 2 = '3'; 3 = '6'; 4 = '12'"), 
           time = as.numeric(time))
  
  sympt_kinet$all_tbl <- sympt_kinet$variables %>% 
    map(~sympt_kinet$all_tbl[c('ID', 'time', .x)])
  
  sympt_kinet$symptomatic_id <- sympt_kinet$all_tbl %>% 
    map2(., names(.), 
         ~filter(.x, 
                 time == 12, 
                 .data[[.y]] == 'yes')) %>% 
    map(~.x$ID)
  
  sympt_kinet$symp_tbl <- 
    map2(sympt_kinet$all_tbl, 
         sympt_kinet$symptomatic_id, 
         ~filter(.x, ID %in% .y))
  
# Descriptive stats --------
  
  insert_msg('Descriptive stats')
  
  ## frequencies of any symptoms and specific symptoms 
  ## at the consecutive visits
  
  sympt_kinet$desc_stats$all <- sympt_kinet$all_tbl %>% 
    map2(., names(.), 
         ~count(group_by(.x, time), 
               .data[[.y]])) %>% 
    map(mutate, 
        percent = n/sum(n) * 100, 
        n_total = sum(n)) %>% 
    map(ungroup) %>% 
    map(set_names, c('time', 'strata', 'n', 'percent', 'n_total')) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x, symptom = .y)) %>% 
    filter(strata == 'yes')
  
  sympt_kinet$desc_stats$sympt <- sympt_kinet$symp_tbl %>% 
    map2(., names(.), 
         ~count(group_by(.x, time), 
                .data[[.y]])) %>% 
    map(mutate, 
        percent = n/sum(n) * 100, 
        n_total = sum(n)) %>% 
    map(ungroup) %>% 
    map(set_names, c('time', 'strata', 'n', 'percent', 'n_total')) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x, symptom = .y)) %>% 
    filter(strata == 'yes')
  
# Modeling the recovery kinetic: all time points and the follow-ups only ------
  
  insert_msg('Modeling the kinetic')
  
  ## models for all time points
  ## second order

  sympt_kinet$models$all <- sympt_kinet$all_tbl %>% 
    map2(., names(.), 
         ~model_kinetic(data = .x, 
                        response = .y, 
                        time = 'time', 
                        ID = 'ID', 
                        family = 'binomial', 
                        order = 2))

  sympt_kinet$models$sympt <- sympt_kinet$symp_tbl %>% 
    map2(., names(.), 
         ~model_kinetic(data = .x, 
                        response = .y, 
                        time = 'time', 
                        ID = 'ID', 
                        family = 'binomial', 
                        order = 2))
  
# Model assumptions -------  

  insert_msg('Model assumptions')
  
  ## normality of residuals
  ## actually quite poor, hence looking for alternatives
  ## such as Cohrane Q test
  
  sympt_kinet$assumptions <- sympt_kinet$models %>% 
    map(~map(.x, summary, 'assumptions') %>% 
          map2_dfr(., names(.), ~mutate(.x, symptom = .y)))
  
# Likelihood ratio test -------
  
  insert_msg('Likelihood ratio test')
  
  sympt_kinet$lrt <- sympt_kinet$models %>% 
    map(~map(.x, lrt))
  
  ## LRT summary: LRT for the entire models
  ## as a global measure to assess changes of frequency in time
  
  sympt_kinet$lrt_summary <- sympt_kinet$lrt %>% 
    map(~map_dfr(.x, filter, order == 'global'))
  
# Cohrane Q test -------
  
  insert_msg('Cohrane Q test')
  
  ## since there's remarkable deviation of normality of residuals
  ## for some models
  ## I'm resorting to non-parametric Cochran's Q test (rstatix)
  
  sympt_kinet$cochran_formulas <- sympt_kinet$variables %>% 
    map(~paste(.x, '~ time|ID')) %>% 
    map(as.formula)

  sympt_kinet$cohran_test <- list(all = sympt_kinet$all_tbl, 
                                  sympt = sympt_kinet$symp_tbl) %>% 
    map(function(data) map2_dfr(data,
                                sympt_kinet$cochran_formulas, 
                                cochran_qtest))

  ## ready to use plot legends
  
  sympt_kinet$plot_legend <- sympt_kinet$cohran_test %>% 
    map(mutate, 
        var_lab = exchange(.y., 
                           dict = globals$var_lexicon), 
        significance = p_value_jps(p), 
        var_lab = paste(var_lab, significance, sep = ', '))
  
  sympt_kinet$plot_legend <- sympt_kinet$plot_legend %>% 
    map(~set_names(.x$var_lab, 
                   .x$.y.))
  
# Plotting --------
  
  insert_msg('Plotting')
  
  sympt_kinet$plots <- list(data = sympt_kinet$desc_stats, 
                            scale = sympt_kinet$plot_legend) %>% 
    pmap(function(data, scale) data %>% 
           ggplot(aes(x = time, 
                      y = percent, 
                      color = symptom)) + 
           geom_path() + 
           geom_point(shape = 16, 
                      size = 2) + 
           scale_color_manual(values = c('green4', 
                                         'firebrick2', 
                                         'steelblue3', 
                                         'gray60', 
                                         'black', 
                                         'orange2', 
                                         'plum3'), 
                              labels = scale, 
                              name = '') + 
           scale_x_continuous(breaks = c(0, 2, 3, 6, 12)) + 
           globals$common_theme + 
           labs(title = 'Kinetic of PSS', 
                subtitle = paste('complete observations: n =', 
                                 nrow(cov_data$clear_data)), 
                x = 'Time after COVID-19, months', 
                y = '% of participants'))
  
# END ------
  
  insert_tail()