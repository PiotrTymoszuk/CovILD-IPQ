# Missingness fraction. Removal of high missing answer records (> 25%)

  insert_head()
  
# container ------
  
  mis <- list()

# calculating percents missing for variables -------
  
  insert_msg('Percentages of missing values for variables')
  
  mis$var_frac_mis <- cov_data$clear_data[globals$variables] %>% 
    map_dbl(~sum(is.na(.x))) %>% 
    tibble(variable = names(.), 
           n_missing = .) %>% 
    mutate(frac_missing = n_missing/nrow(cov_data$clear_data), 
           perc_missing = frac_missing * 100) %>% 
    mutate(var_lab = translate_var(variable))
  
# Plotting variable missingness -------
  
  insert_msg('Plotting variable missngness')
  
  mis$var_mis_plot <- mis$var_frac_mis %>%
    filter(perc_missing > 0) %>%
    ggplot(aes(x = perc_missing, 
               y = reorder(var_lab, perc_missing))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    geom_text(aes(label = signif(perc_missing, 2)), 
              size = 2.75, 
              hjust = -0.5) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Variable missingness', 
         subtitle = paste('total participants: n =', nrow(cov_data$clear_data)), 
         x = '% missing answers')
  
# END -----
  
  insert_tail()