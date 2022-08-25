# Characteristic of the clusters, using the variables specified
# for the explorative analysis

  insert_head()
  
# container list -------
  
  clust_chara <- list()

# globals: analysis table -----
  
  insert_msg('Analysis table')

  clust_chara$analysis_tbl <- left_join(ipq_clust$clust_obj$clust_assignment %>% 
                                          set_names(c('ID', 'clust_id')), 
                                        cov_data$clear_data, 
                                        by = 'ID')
  
  plan('multisession')
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  clust_chara$desc_stats <- expl$variables %>% 
    future_map(~explore(clust_chara$analysis_tbl, 
                        split_factor = 'clust_id', 
                        variables = .x, 
                        what = 'table', 
                        pub_styled = TRUE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(clust_chara$analysis_tbl$clust_id)))
    
# serial testing ------
  
  insert_msg('Serial testing')
  
  clust_chara$test_results <- list(variables = expl$variables, 
                                   types = expl$test_type) %>% 
    future_pmap(compare_variables,
                clust_chara$analysis_tbl, 
                split_factor = 'clust_id', 
                what = 'test', 
                ci = FALSE, 
                pub_styled = TRUE, 
                adj_method = 'none', 
                .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots -------
  
  insert_msg('Plots')
  
  clust_chara[c('baseline_plots', 
                'fup_plots', 
                'ipq_plots', 
                'psych_plots', 
                'lab_plots')] <- c('baseline', 
                                   'fup', 
                                   'ipq', 
                                   'psych', 
                                   'lab') %>% 
    map(function(resp_type) list(variable = expl$variables[[resp_type]], 
                                 type = expl$plot_type[[resp_type]], 
                                 plot_title = expl$variables[[resp_type]] %>% 
                                   translate_var(out_value = 'label_long'), 
                                 plot_subtitle = clust_chara$test_results[[resp_type]]$plot_cap, 
                                 y_lab = expl$variables[[resp_type]] %>% 
                                   translate_var(out_value = 'axis_lab')) %>% 
          pmap(plot_variable, 
               clust_chara$analysis_tbl, 
               split_factor = 'clust_id',
               scale = 'percent', 
               x_lab = 'IP cluster', 
               cust_theme = globals$common_theme) %>% 
          map(~.x + 
                labs(tag = .x$labels$tag %>% 
                       stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                       paste0('\n', .)) + 
                scale_fill_manual(values = globals$clust_colors)) %>% 
          set_names(expl$variables[[resp_type]]))
  
# A summary heat map of the psych variables in the clusters ------
  
  insert_msg('A summary heat map with psych varaibles in the clusters')
  
  ## incomplete IDs
  
  clust_chara$psy_plot_ID <- clust_chara$analysis_tbl[c('ID', expl$variables$psych)] %>% 
    column_to_rownames('ID') %>% 
    t %>% 
    as.data.frame %>% 
    map(function(x) if(all(is.na(x))) NULL else x) %>% 
    compact %>% 
    names
  
  ## plotting data, mean-centering normalization
  ## eliminating the individuals with the missing entire psy battery
  
  clust_chara$psy_plot_data <- clust_chara$analysis_tbl %>% 
    select(ID, clust_id, expl$variables$psych) %>% 
    filter(ID %in% clust_chara$psy_plot_ID)

  clust_chara$psy_plot_data <- 
    clust_chara$psy_plot_data[expl$variables$psych] %>% 
    center_data(complete_cases = FALSE) %>% 
    cbind(clust_chara$psy_plot_data[c('ID', 'clust_id')], .) %>% 
    as_tibble
  
  ## n numbers presented in the plot
  
  clust_chara$psy_plot_n <- clust_chara$psy_plot_data %>% 
    count(clust_id)
  
  clust_chara$psy_plot_tag <- map2_chr(clust_chara$psy_plot_n[[1]], 
                                       clust_chara$psy_plot_n[[2]], 
                                       paste, 
                                       sep = ': n = ') %>% 
    paste(collapse = ', ') %>% 
    paste0('\n', .)
  
  ## long format

  clust_chara$psy_plot_data <-  clust_chara$psy_plot_data %>% 
    pivot_longer(cols = expl$variables$psych, 
                 names_to = 'variable', 
                 values_to = 'norm_val')
  
  ## significant psych parameters
  
  clust_chara$psy_signif <- clust_chara$test_results$psych %>% 
    filter(p_value < 0.05) %>% 
    .$variable %>% 
    translate_var
  
  ## heat map
  
  clust_chara$psy_plot_hm <- clust_chara$psy_plot_data %>% 
    mutate(variable = translate_var(variable)) %>% 
    ggplot(aes(x = reorder(ID, norm_val), 
               y = variable, 
               fill = norm_val)) + 
    geom_tile() + 
    scale_y_discrete(limits = translate_var(expl$variables$psych), 
                     labels = function(x) embolden_scale(x, 
                                                         highlight = clust_chara$psy_signif)) + 
    scale_fill_gradient2(low = 'steelblue2', 
                         mid = 'black', 
                         high = 'firebrick2', 
                         midpoint = 0.5, 
                         limits = c(-3, 3), 
                         name = 'Z-score', 
                         oob = scales::squish) + 
    facet_grid(. ~ clust_id, 
               scales = 'free', 
               space = 'free') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.line = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_markdown()) + 
    labs(title = 'QoL and mental health in the IP clusters', 
         subtitle = 'Kruskal-Wallis test, normalized scores', 
         x = 'Patricipant',
         tag = clust_chara$psy_plot_tag )
  
# END -----
  
  insert_tail()