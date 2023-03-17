# A medley of functional project tools

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(cowplot)
  library(glue)
  library(ggtext)
  library(nVennR)
  library(cowplot)
  library(magick)
  
# data import and transformation -----

  fill_forward <- function(data_chunk, variable) {
    
    ## if complete recovery was observed at the 6 month visit and the one-year
    ## follow-up is missing, the last non-missing is filled forward
    
    data_chunk <- data_chunk %>% 
      arrange(time)
    
    
    last_rec <- data_chunk[nrow(data_chunk), variable]
    
    if(!is.na(last_rec)) {
      
      return(data_chunk)
      
    }
    
    for(i in nrow(data_chunk):1) {
      
      last_full <- data_chunk[nrow(data_chunk) - i, variable]
      
      if(!is.na(last_rec)) {
        
        break
        
      }
      
    }

    data_chunk[nrow(data_chunk), variable] <- last_full
    
    data_chunk
    
  }

# variable:label translation, color setup -----

  set_colors_ <- function(color_no, seed = 123) {
    
    ## picks n colors at random from the standard palette
    
    set.seed(seed)
    
    return(colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] %>% 
             sample(size = color_no))
    
  }
  
# distribution testing -------
  
  get_mvr <- function(data, variables) {
    
    stopifnot(all(variables %in% names(data)))
    
    variables %>% 
      map_dfr(~tibble(variable = .x, 
                      mean = mean(data[[.x]], na.rm = TRUE), 
                      variance = var(data[[.x]], na.rm = TRUE))) %>% 
      mutate(mvr = mean/variance)
    
  }
  
# Training and CV stat plotting -----
  
  plot_rsq_rmse <- function(cv_stats, 
                            plot_subtitle = NULL, 
                            y_factor = 'response') {
    
    list(x = c('rsq', 'RMSE'), 
         y = c('R\u00B2', 'RMSE'), 
         z = c('Explanatory power', 
               'Model error')) %>% 
      pmap(function(x, y, z) cv_stats %>% 
             filter(statistic == x) %>% 
             ggplot(aes(x = estimate, 
                        y = .data[[y_factor]], 
                        fill = prediction)) + 
             geom_bar(stat = 'identity', 
                      position = position_dodge(0.9), 
                      color = 'black') + 
             geom_text(aes(label = signif(estimate, 2)), 
                       size = 2.75, 
                       hjust = -0.5, 
                       position = position_dodge(0.9)) + 
             scale_fill_manual(values = c('steelblue3', 'indianred4'), 
                               labels = c('training', '10-fold CV'), 
                               name = '') + 
             globals$common_theme + 
             theme(axis.title.y = element_blank()) + 
             labs(title = z, 
                  subtitle = plot_subtitle, 
                  x = y)) %>% 
      set_names(c('rsq', 'RMSE'))
    
  }
  
# Model estimates ------
  
  get_var_level <- function(est_data_list, 
                            var_names = globals$variables, 
                            total_n = nrow(mod$multi_tbl), 
                            level_n = mod$level_n) {
    
    ## fills an estimate table with total and level n numbers
    
    ## regex to extract the variable names
    
    extr_regex <- paste(sort(var_names, 
                             decreasing = TRUE), 
                        collapse = '|')
    
    ## defining the levels for factor variable only
    ## total n numbers
    
    out_list <- map(est_data_list, 
                    ~mutate(.x,  
                            variable = stri_extract(parameter, 
                                                    regex = extr_regex), 
                            level = stri_replace(parameter, 
                                                 regex = extr_regex, 
                                                 replacement = ''), 
                            level = ifelse(level == '(Intercept)', 
                                           'baseline', 
                                           ifelse(level == '', 
                                                  NA, 
                                                  level)),  
                            n_complete = total_n))
    
    ## appending with the n numbers for particular level
    
    out_list <- map(out_list, 
                     ~safely(left_join)(.x, 
                                        level_n, 
                                        by = c('variable', 'level'))) %>% 
      map(~.x$result) %>% 
      compact %>% 
      map(mutate, 
          n = ifelse(is.na(n), n_complete, n), 
          y_ax = ifelse(level == 'yes' | is.na(level), 
                        paste0(exchange(variable, 
                                        dict = globals$var_lexicon), 
                               '\nn = ', n), 
                        paste0(exchange(variable, 
                                        dict = globals$var_lexicon), 
                               ': ', level, '\nn = ', n)), 
          y_ax = ifelse(parameter == '(Intercept)', 'Baseline', y_ax))
    
    out_list
    
  }
  
# Model estimate plots -----
  
  est_bubble <- function(est_data, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         x_lab = expression(beta)) {
    
    ## plots model estimates in form of a bubble plot
    
    ggplot(est_data, 
           aes(x = estimate, 
               y = reorder(y_ax, estimate), 
               fill = estimate, 
               size = abs(estimate))) + 
      geom_vline(xintercept = 0, 
                 linetype = 'dashed') + 
      geom_point(shape = 21) + 
      geom_text(aes(label = signif(estimate, 2)), 
                size = 2.75, 
                vjust = -1.5) + 
      scale_size_area(max_size = 6) + 
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'white', 
                           high = 'coral3', 
                           midpoint = 0, 
                           name = expression(beta)) + 
      guides(size = 'none') + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = x_lab)
    
  }
  
  
# Venn plots ------
  
  plot_n_venn <- function(data, 
                          subset_names = NULL, 
                          fill_color = unname(an$method_colors), 
                          plot_title = '', 
                          plot_subtitle = '', 
                          plot_tag = '', 
                          legend_position = 'right', 
                          opacity = 0.2, 
                          borderWidth = 2, 
                          labelRegions = FALSE, 
                          fontScale = 2,
                          plot_leg_ratio = c(0.9, 0.1), 
                          comp_heights = c(0.08, 0.06, 0.8, 0.06), 
                          panel = TRUE, ...) {
    
    ## plots a Venn diagram employing nVennR
    
    if(!is.null(subset_names)) {
      
      subset_names <- stri_replace(subset_names, 
                                   fixed = '\n', 
                                   replacement = ' ')
      
      data <- set_names(data, subset_names)
      
      
    } 
    
    temp_file <- sample(LETTERS, 5, replace = TRUE) %>% 
      paste(collapse = '') %>% 
      paste0('.svg')
    
    venn_plot <- plotVenn(sets = data, 
                          showPlot = TRUE, 
                          systemShow = FALSE, 
                          showLegend = FALSE, 
                          outFile = temp_file, 
                          setColors = fill_color, 
                          opacity = opacity, 
                          borderWidth = borderWidth, 
                          labelRegions = labelRegions, 
                          fontScale = fontScale, ...)
    
    gg_plot <- plot_grid(ggdraw() + 
                           draw_image(image_read_svg(temp_file)))
    
    file.remove(temp_file)
    
    gg_plot <- plot_grid(ggdraw() + 
                           draw_text(text = plot_title, 
                                     size = 8, 
                                     fontface = 'bold', 
                                     hjust = 0, 
                                     x = 0.1), 
                         ggdraw() + 
                           draw_text(text = plot_subtitle, 
                                     size = 8, 
                                     fontface = 'plain', 
                                     hjust = 0, 
                                     x = 0.1), 
                         gg_plot, 
                         ggdraw() + 
                           draw_text(text = plot_tag, 
                                     size = 8, 
                                     fontface = 'plain', 
                                     hjust = 0.5), 
                         nrow = 4, 
                         rel_heights = c(0.08, 0.06, 0.8, 0.06))
    
    ## the legend will be obtained from a 'fake plot'
    
    gg_legend <- tibble(subset = factor(subset_names, 
                                        levels = subset_names), 
                        n = 1) %>% 
      ggplot(aes(x = subset_names, 
                 y = n, 
                 fill = subset_names)) + 
      geom_bar(stat = 'identity', 
               alpha = if(2.5 * opacity > 1) 1 else 2.5 * opacity, 
               size = borderWidth) + 
      scale_fill_manual(values = set_names(fill_color, 
                                           subset_names), 
                        name = '') + 
      globals$common_theme + 
      theme(legend.position = legend_position)
    
    gg_legend <- get_legend(gg_legend)
    
    if(panel) {
      
      switch(legend_position, 
             'right' = plot_grid(gg_plot, 
                                 gg_legend, 
                                 ncol = 2, 
                                 rel_widths = plot_leg_ratio) + 
               theme(plot.margin = globals$common_margin), 
             'bottom' = plot_grid(gg_plot, 
                                  gg_legend, 
                                  nrow = 2, 
                                  rel_heights = plot_leg_ratio) + 
               theme(plot.margin = globals$common_margin))
      
    } else {
      
      list(venn = gg_plot, 
           legend_grob = gg_legend)
      
    }
    
  }
  
# Plotting two-way interactions ------
  
  two_way_corr <- function(data, 
                           response, 
                           num_variable, 
                           split_factor = 'ct_severity_any', 
                           plot_title = exchange(num_variable, 
                                                 dict = globals$var_lexicon), 
                           plot_subtitle = NULL, 
                           x_lab = exchange(num_variable, 
                                            dict = globals$var_lexicon, 
                                            value = 'axis_lab'), 
                           y_lab = exchange(response, 
                                            dict = globals$var_lexicon, 
                                            value = 'axis_lab'), 
                           labeller = 'label_value',  
                           plot_tag = NULL, 
                           fill_colors = c('steelblue', 'coral3'), ...) {
    
    ## correlations with a illness perception score 
    ## in the strata defined by a split factor
    
    data %>% 
      ggplot(aes(x = .data[[num_variable]], 
                 y = .data[[response]], 
                 fill = .data[[split_factor]])) + 
      geom_point(shape = 21, 
                 size = 2) + 
      geom_smooth(...) + 
      scale_fill_manual(values = fill_colors) + 
      facet_grid(cols = vars(.data[[split_factor]]), 
                 labeller = labeller) + 
      guides(fill = 'none') + 
      globals$common_theme + 
      labs(title = plot_title, 
           plot_subtitle = NULL, 
           x = x_lab, 
           y = y_lab, 
           tag = plot_tag)
    
  }
  
  two_way_violin <- function(data, 
                             response, 
                             fct_variable, 
                             split_factor = 'ct_severity_any', 
                             plot_title = exchange(fct_variable, 
                                                   dict = globals$var_lexicon, 
                                                   value = 'label_long'), 
                             plot_subtitle = NULL, 
                             x_lab = exchange(fct_variable, 
                                              dict = globals$var_lexicon), 
                             y_lab = exchange(response,
                                              dict = globals$var_lexicon, 
                                              value = 'axis_lab'), 
                             fill_lab = exchange(split_factor, 
                                                 dict = globals$var_lexicon), 
                             plot_tag = NULL, 
                             shape_alpha = 0.25, 
                             point_alpha = 0.5, 
                             fill_colors = c('steelblue', 'coral3'), ...) {
    
    ## violin plot of an illness perception score split by the fct_variable
    ## and the split_factor
    
    ## a table with means and interquartile ranges
    
    median_tbl <- data %>%
      group_by(.data[[fct_variable]], .data[[split_factor]]) %>% 
      summarise(median = median(.data[[response]]), 
                lower_q = quantile(.data[[response]], 0.25), 
                upper_q = quantile(.data[[response]], 0.75), 
                n = length(.data[[response]]), 
                text_y = max(.data[[response]])) %>% 
      ungroup
    
    ## the plot
    
    data %>% 
      ggplot(aes(x = .data[[fct_variable]], 
                 y = .data[[response]], 
                 fill = .data[[split_factor]])) + 
      geom_violin(alpha = shape_alpha, 
                  position = position_dodge(0.9), 
                  show.legend = FALSE, 
                  ...) + 
      geom_point(position = position_jitterdodge(jitter.width = 0.1, 
                                                 jitter.height = 0, 
                                                 dodge.width = 0.9), 
                 shape = 21, 
                 size = 2, 
                 alpha = point_alpha) + 
      geom_errorbar(data = median_tbl, 
                    aes(y = median, 
                        ymin = lower_q, 
                        ymax = upper_q), 
                    width = 0, 
                    size = 0.75, 
                    color = 'orangered3', 
                    position = position_dodge(0.9)) + 
      geom_point(data = median_tbl,
                 aes(y = median, 
                     group = .data[[split_factor]]), 
                 shape = 23, 
                 size = 3, 
                 fill = 'orangered3', 
                 position = position_dodge(0.9)) + 
      geom_text(data = median_tbl, 
                aes(y = text_y, 
                    label = paste('n =', n), 
                    color = .data[[split_factor]]), 
                size = 2.6, 
                vjust = -0.5, 
                position = position_dodge(0.9), 
                show.legend = FALSE) + 
      scale_fill_manual(values = fill_colors) + 
      scale_color_manual(values = fill_colors) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = x_lab, 
           y = y_lab, 
           fill = fill_lab, 
           tag = plot_tag)
    
  }
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = TRUE) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, na.rm = na.rm)
    
  }
  
  complete_cases <- function(data, id_var = 'ID') {
    
    ### selects the individuals with the complete variable record
    
    dlply(data, id_var) %>% 
      map_dfr(function(x) if(any(!complete.cases(x))) NULL else x)
    
    
  }
  
  mm_inch <- function(x) 0.0393700787 * x
  
# formatting functions ----
  
  format_summ_tbl <- function(data, 
                              rm_n = TRUE, 
                              rm_mean = TRUE, 
                              rm_complete = TRUE, 
                              out_value = 'axis_lab_long') {
    
    ## formats a summary table with descriptive stats
    
    data <- data %>% 
      map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
      map_dfc(stri_replace, regex = '\\nno:.*$', replacement = '') %>% 
      map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
      map_dfc(stri_replace, fixed = 'Median =', replacement = 'median:') %>% 
      map_dfc(stri_replace, fixed = 'Mean =', replacement = 'mean:') %>% 
      map_dfc(stri_replace, fixed = 'Range', replacement = 'range') %>% 
      map_dfc(stri_replace, fixed = 'Complete', replacement = 'complete') %>% 
      #map_dfc(stri_replace, fixed = ' [', replacement = '\n[') %>% 
      mutate(variable = exchange(variable, 
                                 dict = globals$var_lexicon, 
                                 value = out_value))
    
    if(rm_n) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '\\ncomplete.*$', replacement = '')
        
    }
    
    if(rm_mean) {
      
      data <- data %>% 
        map_dfc(stri_replace, 
                regex = 'mean.*\\nmedian:\\s{1}', 
                replacement = '')
      
    }
    
    if(rm_complete) {
      
      data <- data %>% 
        map_dfc(stri_replace, fixed = 'complete: ', replacement = '')
      
    }
    
    data
    
  }
  
  re_adjust <- function(data, method = 'BH') {
    
    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method
    
    if(method != 'none') {
      
      data <- data %>% 
        mutate(p_adjusted = p.adjust(p_value, method = method))
      
    }
    
    data %>% 
      mutate(significance = ifelse(p_adjusted < 0.001, 
                                   'p < 0.001', 
                                   ifelse(p_adjusted >= 0.05, 
                                          paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                          paste('p =', signif(p_adjusted, 2)))))
    
  }

  embolden_scale <- function(x, 
                             highlight,  
                             color = 'black', 
                             family = '', 
                             translate = FALSE, 
                             dict = globals$var_lexicon, ...) {
    
    if(!translate) {
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{x}</b>"), 
                    x))
      
    } else {
      
      labels <- exchange(x, dict = dict, ...)
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{labels[x]}</b>"), 
                    labels[x]))
      
      
    }
    
  }
  
  capitalize <- function(string) {
    
    paste0(toupper(substr(string, 1, 1)), 
           substr(string, 2, nchar(string)))
    
  }
  
  de_capitalize <- function(string) {
    
    paste0(tolower(substr(string, 1, 1)), 
           substr(string, 2, nchar(string)))
    
  }
  
  collapse_and <- function(vector) {
    
    len <- length(vector)
    
    body <- paste(vector[-len], collapse = ', ')
    
    paste0(body, ' and ', vector[len])
    
  }
  
  get_median_iqr <- function(data, variable, signif_digits = 2) {
    
    ## extracts the median value, IQR and the number of complete cases
    ## returns a list with these elements
    
    val_vec <- data[[variable]]
    
    if(!is.numeric(val_vec)) {
      
      stop('Numeric variable required.', call. = FALSE)
      
    }
    
    median <- median(val_vec, na.rm = TRUE) %>% 
      signif(signif_digits)
    
    n_complete <- length(val_vec[!is.na(val_vec)])
    
    iqr <- quantile(val_vec, c(0.25, 0.75)) %>%
      signif(signif_digits) %>% 
      paste(collapse = ' - ')
    
    list(median = median, 
         iqr = iqr, 
         complete = n_complete) %>% 
      map_chr(as.character)
    
  }
  
  get_percent <- function(data, variable, signif_digits = 2) {
    
    ## returns the percents and complete cases

    if(!is.factor(data[[variable]])) {
      
      data <- data %>% 
        mutate(!!variable := factor(.data[[variable]]))
      
    }
    
    count_tbl <- count(data, .data[[variable]]) %>% 
      mutate(percent = signif(n/sum(n) * 100, signif_digits))
    
    perc_vec <- set_names(count_tbl[['percent']], 
                          count_tbl[[1]])

    n_complete <- sum(count_tbl[['n']])
    
    c(perc_vec, 
      c(complete = n_complete))
  
  }
  
  p_value_jps <- function(x) {
    
    ## formats p values according 
    ## to the style of J. Psychosomatic Research
    
    if(length(x) > 1) {
      
      return(map_chr(x, p_value_jps))
      
    }
    
    stopifnot(is.numeric(x))
    
    if(x < 0.001) {
      
      return('p < .001')
      
    }
    
    if(x > 0.99) {
      
      return('ns (p > .99)')
      
    }
    
    if(x >= 0.05) {
      
      new_x <- as.character(round(x, 2))
      
      new_x <- stri_replace(new_x, 
                            fixed = '0.', 
                            replacement = '.')
      
      if(stri_length(new_x) == 2) new_x <- paste0(new_x, '0')
      
      return(paste0('ns (p = ', 
                    new_x, ')'))
      
    }
    
    if(x < 0.01) {
      
      new_x <- round(x, 3)
      
      if(new_x == 0.01) new_x <- paste0(as.character(new_x), '0')
      
      new_x <- as.character(new_x)
      
    } else if(x < 0.05) {
      
      new_x <- round(x, 2)
      
      if(new_x == 0.05) new_x <- round(x, 3)
      
    }
    
    new_x <- as.character(new_x)
    
    new_x <- stri_replace(new_x, 
                          fixed = '0.', 
                          replacement = '.')
    
    return(paste('p =', new_x))
    
    
  }
  
  eff_size_jps <- function(x) {
    
    ## formats the effect size value according to the JPS style
    ## takes a text entry and formats its numeric part
    
    stopifnot(is.character(x))
    
    effect_txt <- stri_extract(x, regex = '(V|r|d|(\u03B7\u00B2))\\s{1}=')
    
    effect_digits <- stri_extract(x, regex = '\\d{1}\\.\\d+')
    
    effect_digits <- as.numeric(effect_digits)
    
    effect_digits <- ifelse(effect_digits < 0, 0, effect_digits)
    
    effect_digits <- as.character(effect_digits)
    
    effect_digits <- stri_replace(effect_digits, 
                                  fixed = '0.', 
                                  replacement = '.')
    
    return(paste(effect_txt, effect_digits))
    
  }
  
  corr_jps <- function(x) {
    
    ## formats the correlation coeffcient according to the JPS style
    ## takes a text entry and formats its numeric part
    
    stopifnot(is.character(x))
    
    effect_txt <- stri_extract(x, regex = '(r|(rho))\\s{1}=')
    
    effect_txt <- stri_replace(effect_txt, 
                               fixed = 'rho', 
                               replacement = '\u03C1')
    
    effect_digits <- stri_extract(x, regex = '\\d{1}\\.\\d+')
    
    effect_digits <- as.character(effect_digits)
    
    effect_digits <- stri_replace(effect_digits, 
                                  fixed = '0.', 
                                  replacement = '.')
    
    return(paste(effect_txt, effect_digits))
    
  }
  
  format_test_jps <- function(x, correlation = FALSE) {
    
    ## formats a table with statistical testing results as required by the JPS
    
    eff_fun <- if(correlation) corr_jps else eff_size_jps
    
    mutate(x, 
           significance = p_value_jps(p_adjusted), 
           eff_size = eff_fun(eff_size))
    
  }

# END -----