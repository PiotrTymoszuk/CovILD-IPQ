# Analyzing the interaction effects between the key factors
# affecting the overall illness perception score and the 
# emotion/concern/consequence sub-score with cardiopulmonary abnormality
# 
# Done with ANOVA/linear modeling with the interaction effect


  insert_head()
  
# container -----
  
  inter <- list()
  
# Analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## the key factors influencing the illness perception responses
  
  inter$variables <- an$cmm_variables[c('ipq_total',
                                        'ipq_sub1')] %>% 
    reduce(union)
  
  inter$variables <- set_names(inter$variables)

  ## the analysis table
  ## defining the cardiopulmonary abnormality variable
  
  inter$analysis_tbl <- cov_data$clear_data %>% 
    mutate(cardiopulmo = ifelse(ct_severity_any == 'yes' | 
                                  lufo_red == 'yes' |
                                  diastolic_dysf == 'yes', 
                                'yes', 'no')) %>% 
    select(sqrt_ipq_total, 
           sqrt_ipq_sub1, 
           ipq_total, 
           ipq_sub1, 
           ct_severity_any, 
           lufo_red, 
           diastolic_dysf, 
           all_of(unname(inter$variables)))
  
  ## variable type: numeric vs factor
  
  inter$var_types <- inter$analysis_tbl[inter$variables] %>% 
    map_lgl(is.numeric)
  
  ## model formulas with the interaction term
  
  inter$formulas_ct$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '* ct_severity_any')) %>% 
    map(as.formula)
  
  inter$formulas_ct$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '* ct_severity_any')) %>% 
    map(as.formula)
  
  inter$formulas_lufo$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '* lufo_red')) %>% 
    map(as.formula)
  
  inter$formulas_lufo$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '* lufo_red')) %>% 
    map(as.formula)
  
  inter$formulas_dia$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '* diastolic_dysf')) %>% 
    map(as.formula)
  
  inter$formulas_dia$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '* diastolic_dysf')) %>% 
    map(as.formula)
  
  ## model formulas without the interaction term
  
  inter$null_formulas_ct$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '+ ct_severity_any')) %>% 
    map(as.formula)
  
  inter$null_formulas_ct$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '+ ct_severity_any')) %>% 
    map(as.formula)
  
  inter$null_formulas_lufo$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '+ lufo_red')) %>% 
    map(as.formula)
  
  inter$null_formulas_lufo$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '+ lufo_red')) %>% 
    map(as.formula)
  
  inter$null_formulas_dia$ipq_total <- inter$variables %>% 
    map(~paste('sqrt_ipq_total ~', .x, '+ diastolic_dysf')) %>% 
    map(as.formula)
  
  inter$null_formulas_dia$ipq_sub1 <- inter$variables %>% 
    map(~paste('sqrt_ipq_sub1 ~', .x, '+ diastolic_dysf')) %>% 
    map(as.formula)
  
# Building the models -------
  
  insert_msg('Building the models')
  
  ## interaction models
  
  inter$models_ct <- inter$formulas_ct %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))

  inter$models_lufo <- inter$formulas_lufo %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))
  
  inter$models_dia <- inter$formulas_dia %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))
  
  ## no-interaction models: they will be used later in LRT
  
  inter$null_models_ct <- inter$null_formulas_ct %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))
  
  inter$null_models_lufo <- inter$null_formulas_lufo %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))
  
  inter$null_models_dia <- inter$null_formulas_dia %>% 
    map(~map(.x, 
             ~make_lm(inter$analysis_tbl, 
                      formula = .x, 
                      mod_fun = lm, 
                      family = NULL)))
  
# Model assumptions -------
  
  insert_msg('Model assumptions')
  
  ## Shapiro-Wilk and Levene tests
  
  inter$assumptions_ct <- inter$models_ct %>% 
    map(~map(.x, 
             summary, 
             'assumptions', 
             type.predict = 'response'))
  
  inter$assumptions_lufo <- inter$models_lufo %>% 
    map(~map(.x, 
             summary, 
             'assumptions', 
             type.predict = 'response'))
  
  inter$assumptions_dia <- inter$models_dia %>% 
    map(~map(.x, 
             summary, 
             'assumptions', 
             type.predict = 'response'))
  
  ## plots of residuals
  
  inter$resid_plots_ct <- inter$models_ct %>% 
    map(~map(.x, 
             plot, 
             type.predict = 'response', 
             cust_theme = globals$common_theme))
  
  inter$resid_plots_lufo <- inter$models_lufo %>% 
    map(~map(.x, 
             plot, 
             type.predict = 'response', 
             cust_theme = globals$common_theme))
  
  inter$resid_plots_dia <- inter$models_dia %>% 
    map(~map(.x, 
             plot, 
             type.predict = 'response', 
             cust_theme = globals$common_theme))
  
# Fit stats -------
  
  insert_msg('Fit stats')
  
  inter$fit_stats_ct <- inter$models_ct %>% 
    map(~map(.x, 
             summary, 'fit') %>% 
          map2_dfr(., names(.), 
                   ~mutate(.x, variable = .y)))
  
  inter$fit_stats_lufo <- inter$models_lufo %>% 
    map(~map(.x, 
             summary, 'fit') %>% 
          map2_dfr(., names(.), 
                   ~mutate(.x, variable = .y)))
  
  inter$fit_stats_dia <- inter$models_dia %>% 
    map(~map(.x, 
             summary, 'fit') %>% 
          map2_dfr(., names(.), 
                   ~mutate(.x, variable = .y)))

# Likelihood ratio test for significance of the interaction term ------
  
  insert_msg('LRT')
  
  ## note: they may deliver a false-positive result (p < 0.05)
  ## for level-deficient models!!!

  inter$lrt_ct <- map2(inter$models_ct, 
                       inter$null_models_ct,
                       ~map2(.x, .y, ~anova(.x$model, .y$model)) %>% 
                         map(as.data.frame) %>% 
                         map(~.x[2, ]) %>% 
                         map2_dfr(., names(.), 
                                  ~mutate(.x, variable = .y))) %>% 
    map(as_tibble)
  
  inter$lrt_lufo <- map2(inter$models_lufo, 
                         inter$null_models_lufo,
                         ~map2(.x, .y, ~anova(.x$model, .y$model)) %>% 
                           map(as.data.frame) %>% 
                           map(~.x[2, ]) %>% 
                           map2_dfr(., names(.), 
                                    ~mutate(.x, variable = .y))) %>% 
    map(as_tibble)
  
  inter$lrt_dia <- map2(inter$models_dia, 
                        inter$null_models_dia,
                        ~map2(.x, .y, ~anova(.x$model, .y$model)) %>% 
                          map(as.data.frame) %>% 
                          map(~.x[2, ]) %>% 
                          map2_dfr(., names(.), 
                                   ~mutate(.x, variable = .y))) %>% 
    map(as_tibble)

# Model inference -------
  
  insert_msg('Model inference')
  
  inter$inference_ct <- inter$models_ct %>% 
    map(~map(.x, 
             summary, 'inference'))
  
  inter$inference_lufo <- inter$models_lufo %>% 
    map(~map(.x, 
             summary, 'inference'))
  
  inter$inference_dia <- inter$models_dia %>% 
    map(~map(.x, 
             summary, 'inference'))
  
# Two-way ANOVA -------
  
  insert_msg('Two-way ANOVA')
  
  inter$anova_ct <- inter$models_ct %>% 
    map(~map(.x, anova))
  
  inter$anova_lufo <- inter$models_lufo %>% 
    map(~map(.x, anova))
  
  inter$anova_dia <- inter$models_dia %>% 
    map(~map(.x, anova))
  
# Ready to use plot tags with the ANOVA results -------
  
  insert_msg('Plot tags with ANOVA results')
    
  ## with the effects sizes (frac. explained variance or eta-sqared)
  ## and p values
  
  inter[c('anova_summary_ct', 
          'anova_summary_lufo', 
          'anova_summary_dia')] <- inter[c('anova_ct', 
                                           'anova_lufo', 
                                           'anova_dia')] %>% 
    map(~map(.x, ~map(.x, filter, variable != 'Residuals'))) %>% 
    map(~map(.x, ~map(.x, mutate, 
                      var_lab = ifelse(stri_detect(variable, fixed = ':'), 
                                       'Interaction', 
                                       translate_var(variable)), 
                      eff_size = paste('\u03B7\u00B2 = ', 
                                       signif(frac_explained, 2)), 
                      significance = ifelse(`Pr(>F)` > 0.1, 
                                            'ns', 
                                            ifelse(`Pr(>F)` >= 0.05, 
                                                   paste0('ns (p = ', 
                                                          signif(`Pr(>F)`, 2), 
                                                          ')'), 
                                                   paste('p =', 
                                                         signif(`Pr(>F)`, 2)))), 
                      plot_cap = paste(eff_size, significance))))
  
  inter[c('plot_tags_ct', 
          'plot_tags_lufo', 
          'plot_tags_dia')] <- inter[c('anova_summary_ct', 
                                       'anova_summary_lufo', 
                                       'anova_summary_dia')] %>% 
    map(~map(.x, 
             ~map(.x, 
                  mutate, 
                  plot_tag = paste0(var_lab, ': ', 
                                    eff_size, ', ', significance)) %>% 
               map(~.x$plot_tag) %>% 
               map(paste, collapse = '\n') %>% 
               map(~paste0('\n', .x))))
  
# Labellers with N numbers for the plots of numeric variables -----
  
  insert_msg('Lablellers for plots of numeric variables')
  
  inter$numeric_labellers_ct <- inter$analysis_tbl %>% 
    count(ct_severity_any) %>% 
    .$n %>% 
    map2_chr(c('no CT abnormality', 
               'CT abnormality'), ., 
             paste, sep = ', n = ') %>% 
    set_names(c('no', 'yes'))
  
  inter$numeric_labellers_lufo <- inter$analysis_tbl %>% 
    count(lufo_red) %>% 
    .$n %>% 
    map2_chr(c('no LFT abnormality', 
               'LFT abnormality'), ., 
             paste, sep = ', n = ') %>% 
    set_names(c('no', 'yes'))
  
  inter$numeric_labellers_dia <- inter$analysis_tbl %>% 
    count(diastolic_dysf) %>% 
    .$n %>% 
    map2_chr(c('no diastolic dysfunction', 
               'diastolic dysfunction'), ., 
             paste, sep = ', n = ') %>% 
    set_names(c('no', 'yes'))  
  
# Plots for numeric variables ------
  
  insert_msg('Plots for numueric variables')
  
  inter$cor_plots_ct <- list(plot_resp = c(ipq_total = 'ipq_total', 
                                           ipq_sub1 = 'ipq_sub1'), 
                             tag = inter$plot_tags_ct) %>% 
    pmap(function(plot_resp, tag) list(num_variable = inter$variables[inter$var_types], 
                                       plot_tag = tag[inter$var_types]) %>% 
           pmap(two_way_corr, 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'ct_severity_any', 
                labeller = as_labeller(inter$numeric_labellers_ct), 
                method = 'lm', 
                color = 'black'))
  
  inter$cor_plots_lufo <- list(plot_resp = c(ipq_total = 'ipq_total', 
                                             ipq_sub1 = 'ipq_sub1'), 
                               tag = inter$plot_tags_lufo) %>% 
    pmap(function(plot_resp, tag) list(num_variable = inter$variables[inter$var_types], 
                                       plot_tag = tag[inter$var_types]) %>% 
           pmap(two_way_corr, 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'lufo_red', 
                labeller = as_labeller(inter$numeric_labellers_lufo), 
                method = 'lm', 
                color = 'black'))
  
  inter$cor_plots_dia <- list(plot_resp = c(ipq_total = 'ipq_total', 
                                            ipq_sub1 = 'ipq_sub1'), 
                              tag = inter$plot_tags_dia) %>% 
    pmap(function(plot_resp, tag) list(num_variable = inter$variables[inter$var_types], 
                                       plot_tag = tag[inter$var_types]) %>% 
           pmap(two_way_corr, 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'diastolic_dysf', 
                labeller = as_labeller(inter$numeric_labellers_dia), 
                method = 'lm', 
                color = 'black'))
  
# Plots for categorical variables ------
  
  insert_msg('Plots for categorical variables')
  
  inter$violin_plots_ct <- 
    list(plot_resp = c(ipq_total = 'ipq_total', 
                       ipq_sub1 = 'ipq_sub1'), 
         tag = inter$plot_tags_ct) %>% 
    pmap(function(plot_resp, tag) list(fct_variable = inter$variables[!inter$var_types], 
                                       plot_tag = tag[!inter$var_types]) %>% 
           pmap(two_way_violin, 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'ct_severity_any'))
  
  inter$violin_plots_lufo <- 
    list(plot_resp = c(ipq_total = 'ipq_total', 
                       ipq_sub1 = 'ipq_sub1'), 
         tag = inter$plot_tags_lufo) %>% 
    pmap(function(plot_resp, tag) list(fct_variable = inter$variables[!inter$var_types], 
                                       plot_tag = tag[!inter$var_types]) %>% 
           pmap(two_way_violin, 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'lufo_red'))
  
  ## the diastolic dysfunction - diastolic dysfunction plot makes no sense
  ## hence working with safely()
  
  inter$violin_plots_dia <- 
    list(plot_resp = c(ipq_total = 'ipq_total', 
                       ipq_sub1 = 'ipq_sub1'), 
         tag = inter$plot_tags_dia) %>% 
    pmap(function(plot_resp, tag) list(fct_variable = inter$variables[!inter$var_types], 
                                       plot_tag = tag[!inter$var_types]) %>% 
           pmap(safely(two_way_violin), 
                data = inter$analysis_tbl, 
                response = plot_resp, 
                split_factor = 'diastolic_dysf') %>% 
           map(~.x$result) %>% 
           compact)
  
# END ------
  
  insert_tail()