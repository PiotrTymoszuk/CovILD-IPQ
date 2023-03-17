# Detailed analysis of factor influencing illness perception
# identified by Elastic Net, LASSO and Bayesian Lasso modeling.
#
# 1) Fit stats (training and CV) are compared for each of the algorithms
# and factors influencing the total illness perceptions score and its sub-scores
# are identified as overlapping non-zero coefficients computed by all 
# three modeling algorithms. Note: no factor influencing the control/coherence
# component could be discerned (COVID-19-independent effects?)
#
# 2) Detailed analysis of the influential factors by correlation and statistical 
# hypothesis testing and analysis of their interaction with cardiopulmonary
# abnormalities (two-way ANOVA)
#
# 3) Unsupervised clustering of the study participants in respect 
# to the BIPQ items
#
# 4) Analysis of effects of the interaction between persistent somatic 
# symptoms and cardiopulmonary findings on IP; two-way ANOVA


# tools ------

  library(plyr)
  library(tidyverse)
  library(exda)
  library(stringi)
  library(soucer)
  library(clustTools)
  library(furrr)
  library(trafo)

  insert_head()
  
  explore <- exda::explore
  
  source_all('./tools/project_tools.R')

# analysis globals -----

  insert_msg('Analysis globals')

  an <- list()

  ## variables selected by each algorithm
  ## second and first term subsumed under one variable
  
  an$variables <- list(eln_mod = eln_mod, 
                       lasso_mod = lasso_mod, 
                       blass_mod = blass_mod) %>% 
    map(~.x$inference) %>% 
    transpose %>% 
    map(compact) %>% 
    map(~map(.x, filter, parameter != '(Intercept)')) %>% 
    map(~map(.x, ~.x$variable) %>% 
          map(stri_replace, fixed = '_sq', replacement = '') %>% 
          map(stri_replace, regex = '(log_)|(sqrt_)', replacement = '') %>% 
          map(unique))
  
  ## common variables selected by at least two penalized modeling methods
  ## determining the variable type and number of levels
  
  an$cmm_variables <- an$variables %>% 
    map(reduce, intersect)
  
  an$conf_variables <- c('age', 
                         'sex', 
                         'sympt_present', 
                         'lufo_red', 
                         'ct_severity_any', 
                         'diastolic_dysf', 
                         'cat_WHO')
  
  an$cmm_lexicon <- 
    tibble(variable = c(reduce(an$cmm_variables, union), 
                        an$conf_variables)) %>% 
    filter(!duplicated(variable)) %>% 
    mutate(numeric = map_lgl(variable, 
                             ~is.numeric(cov_data$clear_data[[.x]])), 
           plot_type = ifelse(numeric, 'correlation', 'violin'), 
           levels = map(variable, 
                        ~levels(cov_data$clear_data[[.x]])), 
           level_no = map_dbl(variable, 
                              ~length(levels(cov_data$clear_data[[.x]]))), 
           eff_type = ifelse(level_no == 2, 
                             'wilcoxon_r', 
                             ifelse(level_no > 2, 
                                    'kruskal_etasq', 
                                    'spearman')), 
           what = ifelse(numeric, 'correlation', 'eff_size'))

  ## responses
  
  an$responses <- c(ipq_sub1 = 'ipq_sub1', 
                    ipq_sub2 = 'ipq_sub2', 
                    ipq_total = 'ipq_total')
  
  ## analysis method labels
  
  an$methods <- c(eln_mod = 'ElasticNet',
                  lasso_mod = 'LASSO', 
                  blass_mod = 'Bayesian LASSO')
  
  an$method_colors <- c(eln_mod = 'darkolivegreen', 
                        lasso_mod = 'indianred', 
                        blass_mod = 'steelblue')
  
# analysis scripts -----
  
  insert_msg('Analysis scripts')
  
  c('./analysis scripts/plots.R', 
    './analysis scripts/ipq_sub1_factors.R',
    './analysis scripts/ipq_sub2_factors.R', 
    './analysis scripts/ipq_factors.R', 
    './analysis scripts/interaction.R', 
    './analysis scripts/ipq_clust_development.R', 
    './analysis scripts/ipq_clustering.R', 
    './analysis scripts/clust_characteristic.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()