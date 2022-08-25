# Analysis of differences in mental health status as a function
# of baseline factors, symptoms and cardiopulmonary features
# responses: the concern, identity, time (ipq_sub1) and 
# the control, coherence (ipq_sub2) components of the IPQ questionnaire
# and the total IPQ item sum.
# Square roots of them used because of improved normality
# methods: 
# (1) uni-variate Gaussian-family modeling, 
# (2) multi-variate backward elimination Gaussian-family modeling
# (3) multi-variate elastic net modeling
# multi-variate models are validated by 10-fold CV

# tools -----

  library(plyr)
  library(tidyverse)
  library(stringi)
  library(lmqc)
  library(caret)
  library(caretExtra)
  library(soucer)
  library(glmnet)
  library(doParallel)

  insert_head()
  
  source_all('./tools/project_tools.R')
  
# common modeling globals ------
  
  insert_msg('Common modeling globals')
  
  mod <- list()

  mod$responses <- c(ipq_sub1 = 'sqrt_ipq_sub1', 
                     ipq_sub2 = 'sqrt_ipq_sub2', 
                     ipq_total = 'sqrt_ipq_total')
  
  ## tables for multi-variate modeling with complete cases only
  ## normalization with mean centering
  
  mod$multi_tbl <- cov_data$clear_data %>% 
    select(ID, 
           all_of(unname(mod$responses)), 
           all_of(globals$variables)) %>% 
    filter(complete.cases(.)) %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x) %>% 
    column_to_rownames('ID')

  ## full model formulas
  
  mod$multi_form <- mod$responses %>% 
    map(make_lm, 
        data = mod$multi_tbl, 
        indep_variable = globals$variables, 
        mod_fun = lm, 
        family = NULL) %>% 
    map(formula)
  
  ## training control object for caret models
  
  mod$tr_control <- trainControl(method = 'repeatedcv', 
                                 number = 10, 
                                 repeats = 10, 
                                 savePredictions = 'final')
  
  ## level n numbers
  
  mod$level_n <- set_names(globals$variables, 
                           globals$variables) %>% 
    map(function(x) if(is.factor(mod$multi_tbl[[x]])) count(mod$multi_tbl, .data[[x]]) else NULL) %>% 
    compact %>% 
    map(set_names, c('level', 'n')) %>% 
    map2_dfr(., names(.), ~mutate(.x, variable = .y)) %>% 
    as_tibble

  ## effect size color coding
  
  mod$size_colors <- c(none = 'gray60', 
                       weak = 'cornsilk', 
                       moderate = 'coral3', 
                       large = 'firebrick4')
  
# modeling scripts ------
  
  insert_msg('Aanlysis scripts')
  
  c('./modeling scripts/uni_variate.R', 
    './modeling scripts/elanet.R', 
    './modeling scripts/lasso.R',
    './modeling scripts/bayes_lasso.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ----
  
  insert_tail()