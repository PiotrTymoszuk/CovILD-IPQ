# Exploratory data analysis
# Remova of participants with > 25% missing answers if any detected

# tools -------

  library(plyr)
  library(tidyverse)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(psych)
  library(clustTools)

  insert_head()
  
  source_all('./tools/project_tools.R')
  
# common variables and parameters ------
  
  insert_msg('Exploration globals')
  
  expl <- list()
  
  ## variables
  
  expl$variables <- list(baseline = c('sex', 
                                      'age', 
                                      'smoking_history', 
                                      'weight_class', 
                                      'comorb_present', 
                                      'no_comorb', 
                                      'cardiovascular_comorb', 
                                      'hypertension_comorb', 
                                      'endometabolic_comorb', 
                                      'hyperchol_comorb', 
                                      'diabetes_comorb', 
                                      'gastro_comorb', 
                                      'malingancy_comorb', 
                                      'respi_comorb', 
                                      'ckd_comorb', 
                                      'immdef_comorb', 
                                      'WHO', 
                                      'cat_WHO'), 
                         fup = c('sympt_present', 
                                 'sympt_number', 
                                 'fatigue_sympt', 
                                 'dyspnoe_sympt', 
                                 'cough_sympt', 
                                 'sleep_sympt', 
                                 'night_sweat_sympt', 
                                 'anosmia_sympt', 
                                 'derma_sympt', 
                                 'gastro_sympt', 
                                 'hair_loss_sympt', 
                                 'Chalder_FS', 
                                 'Chalder_FS_bimodal', 
                                 'lufo_red', 
                                 'ct_severity_any', 
                                 'ct_severity_score', 
                                 'diastolic_dysf', 
                                 'smwd', 
                                 'smwd_dref', 
                                 'rehabilitation'), 
                         ipq = c('ipq_total',
                                   'ipq_sub1',
                                   'ipq_sub2', 
                                   'ipq_q1', 
                                   'ipq_q2', 
                                   'ipq_q3', 
                                   'ipq_q4', 
                                   'ipq_q5', 
                                   'ipq_q6', 
                                   'ipq_q7', 
                                   'ipq_q8'), 
                         psych = globals$psy_variables, 
                         lab = c('anemia', 
                                 'Hb',  
                                 'ferritin', 
                                 'TSAT',  
                                 'sTFR', 
                                 'Hepcidin', 
                                 'NTproBNP', 
                                 'DDimer', 
                                 'CRP', 
                                 'HbA1c'))
  
  ## effect size, test and plot types
  
  expl$var_numeric <- expl$variables %>% 
    map(~cov_data$clear_data[.x]) %>% 
    map(~map_lgl(.x, is.numeric))
  
  expl$plot_type <- expl$var_numeric %>% 
    map(~ifelse(.x, 'violin', 'stack'))
  
  expl$test_type <- expl$var_numeric %>% 
    map(~ifelse(.x, 'kruskal_test', 'chisq_test'))
  
# analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./exploration scripts/distribution.R', 
    './exploration scripts/score_pca.R', 
    './exploration scripts/score_coherence.R', 
    './exploration scripts/psy_correlation.R', 
    './exploration scripts/cohort_characteristic.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ----
  
  insert_tail()