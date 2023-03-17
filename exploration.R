# Exploratory data analysis
# Removal of participants with > 25% missing answers if any detected 
# (done during data import, see: 'import.R')
#
# 1) Characteristic of the study collective, i.e. participants with long-term 
# sequelae of COVID-19 
# (symptoms or cardiopulmonary findings at the one-year FUP).
#
# 2) Comparison of the individuals included in the analysis and excluded due to
# variable missingness, missing BIPQ and lack of long term-sequelae.
#
# 3) Comparison of baseline, recovery and mental health features between 
# the genders done for the participants fulfilling the inclusion criteria
#
# 4) Analysis of distribution (normality/homogeneity) of the modeling responses
# and explanatory variables
#
# 5) Correlation of illness perception score, subscores and particular items
# with other readouts of mental health (stress, anxiety, depression, 
# somatization and others)
#
# 6) Dimensionality of the BIPQ tool investigated by median centered PCA 
# (to get some idea on number of dimensions) and factor analysis
#
# 7) Analysis of the internal consistency of the BIPQ tool with McDonald omega
#
# 8) analysis of overlap between the cardiopulmonary findings and symptoms at 
# the one year follow-up
#
# 9) Characteristic of patients with and without respiratory comorbidity 
#
# 10) Comparison of the BIPQ consistency in all patients and the patients 
# included in the analysis with McDonald omega, as describe above
#
# 11) Kinetics of symptoms in participants included in the analysis
#
# 12) Dimensionality and McDonald's omega for the illness perception dataset
# by Bierbauer et al. 2022 (DOI: 10.1080/23311908.2022.2105007)

# tools -------

  library(plyr)
  library(tidyverse)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(psych)
  library(clustTools)
  library(ggvenn)
  library(kinet)
  library(rstatix)
  library(trafo)

  insert_head()
  
  explore <- exda::explore
  
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
  
  c('./exploration scripts/cohort_characteristic.R', 
    './exploration scripts/included_excluded.R', 
    './exploration scripts/gender.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/psy_correlation.R', 
    './exploration scripts/score_pca.R', 
    './exploration scripts/score_fa.R', 
    './exploration scripts/score_coherence.R', 
    './exploration scripts/overlap.R', 
    './exploration scripts/respiratory.R', 
    './exploration scripts/omega_excluded.R', 
    './exploration scripts/symptom_kinetic.R', 
    './exploration scripts/bierbauer2022.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END ----
  
  insert_tail()