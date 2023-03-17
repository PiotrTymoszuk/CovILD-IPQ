# Analysis of differences in mental health status as a function
# of baseline factors, symptoms and cardiopulmonary features
# responses: the emotion. concern, consequences (ipq_sub1) and 
# the control, coherence (ipq_sub2) components of the IPQ questionnaire
# and the total IPQ item sum.
# Square roots of them used because of improved normality
# methods: 
# (1) uni-variable Gaussian-family modeling, 
#
# (2) multi-parameter linear modeling with backward elimination 
# (just to show that it is badly over-parameterized)
#
# (3) multi-parameter Elastic Net modeling
#
# (4) multi-parameter LASSO
#
# (5) multi-parameter Bayesian LASSO
#
# (4) multi-variate elastic net modeling
#
# Numeric explanatory variables are normalized and mean centered and their 
# second order terms are included to account for curvature of the 
# relationship with the reponse
#
# Tuning of lambda is accomplished by 10-fold cross validation 
# repeated 200 times (Elastic Net and LASSO). The optimal lambda associated 
# with the lowest cross-validation error is selected from the set of lambdas 
# of maximal regularization models (parameter: 'lambda.1se'). Note: using 
# 'lambda.min' (minimal model deviance) results in an expansion of 
# cross-validation error suggestive of over-parameterization of the model.
#
# Multi-variate models are validated by 10-fold CV using caret.

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
  library(trafo)

  insert_head()
  
  explore <- exda::explore
  train <- caret::train
  
  source_all('./tools/project_tools.R')
  
# common modeling globals ------
  
  insert_msg('Common modeling globals')
  
  mod <- list()

  mod$responses <- c(ipq_sub1 = 'sqrt_ipq_sub1', 
                     ipq_sub2 = 'sqrt_ipq_sub2', 
                     ipq_total = 'sqrt_ipq_total')
  
  ## explanatory variables. As defined in the globals list
  ## WHO ordinal score is removed since this variable is not really quantitative
  ## including variables with transformations improving normality and 
  ## second order terms
  ##
  ## Note: variables with strong non-normality are stratified with usual cutoffs
  
  mod$variables <- c('sex', 
                     'age', 
                     'age_sq', 
                     'smoking_history', 
                     'weight_class', 
                     'comorb_present', 
                     'no_comorb_class', 
                     'cardiovascular_comorb', 
                     'hypertension_comorb', 
                     'endometabolic_comorb', 
                     'hyperchol_comorb', 
                     'diabetes_comorb', 
                     'respi_comorb', 
                     'ckd_comorb', 
                     'gastro_comorb', 
                     'malingancy_comorb', 
                     'immdef_comorb', 
                     'cat_WHO', 
                     'sympt_present', 
                     'sympt_number_class', 
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
                     'Chalder_FS_sq', 
                     'Chalder_FS_bimodal', 
                     'smwd', 
                     'smwd_sq', 
                     'smwd_dref', 
                     'smwd_dref_sq', 
                     'lufo_red', 
                     'ct_severity_any', 
                     'ctss_class', 
                     'diastolic_dysf', 
                     'rehabilitation', 
                     'anemia', 
                     'Hb', 
                     'Hb_sq', 
                     'log_ferritin', 
                     'log_ferritin_sq', 
                     'sqrt_TSAT', 
                     'sqrt_TSAT_sq', 
                     'log_sTFR', 
                     'log_sTFR_sq', 
                     'sqrt_Hepcidin', 
                     'sqrt_Hepcidin_sq', 
                     'NTproBNP_class', 
                     'DDimer_class', 
                     'CRP_class', 
                     'HbA1c_class')
  
  ## tables for multi-variate modeling with complete cases only
  ## normalization with mean centering
  
  mod$multi_tbl <- cov_data$clear_data %>% 
    select(ID, 
           all_of(unname(mod$responses)), 
           all_of(mod$variables)) %>% 
    filter(complete.cases(.)) %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x) %>% 
    column_to_rownames('ID')

  ## full model formulas
  
  mod$multi_form <- mod$responses %>% 
    map(make_lm, 
        data = mod$multi_tbl, 
        indep_variable = mod$variables, 
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
  
  insert_msg('Analysis scripts')
  
  ## working with cache (time costly lambda turning)

  if(file.exists('./cache/modeling.RData')) {
    
    load('./cache/modeling.RData')
    
  } else {
    
    c('./modeling scripts/uni_variate.R', 
      './modeling scripts/elimination.R', 
      './modeling scripts/elanet.R', 
      './modeling scripts/lasso.R',
      './modeling scripts/bayes_lasso.R') %>% 
      source_all(message = TRUE, crash = TRUE)
   
    save(uni_mod, 
         eli_mod, 
         eln_mod, 
         lasso_mod, 
         blass_mod, 
         file = './cache/modeling.RData')
     
  }
  
# END ----
  
  insert_tail()