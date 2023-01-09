# Import of the patient's data cleared before during the COVILD-12 month project
# See: https://github.com/PiotrTymoszuk/CovILD-Plus for the import script code
# A subset of participants is selected with at least 
# one COVID-19-related symptom

# tools -------

  library(plyr)
  library(tidyverse)
  library(readxl)
  library(stringi)
  library(soucer)
  library(rlang)

  insert_head()
  
  source_all('./tools/project_tools.R')
  
# containers -----
  
  cov_data <- list()
  globals <- list()
  
# Setup of globals -------
  
  insert_msg('Graphics globals setup')
  
  ## modeling variables
  
  globals$variables <- c('sex', 
                         'age', 
                         'age_sq', 
                         'smoking_history', 
                         'weight_class', 
                         'comorb_present', 
                         'no_comorb', 
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
                         'WHO', 
                         'sympt_present', 
                         'sympt_number', 
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
                         'ct_severity_score',
                         'ctss_class', 
                         'diastolic_dysf', 
                         'rehabilitation', 
                         'anemia', 
                         'Hb', 'Hb_sq', 
                         'ferritin', 
                         'log_ferritin', 
                         'log_ferritin_sq', 
                         'TSAT', 
                         'sqrt_TSAT', 
                         'sqrt_TSAT_sq', 
                         'sTFR', 
                         'log_sTFR', 
                         'log_sTFR_sq', 
                         'Hepcidin', 
                         'sqrt_Hepcidin', 
                         'sqrt_Hepcidin_sq', 
                         'NTproBNP', 
                         'NTproBNP_class', 
                         'DDimer', 
                         'DDimer_class', 
                         'CRP', 
                         'CRP_class', 
                         'HbA1c', 
                         'HbA1c_class')
  
  globals$psy_variables <- c('EQ5DL_p', 
                             'EQ5DL_mobility', 
                             'EQ5DL_selfcare', 
                             'EQ5DL_activities', 
                             'EQ5DL_pain', 
                             'EQ5DL_anxiety', 
                             'SSD12',
                             'Stress', 
                             'BRCS', 
                             'SES', 
                             'SOCL9')

  ## graphics
  
  globals$corr_colors <- c('negative' = 'steelblue4', 
                           'positive' = 'firebrick4', 
                           'ns' = 'gray60')
  
  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
  ## patient severity group colors and labels
  
  globals$sev_colors <- c(A = 'steelblue', 
                          HM = 'gray50', 
                          HS = 'coral4', 
                          cohort = 'darkolivegreen4')
  
  globals$sev_labels <- c(A = 'Ambulatory', 
                          HM = 'Moderate', 
                          HS = 'Severe', 
                          cohort = 'Cohort')
  
  ## IPQ subscore names
  
  globals$ipq_sublabs <- c(ipq_q1 = 'Q1 consequences', 
                           ipq_q2 = 'Q2 timeline', 
                           ipq_q3 = 'Q3 lacking pers. control', 
                           ipq_q4 = 'Q4 lacking treat. control', 
                           ipq_q5 = 'Q5 identity', 
                           ipq_q6 = 'Q6 concern', 
                           ipq_q7 = 'Q7 lacking coherence', 
                           ipq_q8 = 'Q8 emotional repres.')
  
  ## IPQ score and subscore labels and colors
  
  globals$ipq_labs <- 
    c(ipq_total = 'Illness perception score (BIPQ sum)', 
      ipq_sub1 = 'Emotion/concern/consequences subscore (BIPQ 1/2/5/6/8)', 
      ipq_sub2 = 'Control/coherence subscore (BIPQ 3/4/7)')
  
  globals$ipq_colors <- c(ipq_total = 'darkolivegreen4', 
                          ipq_sub1 = 'coral3', 
                          ipq_sub2 = 'steelblue')
  
  ## cluster colors
  
  globals$clust_colors <- c('darkolivegreen3', 
                            'steelblue', 
                            'gray60')
  
# reading the data, IPQ scores and variable lexicons -------

  insert_msg('Reading the data')
  
  ## main data set
  
  load('./data/covild_12.RDa')

  ## variable lexicon
  
  globals$var_lexicon <- read_excel('./data/var_lexicon.xlsx') %>% 
    mutate(label = stri_replace(label, 
                                fixed = '\\u00B2', 
                                replacement = '\u00B2'), 
           label_long = stri_replace(label_long, 
                                     fixed = '\\u00B2', 
                                     replacement = '\u00B2'), 
           axis_lab = ifelse(!is.na(unit), 
                             paste(label, unit, sep = ', '), 
                             label), 
           axis_lab_long = ifelse(!is.na(unit), 
                                  paste(label_long, unit, sep = ', '), 
                                  label_long))
  
  ## single IPQ scores, inverting the items 3, 4 and 7, as described 
  ## by Broadbent and colleagues
  
  cov_data$ipq_subscores <- read_excel('./data/ipq_sub.xlsx') %>% 
    select(- Folder) %>% 
    set_names(c('ID', paste0('ipq_q', 1:8)))
  
# a data frame with longitudinal symptom data ---------
  
  insert_msg('Longitudinal symptom data')
  
  ## it will be used later to check the likelihood
  ## that the symptoms are COVID-19-specific
  ## NA is handled as symptom absence
  
  ## symptoms recorded and present at all follow-ups
  
  cov_data$symptoms <- c('sleep_sympt', 
                         'dyspnoe_sympt', 
                         'cough_sympt', 
                         'gastro_sympt', 
                         'anosmia_sympt', 
                         'night_sweat_sympt', 
                         'fatigue_sympt')
  
  cov_data$sympt_data <- cov_data$data_tbl %>% 
    select(ID, time, all_of(cov_data$symptoms))
  
  for(i in cov_data$symptoms) {
    
    cov_data$sympt_data <- cov_data$sympt_data %>% 
      mutate(!!i := ifelse(is.na(.data[[i]]), 'no', as.character(.data[[i]])), 
             !!i := factor(.data[[i]], c('no', 'yes')))
    
  }
    
# main data set, additional, project-specific wrangling steps -----
  
  insert_msg('Wrangling of the main patient data set')
  
  ## forward feeding of the missing CT score data, 
  ## CT abnormality, LFT deficits and laboratory values
  ## if complete recovery was observed at the 6 month visit and the one-year
  ## follow-up CT is missing, no abnormality is assumed
  
  for(i in c('ct_severity_score', 'ct_severity_any', 'lufo_red', 
             'anemia', 
             'Hb',  
             'ferritin',  
             'TSAT',  
             'sTFR',  
             'Hepcidin',  
             'NTproBNP',  
             'DDimer',  
             'CRP', 
             'HbA1c')) {
    
    cov_data$data_tbl <- cov_data$data_tbl %>% 
      ddply(.(ID), fill_forward, variable = i)
    
  }
  
  ## formatting and numeric variable power
  ## one-year data
  ## missing SMWD: replaced by the expected reference value
  ## missing CFS: replaced by the median
  ## missing rehabilitation data, assumed no rehabilitation

  cov_data$data_tbl <- cov_data$data_tbl %>% 
    as_tibble %>% 
    filter(time == 4) %>% ## one year follow-up data only 
    mutate(respi_comorb = ifelse(pulmonary_comorb == 'yes' | 
                                   copd_comorb == 'yes' |
                                   asthma_comorb == 'yes'|
                                   cldis_comorb == 'yes'|
                                   intenst_lung_comorb == 'yes', 
                                 'yes', 'no'), 
           respi_comorb = factor(respi_comorb, c('no', 'yes')), 
           rehabilitation = ifelse(rehabilitation != 'no', 
                                   'yes', as.character(rehabilitation)), 
           rehabilitation = ifelse(is.na(rehabilitation), 'no', rehabilitation), 
           rehabilitation = factor(rehabilitation, c('no', 'yes')), 
           smoking_history = ifelse(smoking == 'never', 'no', 'yes'), 
           smoking_history = factor(smoking_history, c('no', 'yes')), 
           no_comorb_class = cut(no_comorb, 
                                 c(-Inf, 0, 3, Inf), 
                                 c('0', '1-3', '4+')), 
           age_sq = age^2, 
           smwd = ifelse(is.na(smwd), smwd_ref, smwd), 
           smwd_sq = smwd^2, 
           smwd_dref = smwd - smwd_ref, 
           smwd_dref_sq = smwd_dref^2, 
           Chalder_FS = ifelse(is.na(Chalder_FS), 
                               median(Chalder_FS, na.rm = TRUE), 
                               Chalder_FS), 
           Chalder_FS_sq = Chalder_FS^2, 
           Hb_sq = Hb^2, 
           ctss_class = cut(ct_severity_score, 
                            c(-Inf, 0, 5, 10, Inf), 
                            c('none', 'mild', 'moderate', 'severe')), 
           log_ferritin = log(ferritin), 
           log_ferritin_sq = log_ferritin^2, 
           sqrt_TSAT = sqrt(TSAT), 
           sqrt_TSAT_sq = sqrt_TSAT^2, 
           log_sTFR = log(sTFR), 
           log_sTFR_sq = log_sTFR^2, 
           sqrt_Hepcidin = sqrt(Hepcidin), 
           sqrt_Hepcidin_sq = sqrt_Hepcidin^2, 
           NTproBNP_class = cut(NTproBNP, 
                                c(-Inf, 125, Inf), 
                                c('normal', '>125 pg/mL')), 
           CRP_class = cut(CRP, 
                           c(-Inf, 0.5, Inf), 
                           c('normal', '>0.5 mg/L')), 
           DDimer_class = cut(DDimer, 
                              c(-Inf, 460, Inf), 
                              c('normal', '>460 µg/L')), 
           HbA1c_class = cut(HbA1c, 
                             c(-Inf, 5.7, Inf), 
                             c('normal', '>5.7%')))

  ## handling the symptoms: NA is treated as symptom-free
  ## significant fatigue defined as a symptom
  
  cov_data$symptoms <- cov_data$data_tbl %>% 
    select(ends_with('sympt'), 
           Chalder_FS_bimodal) %>% 
    names
  
  for(i in cov_data$symptoms) {
    
    cov_data$data_tbl <- cov_data$data_tbl %>% 
      mutate(!!i := ifelse(is.na(.data[[i]]), 'no', as.character(.data[[i]])), 
             !!i := factor(.data[[i]], c('no', 'yes')))
    
  }
  
  ## symptom sum, including dermatological ones, hair loss
  ## and significant fatigue as defined by bimodal CFS
  
  cov_data$data_tbl$sympt_number <- cov_data$data_tbl %>% 
    select(cov_data$symptoms) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    map_dfc(~ifelse(is.na(.x), 0, .x)) %>% 
    reduce(`+`)
  
  cov_data$data_tbl <- cov_data$data_tbl %>% 
    mutate(sympt_present = ifelse(sympt_number > 0, 'yes', 'no'), 
           sympt_present = factor(sympt_present, c('no', 'yes')), 
           sympt_number_class = cut(sympt_number, 
                                    c(-Inf, 0, 1, 3, Inf), 
                                    c('0', '1', '2-3', '4+')))
  
  ## selection of the patients, who have ongoing symptoms
  ## or a CT abnormality or an LFT deficit or diastolic dysfunction
  
  ## assigning the raw data set for analysis of excluded participants
  
  cov_data$raw_data <- cov_data$data_tbl
  
  cov_data$clear_data <- cov_data$data_tbl %>% 
    filter(sympt_present == 'yes' | 
             ct_severity_any == 'yes' | 
             lufo_red == 'yes' | 
             diastolic_dysf == 'yes')
  
  cov_data$cov_residuals <- cov_data$clear_data$ID

# clearing the IPQ score data -----
  
  insert_msg('Clearing the IPQ score data')

  ## coercion and inversion of the items 3, 4 and 7 for the score calculation
  
  for(i in paste0('ipq_q', 1:8)) {
    
    cov_data$ipq_subscores <- cov_data$ipq_subscores %>% 
      mutate(!!i := as.numeric(.data[[i]]))
    
  }
  
  for(i in c('ipq_q3', 'ipq_q4', 'ipq_q7')) {
    
    cov_data$ipq_subscores <- cov_data$ipq_subscores %>% 
      mutate(!!i := abs(10 - .data[[i]]))
    
  }
  
  ## removal of incomplete answers
  
  cov_data$ipq_subscores <- cov_data$ipq_subscores %>% 
    filter(complete.cases(.), 
           ID %in% cov_data$cov_residuals)
  
  cov_data$ipq_complete <- cov_data$ipq_subscores$ID

  ## merging with the main data set
  ## elimination of the participants with incomplete IPQ answers
  ## from the main data set

  cov_data$clear_data <- right_join(cov_data$clear_data, 
                                    cov_data$ipq_subscores, 
                                    by = 'ID')
  
  ## calculation of the total IPQ score
  ## and scores for the coherence/control 
  ## and consequences/concern/time
  
  cov_data$clear_data$ipq_total <- cov_data$clear_data %>% 
    select(starts_with('ipq_q')) %>% 
    reduce(`+`)
  
  cov_data$clear_data$ipq_sub1 <- cov_data$clear_data %>% 
    select(paste0('ipq_q', c(1, 2, 5, 6, 8))) %>% 
    reduce(`+`)
  
  cov_data$clear_data$ipq_sub2 <- cov_data$clear_data %>% 
    select(paste0('ipq_q', c(3, 4, 7))) %>% 
    reduce(`+`)
  
  ## calculation of the sub-score transformations

  cov_data$clear_data <- cov_data$clear_data %>% 
    mutate(log_ipq_total = log(ipq_total + 1), 
           sqrt_ipq_total = sqrt(ipq_total), 
           log_ipq_sub1 = log(ipq_sub1 + 1), 
           sqrt_ipq_sub1 = sqrt(ipq_sub1), 
           log_ipq_sub2 = log(ipq_sub2 + 1), 
           sqrt_ipq_sub2 = sqrt(ipq_sub2))

# Variable missingness analysis ------
  
  insert_msg('Variable missingness')
  
  source_all('./exploration scripts/missingness.R')
  
# Removing participants with missing modeling data -------
  
  insert_msg('Removing participants with missing modeling data')
  
  cov_data$clear_data <- cov_data$clear_data %>% 
    select(ID, 
           starts_with('ipq_'), 
           starts_with('log_'), 
           starts_with('sqrt_'), 
           all_of(globals$variables)) %>% 
    filter(complete.cases(.))
  
  cov_data$data_tbl <- cov_data$data_tbl %>% 
    filter(ID %in% cov_data$clear_data$ID)
  
  cov_data$var_complete <- cov_data$clear_data$ID
  
  ## appending the data set with additional psych variables
  
  cov_data$clear_data <- 
    left_join(cov_data$clear_data, 
              cov_data$data_tbl[c('ID', globals$psy_variables)], 
              by = 'ID')
  
  ## filtering the longitudinal symptom data
  
  cov_data$sympt_data <- cov_data$sympt_data %>% 
    filter(ID %in% cov_data$clear_data$ID)
  
# END -----
  
  cov_data <- cov_data[c('data_tbl', 
                         'raw_data', 
                         'sympt_data', 
                         'clear_data', 
                         'year_complete', 
                         'cov_residuals', 
                         'ipq_complete', 
                         'var_complete')]
  
  rm(i)
  
  insert_tail()