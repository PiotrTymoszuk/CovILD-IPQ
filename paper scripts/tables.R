# Paper and supplement tables

  insert_head()
  
# containers -----
  
  paper_tbl <- list()
  suppl_tbl <- list()

# Table 1: characteristic of the study cohort -----
  
  insert_msg('Table 1: baseline cohort characteristic')
  
  paper_tbl$baseline <- left_join(cohort$desc_stats$baseline, 
                                  cohort$test_results$baseline[c('variable', 
                                                                 'significance', 
                                                                 'eff_size')], 
                                  by = 'variable') %>% 
    filter(!variable %in% c('cat_WHO', 'no_comorb')) %>% 
    format_summ_tbl(rm_n = TRUE) %>% 
    rbind(tibble(variable = 'n, participants', 
                 cohort = pap$n_baseline$n[1], 
                 A = pap$n_baseline$n[2], 
                 HM = pap$n_baseline$n[3], 
                 HS = pap$n_baseline$n[4], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = ''), 
           variable = ifelse(variable == 'Sex', 'Male sex', variable)) %>%
    map_dfc(stri_replace, regex = '^.*\\nmale:\\s{1}', replacement = '') %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size'))
  
  paper_tbl$baseline <- 
    as_mdtable(paper_tbl$baseline , 
               label = 'cohort_baseline', 
               ref_name = 'cohort_baseline', 
               caption = 'Baseline demographic and clinical characteristic of the study cohort.')
  
# Table 2: IPQ components and sum in the cohort and severity groups -----
  
  insert_msg('IPQ in the severity groups and the cohort')
  
  paper_tbl$psych <- left_join(cohort$desc_stats$ipq, 
                               cohort$test_results$ipq[c('variable', 
                                                         'significance', 
                                                         'eff_size')], 
                               by = 'variable') %>% 
    filter(variable %in% c(paste0('ipq_q', 1:8), 'ipq_total')) %>% 
    format_summ_tbl(rm_n = TRUE) %>% 
    rbind(tibble(variable = 'n, participants', 
                 cohort = pap$n_baseline$n[1], 
                 A = pap$n_baseline$n[2], 
                 HM = pap$n_baseline$n[3], 
                 HS = pap$n_baseline$n[4], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size'))
  
  paper_tbl$psych <- 
    as_mdtable(paper_tbl$psych , 
               label = 'cohort_ipq', 
               ref_name = 'psych', 
               caption = 'Illness perception score and item values of the Brief Illness Perception Questionnaire (BIPQ, Q1 - Q8) at one year after COVID-19.')
  
# Supplementary Table S1: variable dictionary -------
  
  insert_msg('Table S1: variable dictionary')
  
  suppl_tbl$var_lexicon <- globals$var_lexicon %>% 
    filter(variable %in% c(globals$variables, 
                           paste0('ipq_q', 1:8), 
                           'ipq_total')) %>% 
    select(variable, label_long, unit, description) %>% 
    set_names(c('R name', 
                'Plot/table label', 
                'Unit', 
                'Description'))
  
  suppl_tbl$var_lexicon <- 
    as_mdtable(suppl_tbl$var_lexicon , 
               label = 'var_dict', 
               ref_name = 'var_dict', 
               caption = 'Study variables')
  
# Supplementary Table S2: COVID-19 symptoms, cardiopulmonary outcome, reha and IPQ, 1-year ----
  
  insert_msg('Table 2: FUP chaqracteristic of the cohort')
  
  suppl_tbl$fup <- left_join(cohort$desc_stats$fup, 
                             cohort$test_results$fup[c('variable', 
                                                       'significance', 
                                                       'eff_size')], 
                             by = 'variable') %>% 
    format_summ_tbl(rm_n = TRUE) %>% 
    rbind(tibble(variable = 'n, participants', 
                 cohort = pap$n_baseline$n[1], 
                 A = pap$n_baseline$n[2], 
                 HM = pap$n_baseline$n[3], 
                 HS = pap$n_baseline$n[4], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size'))
  
  suppl_tbl$fup <- 
    as_mdtable(suppl_tbl$fup, 
               label = 'cohort_fup', 
               ref_name = 'cohort_fup', 
               caption = 'COVID-19 symptoms, performance, fatigue, exertional capacity, cardiopulmonary abnormalities and illness perception at the one-year follow-up.')
  
# Supplementary Table S3: laboratory parameters at the follow-up ------
  
  insert_msg('Table S3: biochemistry at the follow-up')
  
  suppl_tbl$biochemistry_fup <- left_join(cohort$desc_stats$lab, 
                                          cohort$test_results$lab[c('variable', 
                                                                    'significance', 
                                                                    'eff_size')], 
                                          by = 'variable') %>% 
    format_summ_tbl(rm_n = TRUE) %>% 
    rbind(tibble(variable = 'n, participants', 
                 cohort = pap$n_baseline$n[1], 
                 A = pap$n_baseline$n[2], 
                 HM = pap$n_baseline$n[3], 
                 HS = pap$n_baseline$n[4], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = ifelse(stri_detect(variable, regex = 'HbA1c|TF-Sat'), 
                             variable, 
                             stri_replace(variable, 
                                          fixed = ', %', 
                                          replacement = ''))) %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size'))
  
  suppl_tbl$biochemistry_fup <- 
    as_mdtable(suppl_tbl$biochemistry_fup, 
               label = 'cohort_biochemistry', 
               ref_name = 'cohort_biochemistry', 
               caption = 'Laboratory parameters at the one-year follow-up.')
  
# Supplementary Table S4: differences between the IPQ clusters -----
  
  insert_msg('Supplementary Table S4: differences between the clusters')
  
  suppl_tbl$ipq_clusters <- clust_chara$test_results[c('baseline', 
                                                       'fup', 
                                                       'lab')] %>% 
    map_dfr(filter, p_value < 0.1) %>% 
    select(variable, significance, eff_size) %>% 
    right_join(clust_chara$desc_stats %>% 
                 reduce(rbind), ., 
               by = 'variable') %>% 
    format_summ_tbl(rm_n = TRUE) %>% 
    rbind(tibble(variable = 'n, participants', 
                 `#1` = pap$n_clust$n[1], 
                 `#2` = pap$n_clust$n[2], 
                 `#3` = pap$n_clust$n[3], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = ifelse(stri_detect(variable, regex = 'HbA1c|TF-Sat'), 
                             variable, 
                             stri_replace(variable, 
                                          fixed = ', %', 
                                          replacement = ''))) %>% 
    set_names(c('Variable', 
                'Cluster #1', 
                'Cluster #2', 
                'Cluster #3', 
                'Significance', 
                'Effect size'))
  
  suppl_tbl$ipq_clusters <- 
    as_mdtable(suppl_tbl$ipq_clusters, 
               label = 'ipq_clusters', 
               ref_name = 'ipq_clusters', 
               caption = 'Significant (p < 0.05) and near-significant (p < 0.1) differences between the illness perception clusters.')
  
# Saving the tables on the disc ------
  
  insert_msg('Saving the tables')
  
  paper_tbl %>% 
    set_names(paste('Table', 1:length(paper_tbl))) %>% 
    write_xlsx('./paper/tables.xlsx')
  
  suppl_tbl %>% 
    set_names(paste0('Table S', 1:length(suppl_tbl))) %>% 
    write_xlsx('./paper/supplementary_tables.xlsx')
  
# END ------
  
  insert_tail()