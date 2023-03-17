# Paper and supplement tables

  insert_head()
  
# containers -----
  
  paper_tbl <- list()
  suppl_tbl <- list()
  
  rev_tables <- list() ## tables for the reviewer

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
               caption = paste('Baseline demographic and clinical', 
                               'characteristic of the study cohort.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
# Table 2: IPQ components and sum in the cohort and severity groups -----
  
  insert_msg('IPQ in the severity groups and the cohort')
  
  paper_tbl$psych <- left_join(cohort$desc_stats$ipq, 
                               cohort$test_results$ipq[c('variable', 
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
  
  paper_tbl$psych <- 
    as_mdtable(paper_tbl$psych , 
               label = 'cohort_ipq', 
               ref_name = 'psych', 
               caption = paste('Illness perception score', 
                               '(sum of all BIPQ items),', 
                               'emotion/concern/consequences subscore', 
                               '(sum of items 1, 2, 5, 6 and 8)', 
                               'control/coherence subscore', 
                               '(sum of items 3, 4 and 7) and values of sigle', 
                               'Brief Illness Perception Questionnaire items', 
                               '(BIPQ, Q1 - Q8) at one year after COVID-19.', 
                               'Values are presented as medians with', 
                               'interquartile ranges (IQR).'))
  
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
  
  ## adding the levels
  
  suppl_tbl$var_lexicon <- suppl_tbl$var_lexicon$`R name` %>% 
    map(~levels(cov_data$clear_data[[.x]])) %>% 
    map(~ifelse(is.null(.x), '', paste(.x, collapse = ', '))) %>% 
    mutate(suppl_tbl$var_lexicon, Stratification = .) %>% 
    map_dfc(unlist)

  suppl_tbl$var_lexicon <- 
    as_mdtable(suppl_tbl$var_lexicon , 
               label = 'var_dict', 
               ref_name = 'var_dict', 
               caption = paste('Study variables. The table is available', 
                               'as a supplementary Excel file.'))
  
# Supplementary Table S2: initial pool of explanatory variables ------
  
  insert_msg('Table S2: modeling, explanatory variables')
  
  suppl_tbl$modeling_vars <- mod$variables %>% 
    exchange(dict = globals$var_lexicon) %>% 
    paste(collapse = ', ') %>% 
    tibble(`Explanatory variable` = .) %>% 
    as_mdtable(label = 'modeling_vars', 
               ref_name = 'modeling vars', 
               caption = paste('Initial set of candidate explanatory', 
                               'variables in modeling of illness perception.'))
  
# Supplementary Table S3: differences between included/excluded -----
  
  insert_msg('Table S3: Differences between included and excluded participants')
  
  ## the table presents the top differentiating factors
  
  suppl_tbl$excluded$top_factors <- inc_bias$test_results[c('baseline', 
                                                            'fup', 
                                                            'lab')] %>% 
    map(filter, p_adjusted < 0.1) %>% 
    map(~.x$variable) %>% 
    reduce(c)
  
  suppl_tbl$excluded <- list(inc_bias$desc_stats, 
                             inc_bias$test_results) %>% 
    map(~map_dfr(.x, 
                 filter, 
                 variable %in% suppl_tbl$excluded$top_factors)) %>% 
    map(select, 
        any_of(c('variable', 'included', 
                 'excluded', 'significance', 
                 'eff_size'))) %>% 
    reduce(left_join, by = 'variable')
  
  suppl_tbl$excluded <- suppl_tbl$excluded %>% 
    format_summ_tbl %>% 
    rbind(tibble(variable = 'n, participants', 
                 included = table(inc_bias$analysis_tbl$analysis_status)[1], 
                 excluded = table(inc_bias$analysis_tbl$analysis_status)[2], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'Included in the analysis', 
                'Excluded from the analysis', 
                'Significance', 
                'Effect size'))
    
  suppl_tbl$excluded <- 
    as_mdtable(suppl_tbl$excluded , 
               label = 'excluded', 
               ref_name = 'excluded',
               caption = paste('Significant and near-significant (p < 0.1)', 
                               'factors differentiating between the CovILD', 
                               'cohort participants included in the analysis', 
                               'and excluded due to variable missingness or', 
                               'lacking long-term COVID-19 sequelae.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
# Supplementary Table S4: COVID-19 follow-up, 1-year ----
  
  insert_msg('Table S4: FUP chaqracteristic of the cohort')
  
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
               caption = paste('COVID-19 symptoms, performance, fatigue,', 
                               'exertional capacity, cardiopulmonary', 
                               'abnormalities and rehabilitation status', 
                               'at the one-year follow-up.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
# Supplementary Table S5: laboratory parameters at the follow-up ------
  
  insert_msg('Table S5: biochemistry at the follow-up')
  
  suppl_tbl$biochemistry_fup <- 
    left_join(cohort$desc_stats$lab, 
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
               caption = paste('Laboratory parameters at the one-year', 
                               'follow-up. Values are presented as medians', 
                               'with interquartile ranges (IQR).'))
  
# Supplementary Table S6: gender differences ------
  
  insert_msg('Table S6: gender differences')
  
  ## top differences between the genders are listed
  
  suppl_tbl$gender$top_factors <- gender$test_results[c('baseline', 
                                                        'fup', 
                                                        'lab', 
                                                        'ipq')] %>% 
    map(filter, p_adjusted < 0.1) %>% 
    map(~.x$variable) %>% 
    reduce(c)
  
  suppl_tbl$gender <- list(gender$desc_stats, 
                           gender$test_results) %>% 
    map(~.x[c('baseline', 
              'fup', 
              'lab', 
              'ipq')]) %>% 
    map(~map_dfr(.x, 
                 filter, 
                 (variable %in% suppl_tbl$gender$top_factors | 
                    stri_detect(variable, regex = '^ipq_')))) %>% 
    map(select, 
        any_of(c('variable', 'female', 'male', 
                 'significance', 'eff_size'))) %>% 
    reduce(left_join, by = 'variable')
  
  suppl_tbl$gender <- suppl_tbl$gender %>% 
    format_summ_tbl %>% 
    rbind(tibble(variable = 'n, participants', 
                 female = table(cov_data$clear_data$sex)[1], 
                 male = table(cov_data$clear_data$sex)[2], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'Females', 
                'Males', 
                'Significance', 
                'Effect size'))
  
  suppl_tbl$gender <- 
    as_mdtable(suppl_tbl$gender, 
               label = 'gender', 
               ref_name = 'gender', 
               caption = paste('Significant and near-significant (p < 0.1)', 
                               'factors differentiating between female', 
                               'and male participants and illness', 
                               'perception in the genders.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
# Supplementary Table S7: respiratory comorbidity ---------
  
  insert_msg('Supplementary Table S7: respiratory comorbidity')
  
  ## significant and near significant differences
  
  suppl_tbl$respi$top_factors <- respi$test_results[c('baseline', 
                                                       'fup', 
                                                       'lab', 
                                                       'ipq')] %>% 
    map(filter, p_adjusted < 0.1) %>% 
    map(~.x$variable) %>% 
    reduce(c)
  
  suppl_tbl$respi <- list(respi$desc_stats, 
                           respi$test_results) %>% 
    map(~map_dfr(.x, 
                 filter, 
                 variable %in% suppl_tbl$respi$top_factors)) %>% 
    map(select, 
        any_of(c('variable', 'no', 'yes', 
                 'significance', 'eff_size'))) %>% 
    reduce(left_join, by = 'variable')
  
  suppl_tbl$respi <- suppl_tbl$respi %>% 
    format_summ_tbl %>% 
    rbind(tibble(variable = 'n, participants', 
                 no = table(cov_data$clear_data$respi_comorb)[1], 
                 yes = table(cov_data$clear_data$respi_comorb)[2], 
                 significance = NA, 
                 eff_size = NA), 
          .) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ', %', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 
                'No respiratory comorbidity', 
                'Respiratory comorbidity', 
                'Significance', 
                'Effect size'))
  
  suppl_tbl$respi <- 
    as_mdtable(suppl_tbl$respi, 
               label = 'respi', 
               ref_name = 'respi', 
               caption = paste('Significant and near-significant (p < 0.1)', 
                               'factors differentiating between participants', 
                               'with and without respiratory comorbidity.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
  
  
# Supplementary Table S8: differences between the IPQ clusters -----
  
  insert_msg('Supplementary Table S8: differences between the clusters')
  
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
               caption = paste('Significant and near-significant (p < 0.1)', 
                               'differences between the illness perception', 
                               'clusters.', 
                               'Numeric variables are presented as medians', 
                               'with interquartile ranges (IQR).',  
                               'Categorical variables are shown as percentages', 
                               'and counts within the strata.'))
  
# Saving the tables on the disc ------
  
  insert_msg('Saving the tables')
  
  paper_tbl %>% 
    set_names(paste('Table', 1:length(paper_tbl))) %>% 
    write_xlsx('./paper/tables.xlsx')
  
  suppl_tbl$cover <- tibble(Table = paste0('Table S', 1:length(suppl_tbl)), 
                            Description = map(suppl_tbl, attr, 'caption')) %>% 
    map_dfc(unlist)
  
  suppl_tbl[c('cover', names(suppl_tbl)[names(suppl_tbl) != 'cover'])] %>% 
    set_names(c('Cover', paste0('Table S', 1:(length(suppl_tbl) - 1)))) %>% 
    write_xlsx('./paper/supplementary_tables.xlsx')
  
# END ------
  
  insert_tail()