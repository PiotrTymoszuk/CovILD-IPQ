# Values used in the manuscript text

  insert_head()
  
# container -----
  
  paper_val <- list()
  
# Participant n numbers ------
  
  insert_msg('N numbers of participants')
  
  paper_val$n_fup_missing <- 
    mdexpr(145 - nrow(cov_data$data_tbl), 
           ref_name = 'n_fup_missing', 
           caption = 'Participnts with the missing one-year follow-up.')
  
  paper_val$n_fup_complete <- 
    mdexpr(nrow(cov_data$data_tbl), 
           ref_name = 'n_fup_complete', 
           caption = 'Participants having complete the one-year follow-up.')
  
  paper_val$n_no_recovery <- 
    mdexpr(length(cov_data$cov_residuals), 
           ref_name = 'n_no_recovery', 
           caption = 'Participants with incomplete recovery.')
  
  paper_val$n_ipq_complete <- 
    mdexpr(length(cov_data$ipq_complete), 
           ref_name = 'n_ipq_complete', 
           caption = 'Participants with complete IPQ answers.')
  
  paper_val$n_analysis <- 
    mdexpr(nrow(cov_data$clear_data), 
           ref_name = 'n_analysis', 
           caption = 'Participants in the analysis.')
  
# Numbers of factors included in clustering analysis -------
  
  insert_msg('N numbers and names of clustering factors')
  
  paper_val$n_clust_factors <- 
    mdexpr(clustTools::nobs(ipq_clust$clust_obj)$variables, 
           ref_name = 'n_clust_factors', 
           caption = 'Number of clustering factors.')
  
  paper_val$clust_factors <- 
    mdexpr(colnames(model.frame(ipq_clust$clust_obj)) %>% 
             translate_var(out_value = 'label_long') %>% 
             stri_replace(fixed = 'Illness perception, ', 
                          replacement = '') %>% 
             paste(collapse = ', '), 
           ref_name = 'clust_factors', 
           caption = 'Names of clustering factors')
  
  paper_val$clust_factors_and <- 
    mdexpr(colnames(model.frame(ipq_clust$clust_obj)) %>% 
             translate_var(out_value = 'label_long') %>% 
             stri_replace(fixed = 'Illness perception, ', 
                          replacement = '') %>% 
             collapse_and, 
           ref_name = 'clust_factors_and', 
           caption = 'Names of clustering factors.')
  
# Numbers and names of significant factors ------
  
  insert_msg('N numbers and names of sgnificant explanatory variables')
  
  paper_val$n_indep_vars <- 
    mdexpr(length(mod$variables), 
           ref_name = 'n_indep_vars', 
           caption = 'Number of independent variables.')

  paper_val$n_factor_elnet <- 
    mdexpr(length(an$variables$ipq_total$eln_mod), 
           ref_name = 'n_factor_elnet', 
           caption = 'Number of significant factors in Elastic Net modeling.')
  
  paper_val$n_factor_lasso <- 
    mdexpr(length(an$variables$ipq_total$lasso_mod), 
           ref_name = 'n_factor_lasso', 
           caption = 'Number of significant factors in LASSO modeling.')
  
  paper_val$n_factor_blasso <- 
    mdexpr(length(an$variables$ipq_total$blass_mod), 
           ref_name = 'n_factor_lasso', 
           caption = 'Number of significant factors in Bayesian LASSO modeling.')
  
  paper_val$factor_elnet <- 
    mdexpr(an$variables$ipq_total$eln_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             paste(collapse = ', '), 
           ref_name = 'factor_elnet', 
           caption = 'Significant factors in Elastic Net modeling.')
  
  paper_val$factor_elnet_and <- 
    mdexpr(an$variables$ipq_total$eln_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             collapse_and, 
           ref_name = 'factor_elnet_and', 
           caption = 'Significant factors in Elastic Net modeling.')
  
  paper_val$factor_lasso <- 
    mdexpr(an$variables$ipq_total$lasso_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             paste(collapse = ', '), 
           ref_name = 'factor_lasso', 
           caption = 'Significant factors in LASSO modeling.')
  
  paper_val$factor_lasso_and <- 
    mdexpr(an$variables$ipq_total$lasso_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             collapse_and, 
           ref_name = 'factor_lasso_and', 
           caption = 'Factors in LASSO modeling.')
  
  paper_val$factor_blasso <- 
    mdexpr(an$variables$ipq_total$blass_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             paste(collapse = ', '), 
           ref_name = 'factor_blasso', 
           caption = 'Significant factors in Bayesian LASSO modeling.')
  
  paper_val$factor_blasso_and <- 
    mdexpr(an$variables$ipq_total$blass_mod %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             collapse_and, 
           ref_name = 'factor_blasso_and', 
           caption = 'Factors in Bayesiean LASSO modeling.')
  
# Numbers and names of the key factors for IPQ ------
  
  insert_msg('Numbers and names of the key factors for IPQ')
  
  paper_val$key_factor_number <- 
    mdexpr(length(an$cmm_variables$ipq_total), 
           ref_name = 'key_factor_number', 
           caption = 'Number of the key factors for IPQ.')
  
  paper_val$key_factors <- 
    mdexpr(an$cmm_variables$ipq_total %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             paste(collapse = ', '), 
           ref_name = 'key_factors', 
           caption = 'The key factors for IPQ.')
  
  paper_val$key_factors_and <- 
    mdexpr(an$cmm_variables$ipq_total %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             collapse_and, 
           ref_name = 'key_factors_and', 
           caption = 'The key factors for IPQ.')
  
# Numbers and names of the key factors for the emotion/concern/consequences -------
  
  insert_msg('Key factors for the emotion/concern/consequences')
  
  paper_val$key_factor_number_sub1 <-  
    mdexpr(length(an$cmm_variables$ipq_sub1), 
           ref_name = 'key_factor_number_sub1', 
           caption = paste('Number of the key factors for', 
                           'emotion/concern/consequences.'))
  
  paper_val$key_factors_sub1 <- 
    mdexpr(an$cmm_variables$ipq_sub1 %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             paste(collapse = ', '), 
           ref_name = 'key_factors_sub1', 
           caption = 'The key factors for emotion/concern/consequences.')
  
  paper_val$key_factors_and_sub1 <- 
    mdexpr(an$cmm_variables$ipq_sub1 %>% 
             translate_var(out_value = 'label_long') %>% 
             de_capitalize %>% 
             collapse_and, 
           ref_name = 'key_factors_and_sub1', 
           caption = 'The key factors for emotion/concern/consequences.')
  
# Cohort characteristic ------
  
  insert_msg('Cohort characteristic')
  
  paper_val$cohort_age <- mdexpr(get_median_iqr(cov_data$clear_data, 'age'), 
                                 ref_name = 'cohort_age', 
                                 caption = 'Age, cohort')
  
  paper_val$cohort_sex <- mdexpr(get_percent(cov_data$clear_data, 'sex'), 
                                 ref_name = 'cohort_sex',
                                 caption = 'Sex, cohort')
  
  paper_val$cohort_smoking <- 
    mdexpr(get_percent(cov_data$clear_data, 'smoking_history'), 
           ref_name = 'cohort_comorbidity', 
           caption = 'Comorbidity, cohort')
  
  paper_val$cohort_comorb <- 
    mdexpr(get_percent(cov_data$clear_data, 'comorb_present'), 
           ref_name = 'cohort_comorbidity', 
           caption = 'Comorbidity, cohort')
  
  paper_val$severity <- 
    mdexpr(get_percent(cov_data$clear_data, 'cat_WHO'), 
           ref_name = 'cov_severity', 
           caption = 'Severity, cohort')
  
  paper_val$symptoms <- 
    mdexpr(get_percent(cov_data$clear_data, 'sympt_present'), 
           ref_name = 'persistent_symptoms', 
           caption = 'Symptoms, cohort')
  
  paper_val$fatigue_biCFS <- 
    mdexpr(get_percent(cov_data$clear_data, 'Chalder_FS_bimodal'), 
           ref_name = 'cfs_signif_fatigue', 
           caption = 'Significant fatigue, cohort')
  
  paper_val$red_performance <- 
    mdexpr(get_percent(cov_data$clear_data, 'fatigue_sympt'), 
           ref_name = 'red_performance', 
           caption = 'Reduced performance, cohort')
  
  paper_val$dyspnea <- 
    mdexpr(get_percent(cov_data$clear_data, 'dyspnoe_sympt'), 
           ref_name = 'dyspnoe_sympt', 
           caption = 'Dyspnea, cohort')
  
  paper_val$sleep <- 
    mdexpr(get_percent(cov_data$clear_data, 'sleep_sympt'), 
           ref_name = 'sleep_sympt', 
           caption = 'Sleep problems, cohort')
  
  paper_val$lufo_red <- 
    mdexpr(get_percent(cov_data$clear_data, 'lufo_red'), 
           ref_name = 'lufo_red', 
           caption = 'LFT abnormality, cohort')
  
  paper_val$ct_findings <- 
    mdexpr(get_percent(cov_data$clear_data, 'ct_severity_any'), 
           ref_name = 'ct_findings', 
           caption = 'CT abnormality, cohort')
  
  paper_val$diastolic_dysf <- 
    mdexpr(get_percent(cov_data$clear_data, 'diastolic_dysf'), 
           ref_name = 'diastolic_dysf', 
           caption = 'Diastolic dysfunction, cohort')
  
# IPQ in the cohort ------
  
  insert_msg('IPQ in the cohort')
  
  paper_val$ipq_cohort <- 
    mdexpr(get_median_iqr(cov_data$clear_data, 'ipq_total'), 
           ref_name = 'ipq_cohort', 
           caption = 'IPQ, cohort')
  
  paper_val$ipq_severe <- 
    mdexpr(get_median_iqr(cov_data$clear_data %>% 
                            filter(cat_WHO == 'HS'), 
                          'ipq_total'), 
           ref_name = 'ipq_severe', 
           caption = 'IPQ, severe')
  
  paper_val$ipq_moderate <- 
    mdexpr(get_median_iqr(cov_data$clear_data %>% 
                            filter(cat_WHO == 'HM'), 
                          'ipq_total'), 
           ref_name = 'ipq_moderate', 
           caption = 'IPQ, moderate')
  
  paper_val$ipq_ambulatory <- 
    mdexpr(get_median_iqr(cov_data$clear_data %>% 
                            filter(cat_WHO == 'A'), 
                          'ipq_total'), 
           ref_name = 'ipq_ambulatory', 
           caption = 'IPQ, ambulatory')
  
  paper_val$ipq_q3_cohort <- 
    mdexpr(get_median_iqr(cov_data$clear_data, 'ipq_q3'), 
           ref_name = 'ipq_q3_cohort', 
           caption = 'IPQ Q3, cohort')
  
# Performance measures of the optimal clustering algorithm -------
  
  insert_msg('Clustering performance measures')
  
  paper_val$clust_var <- 
    mdexpr(clustTools::var(ipq_clust$clust_obj)$frac_var %>% 
             signif(2), 
           ref_name = 'clust_var', 
           caption = 'Explained clustering variance.')
  
  paper_val$clust_cv <- 
    mdexpr(1 - signif(cv(ipq_clust$clust_obj, 
                         nfolds = 10, 
                         kNN = 7, 
                         resolve_ties = TRUE)$summary$mean_error[1], 
                      2), 
           ref_name = 'cv_accuracy', 
           caption = 'CV accuracy of the clustering structure.')
  
  paper_val$clust_fun <- mdexpr(ipq_clust$clust_obj$clust_fun %>% 
                                  toupper, 
                                ref_name = 'clust_fun', 
                                caption = 'Clustering algorithm.')
  
  paper_val$clust_dist <- mdexpr(ipq_clust$clust_obj$dist_method %>% 
                                   capitalize, 
                                 ref_name = 'clust_dist', 
                                 caption = 'Clustering distance.')
  
  paper_val$clust_number <- 
    mdexpr(ipq_clust$clust_obj$clust_assignment$clust_id %>% 
             levels %>% 
             length, 
           ref_name = 'clust_number', 
           caption = 'Number of clusters.')
  
  paper_val$clust_names <- 
    mdexpr(ipq_clust$clust_obj$clust_assignment$clust_id %>% 
             levels %>% 
             paste(collapse = ', '), 
           ref_name = 'clust_names', 
           caption = 'Cluster names.')
  
  paper_val$clust_names_and <- 
    mdexpr(ipq_clust$clust_obj$clust_assignment$clust_id %>% 
             levels %>% 
             collapse_and, 
           ref_name = 'clust_names', 
           caption = 'Cluster names.')
  
  paper_val$clusters_n <-  
    mdexpr(set_names(ngroups(ipq_clust$clust_obj)$n, 
                     ngroups(ipq_clust$clust_obj)$clust_id), 
           ref_name = 'clusters_n', 
           caption = 'Cluster n numbers.')
  
  paper_val$clusters_percent <- 
    mdexpr(signif(ngroups(ipq_clust$clust_obj)$n/sum(ngroups(ipq_clust$clust_obj)$n) * 100, 2),
           ref_name = 'clusters_percent', 
           caption = 'Clusters, percents of the cohort.')
  
# Optimal lambda values, total score -----
  
  insert_msg('Optimal lambda values, total score')
  
  paper_val$elnet_lambda <- 
    mdexpr(filter(eln_mod$opt_lambda, response == 'ipq_total')$lambda %>% 
             signif(2), 
           ref_name = 'elnet_lambda', 
           caption = 'Elastic Net, optimal lambda')
  
  paper_val$lasso_lambda <- 
    mdexpr(filter(lasso_mod$opt_lambda, response == 'ipq_total')$lambda %>% 
             signif(2), 
           ref_name = 'lasso_lambda', 
           caption = 'LASSO, optimal lambda')
  
  paper_val$blasso_sparsity <- 
    mdexpr(blass_mod$models$ipq_total$bestTune[1, 'sparsity'] %>% 
             signif(2), 
           ref_name = 'blasso_sparsity', 
           caption = 'Bayesian LASSO, optimal sparsity')
  
# Optimal lambda values, emotion/concerns/consequences ------
  
  insert_msg('Optimal lambda values, emotion/concerns/consequences')
  
  paper_val$elnet_lambda_sub1 <- 
    mdexpr(filter(eln_mod$opt_lambda, response == 'ipq_sub1')$lambda %>% 
             signif(2), 
           ref_name = 'elnet_lambda_sub1', 
           caption = paste('Elastic Net, optimal lambda,', 
                           'emotion/concerns/consequences'))
  
  paper_val$lasso_lambda_sub1 <- 
    mdexpr(filter(lasso_mod$opt_lambda, response == 'ipq_sub1')$lambda %>% 
             signif(2), 
           ref_name = 'lasso_lambda_sub1', 
           caption = 'LASSO, optimal lambda, emotion/concerns/consequences')
  
  paper_val$blasso_sparsity <- 
    mdexpr(blass_mod$models$ipq_sub1$bestTune[1, 'sparsity'] %>% 
             signif(2), 
           ref_name = 'blasso_sparsity_sub1', 
           caption = paste('Bayesian LASSO, optimal sparsity,', 
                           'emotion/concerns/consequences'))
  
# Multi - parameter modeling performance measures, total score ------
  
  insert_msg('Multi-paramater model performance, total score')

  paper_val$elnet_rsq_train <- 
    mdexpr(eln_mod$cv_fit_stats$ipq_total$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'elnet_rsq_train', 
           caption = 'R-squared of the Elastic Net model.')
  
  paper_val$elnet_rsq_cv <- 
    mdexpr(eln_mod$cv_fit_stats$ipq_total$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'elnet_rsq_cv', 
           caption = 'CV R-squared of the Elastic Net model.')
  
  paper_val$lasso_rsq_train <- 
    mdexpr(lasso_mod$cv_fit_stats$ipq_total$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'lasso_rsq_train', 
           caption = 'R-squared of the LASSO model.')
  
  paper_val$lasso_rsq_cv <- 
    mdexpr(lasso_mod$cv_fit_stats$ipq_total$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'lasso_rsq_cv', 
           caption = 'CV R-squared of the LASSO model.')
  
  paper_val$blasso_rsq_train <- 
    mdexpr(blass_mod$cv_fit_stats$ipq_total$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'blasso_rsq_train', 
           caption = 'R-squared of the Bayesian LASSO model.')
  
  paper_val$blasso_rsq_cv <- 
    mdexpr(blass_mod$cv_fit_stats$ipq_total$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'blasso_rsq_cv', 
           caption = 'CV R-squared of the Bayesian LASSO model.')
  
# Multi - parameter modeling performance measures, emotion/concern/consequences ------
  
  insert_msg('Multi-paramater model performance, emotion/concern/consequences')
  
  paper_val$elnet_rsq_train_sub1 <- 
    mdexpr(eln_mod$cv_fit_stats$ipq_sub1$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'elnet_rsq_train_sub1', 
           caption = paste('R-squared of the Elastic Net model,', 
                           'emotion/concern/consequences.'))
  
  paper_val$elnet_rsq_cv_sub1 <- 
    mdexpr(eln_mod$cv_fit_stats$ipq_sub1$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'elnet_rsq_cv_sub1', 
           caption = paste('CV R-squared of the Elastic Net model,', 
                           'emotion/concern/consequences.'))
  
  paper_val$lasso_rsq_train_sub1 <- 
    mdexpr(lasso_mod$cv_fit_stats$ipq_sub1$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'lasso_rsq_train_sub1', 
           caption = paste('R-squared of the LASSO model,', 
                           'emotion/concern/consequences.'))
  
  paper_val$lasso_rsq_cv_sub1 <- 
    mdexpr(lasso_mod$cv_fit_stats$ipq_sub1$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'lasso_rsq_cv', 
           caption = paste('CV R-squared of the LASSO model,', 
                           'emotion/concern/consequences.'))
  
  paper_val$blasso_rsq_train_sub1 <- 
    mdexpr(blass_mod$cv_fit_stats$ipq_sub1$estimate[4] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'blasso_rsq_train_sub1', 
           caption = paste('R-squared of the Bayesian LASSO model,', 
                           'emotion/concern/consequences.'))
  
  paper_val$blasso_rsq_cv_sub1 <- 
    mdexpr(blass_mod$cv_fit_stats$ipq_sub1$estimate[12] %>% 
             signif(2) %>% 
             unname, 
           ref_name = 'blasso_rsq_cv_sub1', 
           caption = paste('CV R-squared of the Bayesian LASSO model,', 
                           'emotion/concern/consequences.'))
  
# Total IPQ in the clusters -----
  
  insert_msg('Total IPQ in the clusters')

  paper_val$ipq_clust1 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#1'), 
                          'ipq_total'), 
           ref_name = 'ipq_clust1', 
           caption = 'IPQ, cluster 1')
  
  paper_val$ipq_clust2 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#2'), 
                          'ipq_total'), 
           ref_name = 'ipq_clust2', 
           caption = 'IPQ, cluster 2')
  
  paper_val$ipq_clust3 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#3'), 
                          'ipq_total'), 
           ref_name = 'ipq_clust3', 
           caption = 'IPQ, cluster 3')

# Emotion/concern/consequences in the clusters -----
  
  insert_msg('Emotion/concern/cconsequences in the clusters')
  
  paper_val$sub1_clust1 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#1'), 
                          'ipq_sub1'), 
           ref_name = 'sub1_clust1', 
           caption = 'Emotion/concern/consequences, cluster 1')
  
  paper_val$sub1_clust2 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#2'), 
                          'ipq_sub1'), 
           ref_name = 'sub1_clust2', 
           caption = 'Emotion/concern/consequences, cluster 2')
  
  paper_val$sub1_clust3 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#3'), 
                          'ipq_sub1'), 
           ref_name = 'sub1_clust3', 
           caption = 'Emotion/concern/consequences, cluster 3')
  
# Control/coherence in the clusters ---------
  
  insert_msg('Control/consequences in the clusters')
  
  paper_val$sub2_clust1 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#1'), 
                          'ipq_sub2'), 
           ref_name = 'sub2_clust1', 
           caption = 'Control/coherence, cluster 1')
  
  paper_val$sub2_clust2 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#2'), 
                          'ipq_sub2'), 
           ref_name = 'sub2_clust2', 
           caption = 'Control/coherence, cluster 2')
  
  paper_val$sub2_clust3 <- 
    mdexpr(get_median_iqr(ipq_clust$analysis_tbl %>% 
                            filter(clust_id == '#3'), 
                          'ipq_sub2'), 
           ref_name = 'sub2_clust3', 
           caption = 'Control/coherence, cluster 3')
  
# Score consistency alpha ------
  
  insert_msg('Score consistency alpha')
  
  paper_val$ipq_total_alpha <- 
    mdexpr(ipq_co$alpha_stats %>% 
             filter(response == 'ipq_total') %>% 
             .$raw_alpha %>% 
             signif(2))
  
  paper_val$ipq_total_alpha_ci <- 
    mdexpr(paste(ipq_co$alpha_stats %>% 
                   filter(response == 'ipq_total') %>% 
                   .$plot_cap %>% 
                   stri_extract(regex = '\\[.*\\]$') %>% 
                   stri_replace_all(regex = '\\[|\\]', 
                                    replacement = '')))
  
# Score consistency omega --------
  
  insert_msg('Score consistency, omega')
  
  paper_val$omega_global <- 
    mdexpr(ipq_co$omega_obj$omega.group$total[1] %>% 
             signif(2))
  
  paper_val$omega_sub1 <- 
    mdexpr(ipq_co$omega_obj$omega.group$total[2] %>% 
             signif(2))
  
  paper_val$omega_sub2 <- 
    mdexpr(ipq_co$omega_obj$omega.group$total[3] %>% 
             signif(2))

# END ----
  
  insert_tail()