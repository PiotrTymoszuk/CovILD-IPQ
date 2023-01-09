# IPQ score issues

  library(tidyverse)
  library(writexl)
  library(cowplot)
  library(figur)
  library(ggplotify)
  
# container ------
  
  issues <- list()

# Issue 1: data discrepancy -------

  insert_msg('Data discrepancy')

  issues$score_discrepancy <- cov_data$data %>% 
    select(starts_with('ipq_q'), 
           'ipq_sum', 
           'KW_IPQ') %>% 
    set_names(c(paste('IPQ item', 1:8), 
                'IPQ item sum (done now)', 
                'IPQ score (from the CovILD pipeline)')) 
  
  issues$score_discrepancy %>% 
    writexl::write_xlsx('./issues/IPQ_discrepancy.xlsx')
  
# Issue 2: item correlations ------
  
  insert_msg('Item correlation')
  
  issues$correlogram <- as.ggplot(~pairs.panels(ipq_co$analysis_table %>% 
                                                  set_names(paste0('Q', 1:8)), 
                                                method = 'spearman', 
                                                stars = TRUE)) %>% 
    as_figure('item_correlation', 
              ref_name = 'correlogram', 
              w = 180, 
              h = 180)
  
# Issue 3: is the inversion really correct? ------
  
  insert_msg('Inversion')
  
  issues$items_12568 <- cohort$psy_plots[c('ipq_q1', 
                                           'ipq_q2', 
                                           'ipq_q5', 
                                           'ipq_q6', 
                                           'ipq_q8')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure('items12568_severity', 
              ref_name = 'items_12568', 
              w = 180, 
              h = 210)
  
  issues$items_347 <- cohort$psy_plots[c('ipq_q3', 
                                         'ipq_q4', 
                                         'ipq_q7')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure('items347_severity', 
              ref_name = 'items_12568', 
              w = 180, 
              h = 140)
  
# saving the plots ------
  
  insert_msg('Saving the plots on the disc')
  
  issues[c('correlogram', 
           'items_12568', 
           'items_347')] %>% 
    map(pickle, 
        path = './issues', 
        format = 'pdf')
  
# END ------
  
  insert_tail()