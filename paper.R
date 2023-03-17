# Paper-rendering scripts

# tools -----

  library(plyr)
  library(tidyverse)
  library(stringi)
  library(soucer)
  library(cowplot)
  library(figur)
  library(rmarkdown)
  library(knitr)
  library(bookdown)
  library(flextable)
  library(writexl)
  library(clustTools)
  library(trafo)

  insert_head()
  
  explore <- exda::explore
  train <- caret::train
  
  source_all('./tools/project_tools.R')
  
# globals -----
  
  insert_msg('Paper globals')
  
  pap <- list()
  
  ## n numbers
  
  pap$n_baseline <- cohort$analysis_tbl %>% 
    count(subset_var)
  
  pap$n_clust <- ipq_clust$clust_obj$clust_assignment %>% 
    count(clust_id)
  
  ## common scales for the estimate plots
  
  pap$est_limits <- c(-0.5, 0.5)
  
  pap$est_fills <- scale_fill_gradient2(low = 'steelblue', 
                                        high = 'firebrick', 
                                        mid = 'white', 
                                        midpoint = 0, 
                                        limits = pap$est_limits, 
                                        name = expression(beta))
  
# paper scripts -------

  insert_msg()
  
  c('./paper scripts/tables.R', 
    './paper scripts/figures.R', 
    './paper scripts/supplement.R', 
    './paper scripts/values.R', 
    './paper scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()