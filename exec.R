# Launches the entire pipeline

  library(soucer)

  print(source_all(c('import.R', 
                     'exploration.R', 
                     'modeling.R', 
                     'analysis.R', 
                     'paper.R'), 
                   crash = FALSE, 
                   message = TRUE))