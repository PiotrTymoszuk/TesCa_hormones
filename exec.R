# Launches the entire analysis pipeline

  library(soucer)
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     'survival.R', 
                     'clustering.R', 
                     'report.R'), 
                   crash = TRUE, message = TRUE))