# Launches the entire analysis pipeline

  library(soucer)
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     'histology.R', 
                     'hormones.R', 
                     'survival.R', 
                     'report.R'), 
                   crash = TRUE, message = TRUE))