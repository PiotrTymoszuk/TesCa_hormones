# Modeling of relapse-free survival

# tools -----

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(exda)
  library(rstatix)

  library(survival)
  library(survminer)
  library(coxExtensions)
  library(kmOptimizer)

  library(soucer)
  
  insert_head()

  c('./tools/globals.R', 
    './tools/functions.R') %>%
    source_all(message = TRUE, crash = TRUE)
  
# analysis globals --------
  
  insert_msg('Common survival analysis globals')
  
# analysis scripts ----
  
  insert_msg('Analysis scripts')
  
  
  
# END ------
  
  insert_tail()