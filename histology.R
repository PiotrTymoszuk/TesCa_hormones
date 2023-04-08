# Differences in clinical parameters and hormones between tumors of seminoma 
# and NSGCT histology

# tools --------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)
  library(rlang)
  
  library(exda)
  library(rstatix)
  library(caret)
  library(caretExtra)
  library(party)
  library(nVennR)

  library(survival)
  library(survminer)

  library(ggrepel)
  
  library(soucer)
  library(furrr)
  library(doParallel)
  
  insert_head()
  
  select <- dplyr::select
  explore <- exda::explore
  set_rownames <- trafo::set_rownames
  
  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  histo_globals <- list()
  
  ## variables to be explored
  ## variables relatng to histology are removed
  
  histo_globals$variables <- 
    names(tesca$data)[!names(tesca$data) %in% c('ID', 
                                                'birth_date', 
                                                'surgery_date', 
                                                'relapse', 
                                                'fup_date', 
                                                'relapse_date', 
                                                'histology', 
                                                'relapse', 
                                                'rfs_days')]
  
  histo_globals$variables <- 
    histo_globals$variables[!stri_detect(histo_globals$variables, 
                                         regex = '_(cancer|percent)$')]
  
# analysis scripts --------
  
  insert_msg('Analysis scripts')
  
  c('./histology scripts/histology.R', 
    './histology scripts/survival.R', 
    './histology scripts/classifier.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()