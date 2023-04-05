# Clustering of participants in respect to pre-surgery sex hormone levels

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(trafo)
  
  library(exda)
  library(rstatix)
  
  library(clustTools)

  library(soucer)
  library(furrr)

  insert_head()
  
  explore <- exda::explore
  set_rownames <- trafo::set_rownames
  
  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  clust_globals <- list()
  
  ## analysis variables and analyis table: no transformation is applied
  ## except for normalization with median-centering
  
  clust_globals$variables <- c('E2', 'T_total', 'HCG', 'FSH', 'LH', 'PRL')
  
  ## analysis table: median centered variable values
  
  clust_globals$analysis_tbl <- tesca$data %>% 
    select(ID, all_of(clust_globals$variables)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
# analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  c('./clustering scripts/development.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()