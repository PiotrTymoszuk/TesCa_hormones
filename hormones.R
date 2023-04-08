# Characteristic of hormones, identification of hormone abnormalities, 
# characteristic and survival for patients with hormone abnormalities

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  
  library(exda)
  library(rstatix)
  library(DescTools)
  library(clustTools)
  library(MASS)
  library(poLCA)

  library(survival)
  library(survminer)
  library(coxExtensions)

  library(caret)
  library(caretExtra)
  library(party)
  
  library(ggrepel)
  
  library(soucer)
  library(furrr)
  
  insert_head()
  
  select <- dplyr::select
  explore <- exda::explore
  set_rownames <- trafo::set_rownames
  
  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# analysis globals --------
  
  insert_msg('Analysis globals')
  
  hor_globals <- list()
  
  ## variables, sex hormones and their clinical strata
  
  hor_globals$numeric_variables <- 
    c('E2', 'T_total', 'FSH', 'LH', 'PRL')
  
  hor_globals$factor_variables <- 
    paste0(hor_globals$numeric_variables, '_class')
  
  ## analysis tables
  
  hor_globals$numeric_data <- tesca$data %>% 
    select(ID, all_of(hor_globals$numeric_variables)) %>% 
    filter(complete.cases(.))
  
  hor_globals$factor_data <- tesca$data %>% 
    select(ID, all_of(hor_globals$factor_variables)) %>% 
    filter(complete.cases(.))
  
  ## latent class colors
  
  hor_globals$class_colors <- 
    c('neutral' = 'steelblue', 
      'testicle' = 'coral3', 
      'pituitary' = 'plum3')
  
# analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  c('./hormone scripts/pca.R', 
    './hormone scripts/correlation.R', 
    './hormone scripts/correspondence.R', 
    './hormone scripts/lca.R', 
    './hormone scripts/class_hormones.R', 
    './hormone scripts/background.R', 
    './hormone scripts/classifier.R', 
    './hormone scripts/survival.R', 
    './hormone scripts/marker.R ') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -------
  
  insert_tail()