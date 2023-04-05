# Explorative data analysis:
#
# 1) Missingness
#
# 2) Characteristic of the cohort
#
# 3) Information content of the variables measured by the Gini coefficient
#
# 4) Distribution tests for normality and mean/variance equality
#
# 5) PCA and clustering tendency of the sex hormone dataset
#
# 6) Correlation analysis of the sex hormone dataset

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)

  library(exda)
  library(rstatix)
  library(DescTools)
  library(clustTools)

  library(ggrepel)

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
  
  ex_globals <- list()
  
  ## variables to be explored
  
  ex_globals$variables <- 
    names(tesca$data)[!names(tesca$data) %in% c('ID', 
                                                'birth_date', 
                                                'surgery_date', 
                                                'relapse', 
                                                'fup_date', 
                                                'relapse_date')]

# analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./exploration scripts/missingness.R', 
    './exploration scripts/cohort.R', 
    './exploration scripts/gini.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/relapse.R', 
    './exploration scripts/pca.R', 
    './exploration scripts/correlation.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -------
  
  insert_tail()