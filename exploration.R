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
# 5) Differences between tumor histologies (seminoma vs mixed)

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)

  library(exda)
  library(rstatix)
  library(DescTools)

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
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ex_globals <- list()
  
  ## variables to be explored
  ## chorion cancer: only one category provided
  
  ex_globals$variables <- 
    names(tesca$data)[!names(tesca$data) %in% c('ID', 
                                                'birth_date', 
                                                'surgery_date', 
                                                'relapse', 
                                                'fup_date', 
                                                'relapse_date', 
                                                'chorion_cancer')]

# analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./exploration scripts/missingness.R', 
    './exploration scripts/gini.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/cohort.R', 
    './exploration scripts/relapse.R', 
    './exploration scripts/excluded.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -------
  
  insert_tail()