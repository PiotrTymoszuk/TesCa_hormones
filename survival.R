# Modeling of relapse-free survival:
#
# 1) Univariable Cox modeling, for normalized numeric variables, both first-
# and second-order terms are included in the models
#
# 2) Univariable KM analysis: linear predictor scores of the univariable 
# Cox models are cut in tertiles, factors are handled as they are
#
# 3) Finding of the optimal cutoffs for non-transformed numeric explanatory 
# variables with KM optimizer
#
# 4) LASSO Cox modeling for the first/second order PRL term 
# and the optimal PRL cutoff

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
  library(glmnet)
  library(caret)
  library(rms)

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
  
  insert_msg('Common survival analysis globals')
  
  surv_globals <- list()
  
  surv_globals$data <- tesca$data %>% 
    transmute(ID = ID, 
              relapse = relapse, 
              rfs_days = rfs_days, 
              E2_sqrt = sqrt(E2), 
              PRL_sqrt = sqrt(PRL), 
              FSH_sqrt = sqrt(FSH), 
              LH_sqrt = sqrt(LH), 
              T_total_sqrt = sqrt(T_total), 
              testosterone_replacement = testosterone_replacement, 
              LDH_class = LDH_class, 
              cs_lugano = car::recode(cs_lugano, 
                                      "'I' = 'I'; 
                                      'II' = 'II - III'; 
                                      'III' = 'II - III'"), 
              cs_lugano = factor(cs_lugano, c('I', 'II - III')), 
              radiotherapy = radiotherapy, 
              infiltration_rete_testis = infiltration_rete_testis, 
              seminoma_cancer = seminoma_cancer, 
              embryonal_cancer = embryonal_cancer, 
              LDH_class = LDH_class, 
              HCG_class = HCG_class, 
              AFP_class = AFP_class, 
              chemotherapy = chemotherapy, 
              teratoma_cancer = teratoma_cancer, 
              RLA = RLA, 
              max_size_cm_sqrt = sqrt(max_size_cm), 
              lymphovas_invasion = lymphovas_invasion, 
              histology = histology, 
              pt_stage = car::recode(pt_stage, 
                                     "'I' = 'I'; 
                                      'II' = 'II - III'; 
                                      'III' = 'II - III'"), 
              pt_stage = factor(pt_stage, c('I', 'II - III')), 
              age_surgery_sqrt = sqrt(age_surgery), 
              bmi_sqrt = sqrt(bmi), # convergence problems!
              body_mass_class = body_mass_class, 
              T_total_class = T_total_class, 
              E2_class = E2_class, 
              FSH_class = FSH_class, 
              LH_class = LH_class, 
              PRL_class = PRL_class)
  
  ## Z scores of the numeric variables
  
  surv_globals$data <- surv_globals$data %>%
    select(-relapse, -rfs_days) %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x) %>% 
    cbind(surv_globals$data[c('relapse', 'rfs_days')])
  
  ## variable lexicon
  
  surv_globals$lexicon <- 
    tibble(variable = names(surv_globals$data)[names(surv_globals$data) != 'ID']) %>% 
    filter(!variable %in% c('relapse', 'rfs_days'))
  
  surv_globals$lexicon <- surv_globals$lexicon %>% 
    mutate(format = surv_globals$data[surv_globals$lexicon$variable] %>% 
             map_lgl(is.numeric), 
           format = ifelse(format, 'numeric', 'factor'), 
           transformation = stri_extract(variable, regex = '(sqrt|log)$'), 
           source_variable = stri_replace(variable, 
                                          regex = '_sqrt|_log', 
                                          replacement = ''), 
           label = exchange(source_variable, 
                            dict = tesca$lexicon, 
                            value = 'label'), 
           label = ifelse(!is.na(transformation), 
                          paste(transformation, label), 
                          label))
  
  surv_globals$variables <- surv_globals$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)
  
# analysis scripts ----
  
  insert_msg('Analysis scripts')
  
  c('./survival scripts/univariate.R', 
    './survival scripts/uni_km.R', 
    './survival scripts/uni_cutoff.R', 
    './survival scripts/multi_cox.R', 
    './survival scripts/multi_cut.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END ------
  
  insert_tail()