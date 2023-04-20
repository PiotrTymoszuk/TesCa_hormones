# Import of the testis cancer study data provided by the Urology Team

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  library(readxl)
  library(soucer)
  library(lubridate)
  
  insert_head()
  
  c('./tools/globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# data containers -------

  tesca <- list()
  
# reading the raw study data ------
  
  insert_msg('Reading the raw data set')
  
  tesca$raw <- read_xlsx('./data/IBK/HoTuHo_final_06042023.xlsx')
  
# Wrangling ------
  
  insert_msg('Wrangling')
  
  tesca$cleared <- tesca$raw %>% 
    transmute(ID = paste0('P', 1:nrow(tesca$raw)), 
              birth_date = Geburtsdatum, 
              ## there's an obvious error in surgery date for some patients
              ## the year '1905' could not be true. In such cases, the surgery 
              ## date is computed based on the (approximate) age at surgery 
              ## provided by the study team
              surgery_date = as.character(`OP Datum`),
              surgery_date = ifelse(!stri_detect(surgery_date, regex = '^1905'), 
                                    surgery_date, 
                                    paste(as.numeric(stri_extract(as.character(Geburtsdatum), 
                                                                  regex = '^\\d{4}')) + 
                                            as.numeric(Alter), 
                                          stri_extract(as.character(Geburtsdatum), 
                                                       regex = '\\d{2}-\\d{2}$'), 
                                          sep = '-')),   
              surgery_date = as.Date(surgery_date), 
              age_surgery = difftime(surgery_date, birth_date),
              age_surgery = time_length(age_surgery, unit = 'years'), 
              surgery_type = car::recode(`OP (1=Sx, 2=Enukleation)`, 
                                         "1 = 'resection'; '2' = 'enucleation'"), 
              surgery_type = factor(surgery_type, c('resection', 'enucleation')), 
              bmi = BMI, 
              body_mass_class = cut(BMI, 
                                    c(-Inf, 25, 30, Inf), 
                                    c('normal', 'overweight', 'obesity')), 
              ## the stage is reduced to the major stage (i.e. 1a -> 1)
              pt_stage = stri_extract(pT, regex = '\\d{1}'), 
              pt_stage = as.roman(as.numeric(pt_stage)), 
              pt_stage = factor(pt_stage, c('I', 'II', 'III', 'IV')), 
              residual_tumor = car::recode(`R_Status (R0=0, R1=1)`, 
                                           "0 = 'R0'; 1 = 'R1'"), 
              residual_tumor = factor(residual_tumor, c('R0', 'R1')), 
              max_size_cm = as.numeric(`TU Größe max (cm)`), 
              infiltration_rete_testis = car::recode(`Inf. Rete testis (0=nein, 1=ja)`, 
                                                     "'0' = 'no'; '1' = 'yes'"), 
              infiltration_rete_testis = factor(infiltration_rete_testis,
                                                c('no', 'yes')), 
              lymphovas_invasion = car::recode(`LVI (0=nein, 1=ja)`, 
                                               "'0' = 'no'; '1' = 'yes'"), 
              lymphovas_invasion = factor(lymphovas_invasion, c('no', 'yes')), 
              ## the Lugano classification is reduced to the major stage
              cs_lugano = stri_extract(`klinisches Stadium Lugano (1=1, 2=2, 3=3)`, 
                                       regex = '\\d{1}'), 
              cs_lugano = as.roman(cs_lugano), 
              cs_lugano = factor(cs_lugano, c('I', 'II', 'III', 'IV')), 
              IGCCCG_risk_group = car::recode(`IGCCCG Risikogroup (good=1, interm=2, poor=3)`,
                                              "'1' = 'good'; '2' = 'intermediate'; '3' = 'poor'"), 
              IGCCCG_risk_group = factor(IGCCCG_risk_group, 
                                         c('good', 'intermediate', 'poor')), 
              ## histology
              histology = car::recode(`Reines Seminom (0=nein, 1=ja)`, 
                                          "'0' = 'NSGCT'; '1' = 'seminoma'"), 
              histology = factor(histology, c('seminoma', 'NSGCT')), 
              teratoma_percent = as.numeric(stri_extract(`Teratom (%)`, regex = '\\d+')), 
              embryonal_percent = as.numeric(stri_extract(`embryonales CA (%)`, regex = '\\d+')), 
              chorion_ca_percent = as.numeric(stri_extract(`ChorionCA (%)`, regex = '\\d+')), 
              yolk_sac_ca_percent = as.numeric(stri_extract(`DottersackTU (%)`, regex = '\\d+')), 
              seminoma_percent = as.numeric(stri_extract(`Seminom (%)`, regex = '\\d+')), 
              teratoma_percent = ifelse(histology == 'seminoma', 
                                        0, teratoma_percent), 
              embryonal_percent = ifelse(histology == 'seminoma', 
                                         0, embryonal_percent), 
              chorion_ca_percent = ifelse(histology == 'seminoma', 
                                          0, chorion_ca_percent), 
              yolk_sac_ca_percent = ifelse(histology == 'seminoma', 
                                            0, yolk_sac_ca_percent), 
              seminoma_percent = ifelse(histology == 'seminoma', 
                                        100, seminoma_percent), 
              ## dummy variable for predominant cancer histologies:
              ## defined as >= 75% histology
              teratoma_cancer = cut(teratoma_percent, 
                                     c(-Inf, 75, Inf), 
                                     c('no', 'yes'), 
                                     right = FALSE), 
              embryonal_cancer = cut(embryonal_percent, 
                                     c(-Inf, 75, Inf), 
                                     c('no', 'yes'), 
                                     right = FALSE), 
              chorion_cancer = cut(chorion_ca_percent, 
                                   c(-Inf, 75, Inf), 
                                   c('no', 'yes'), 
                                   right = FALSE),
              yolk_sac_cancer = cut(yolk_sac_ca_percent, 
                                    c(-Inf, 75, Inf), 
                                    c('no', 'yes'), 
                                    right = FALSE), 
              seminoma_cancer = cut(seminoma_percent, 
                                    c(-Inf, 75, Inf), 
                                    c('no', 'yes'), 
                                    right = FALSE), 
              ## sex hormones prior to surgery
              LH = ifelse(stri_detect(`LH (mU/ml)`, fixed = '<'), 
                          0, as.numeric(`LH (mU/ml)`)), 
              FSH = ifelse(stri_detect(`FSH (mU/ml)`, fixed = '<'), 
                           0, as.numeric(`FSH (mU/ml)`)), 
              PRL = `Prolaktin (uU/mL) RICHTIG!`, 
              T_total = stri_replace(`Testosteron (gesamt) - ng/ml`, 
                                     fixed = '>', replacement = ''), 
              T_total = as.numeric(T_total), 
              T_free = stri_replace(`Testosteron (frei)`, 
                                    fixed = '>',
                                    replacement = ''),
              T_free = as.numeric(T_free), 
              SHBG = as.numeric(SHBG), 
              HCG = stri_replace(`HCG (U/I)`, 
                                 fixed = '<', 
                                 replacement = ''), 
              HCG = stri_replace(HCG, fixed = ',', replacement = '.'), 
              HCG = as.numeric(HCG), 
              AFP = stri_replace(`AFP (µ/l)`, 
                                 fixed = '<', replacement = ''), 
              AFP = stri_replace(AFP, fixed = ',', replacement = '.'), 
              AFP = as.numeric(AFP), 
              LDH = as.numeric(`LDH (U/I)`), 
              chemotherapy = car::recode(`Chemo (0=nein, 1=ja)`,
                                         "0 = 'no'; 1 = 'yes'"), 
              chemotherapy = factor(chemotherapy, c('no', 'yes')), 
              radiotherapy = car::recode(`RT (0=nein, 1=ja)`, 
                                         "'0' = 'no'; '1' = 'yes'"), 
              radiotherapy = factor(radiotherapy, c('no', 'yes')),
              RLA = car::recode(`RLA (0=nein, 1=ja)`, 
                                "0 = 'no'; 1 = 'yes'"), 
              RLA = factor(RLA, c('no', 'yes')), 
              ## relapse: if the date is provided by no index
              ## there's no relapse, as discussed with the customer
              ## if the patient is followed up by another center
              ## ant there's no FUP date, the last freeze of the registry 
              ## on 2023-03-22 is assumed as the FUP date
              fup_date = `letzte Follow-up Visite(Datum)`,  
              fup_date = ifelse(is.na(fup_date) & 
                                  !is.na(tesca$raw$`wenn ja, Datum`) &
                                  stri_detect(tesca$raw$`wenn ja, Datum`, 
                                              fixed = 'Kontrolle'), 
                                as.Date('2023-03-22'), 
                                as.Date(fup_date)), 
              fup_date = as.Date(fup_date, origin = '1970-01-01'), 
              rfs_days = difftime(fup_date, surgery_date), 
              rfs_days = time_length(rfs_days, unit = 'days'), 
              relapse = ifelse(is.na(`Rezidiv (0=nein, 1=ja)`) & !is.na(fup_date), 
                               0, as.numeric(`Rezidiv (0=nein, 1=ja)`)),
              relapse_factor = cut(relapse, c(-Inf, 0, Inf), c('no', 'yes')), 
              ## relapse date, correcting the `Datum Rezidiv` entry '2014+2015'
              ## assumed 2014-01-01
              relapse_date = ifelse(stri_detect(`Datum Rezidiv`, fixed = '+'), 
                                    as.numeric(time_length(difftime(as.Date('2014-01-01'), 
                                                         as.Date('1899-12-30')), 
                                                unit = 'days')), 
                                    as.numeric(`Datum Rezidiv`)),
              relapse_date = as.Date(relapse_date, 
                                     origin = "1899-12-30"), 
              relapse = ifelse(is.na(relapse) & !is.na(relapse_date), 
                               1, relapse), 
              ## relapse free survival in days: if the relapse date is earlier 
              ## than the last follow-up, its used to calculate the RFS
              rfs_days = ifelse(is.na(fup_date), 
                                ifelse(!is.na(relapse_date), 
                                       time_length(difftime(relapse_date, surgery_date), 
                                                   unit = 'days'), 
                                       NA), 
                                ifelse(!is.na(relapse_date) & relapse_date < fup_date, 
                                       time_length(difftime(relapse_date, surgery_date), 
                                                   unit = 'days'), 
                                       rfs_days)), 
              ## T replacement: if date provided it's treated as yes
              testosterone_replacement = ifelse(stri_detect(`Testosteronersatz (0=nein,1=ja)`, 
                                                            fixed = '.'), 
                                                1, `Testosteronersatz (0=nein,1=ja)`), 
              testosterone_replacement = car::recode(testosterone_replacement, 
                                                     "'0' = 'no'; '1' = 'yes'"), 
              testosterone_replacement = factor(testosterone_replacement, 
                                                c('no', 'yes')), 
              E2 = stri_replace(`Östradiol (pg/ml)`, 
                                fixed = '<', 
                                replacement = ''), 
              E2 = stri_replace(E2, fixed = ',', replacement = '.'), 
              E2 = as.numeric(E2))
  
# cutoffs for LDH, HCG and AFP, SHBG and sex hormones -------
  
  insert_msg('Cutoffs for LDH, HCG and AFP and sex hormones')
  
  ## and definition of marker-negative cancers (AFP- HCG-)
  ## and marker-positive cancers (AFP+ or HCG+)
  
  tesca$cleared <- tesca$cleared %>% 
    mutate(LDH_class = cut(LDH, 
                           c(-Inf, 250, Inf), 
                           c('0 - 250 U/L', '> 250 U/L')),
           AFP_class = cut(AFP, 
                           c(-Inf, 7, Inf), 
                           c('0 - 7 ng/mL', '> 7 ng/mL')), 
           HCG_class = cut(HCG, 
                           c(-Inf, 2, Inf), 
                           c('0 - 2 IU/L', '> 2 IU/L')), 
           T_total_class = cut(T_total, 
                               c(-Inf, 3.5, 9, Inf), 
                               c('0 - 3.5 ng/mL', 
                                 '3.5 - 9 ng/mL', 
                                 '> 9 ng/mL')), 
           E2_class = cut(E2, 
                          c(-Inf, 20, 55, Inf), 
                          c('0 - 20 pg/mL', 
                            '20 - 55 pg/mL', 
                            '> 55 pg/mL')), 
           FSH_class = cut(FSH, 
                           c(-Inf, 1, 10, Inf), 
                           c('0 - 1 mU/mL', 
                             '1 - 10 mU/mL', 
                             '> 10 mU/mL')), 
           LH_class = cut(LH, 
                          c(-Inf, 1.7, 8.6, Inf), 
                          c('0 - 1.7 mU/mL', 
                            '1.7 - 8.6 mU/mL', 
                            '> 8.6 mU/mL')), 
           PRL_class = cut(PRL, 
                           c(-Inf, 480, Inf), 
                           c('0 - 480 µU/mL', 
                             '> 480 µU/mL')), 
           marker_status = ifelse(is.na(AFP_class) | is.na(HCG_class), 
                                  NA, 
                                  ifelse(AFP_class == '0 - 7 ng/mL' & HCG_class == '0 - 2 IU/L', 
                                         'AFP/HCG-', 'AFP/HCG+')), 
           marker_status = factor(marker_status, 
                                  c('AFP/HCG-', 'AFP/HCG+')))
  
# analysis data set: patients with < 50% missing data included --------
  
  insert_msg('Analysis dataset: patients with < 50% missing data')
  
  tesca$missing <- tesca$cleared %>%
    select(- ID) %>% 
    map_dfc(is.na) %>% 
    as.matrix %>% 
    set_rownames(tesca$cleared$ID)
  
  tesca$missing <- tesca$missing %>% 
    rowSums %>% 
    compress(names_to = 'ID', 
             values_to = 'n_missing') %>% 
    mutate(perc_missing = n_missing/ncol(tesca$cleared) * 100)
  
  tesca$included_records <- tesca$missing %>% 
    filter(perc_missing <= 50) %>% 
    .$ID

  tesca$data <- tesca$cleared %>% 
    filter(ID %in% tesca$included_records)
  
  tesca$missing <- NULL
  tesca$included_records <- NULL
  
  tesca <- compact(tesca)

# variable lexicon, variable levels and level n numbers -------
  
  insert_msg('Variable lexicon')
  
  tesca$lexicon <- read_xlsx('./data/IBK/variable_lexicon.xlsx') %>% 
    mutate(axis_label = ifelse(!is.na(unit), 
                               paste(label, unit, sep = ', '), 
                               label), 
           table_label = ifelse(!is.na(unit), 
                                paste(label_long, unit, sep = ', '), 
                                label_long), 
           class = factor(class, 
                          c('index', 'demography', 
                            'pathology', 'hormones', 
                            'treatment', 'prognosis')))
  
  ## categories for the factor variables
  
  tesca$levels <- tesca$data %>% 
    map(function(x) if(is.factor(x)) levels(x) else NA) %>% 
    map_chr(paste, collapse = ', ') %>% 
    compress(names_to = 'variable', 
             values_to = 'levels') %>% 
    mutate(levels = ifelse(levels == 'NA', 
                           NA, levels))
  
  tesca$lexicon <- left_join(tesca$lexicon, 
                             tesca$levels, 
                             by = 'variable')

# END ------
  
  insert_tail()