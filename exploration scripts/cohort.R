# Characteristic of the study cohorts: IBK and TCGA

  insert_head()
  
# container -------
  
  cohort <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variable lexicons

  cohort$lexicon$tesca <- tesca$lexicon %>% 
    filter(variable %in% ex_globals$variables)
  
  cohort$lexicon$tcga <- tcga$lexicon %>% 
    filter(!variable %in% c('ID', 'relapse', 'death', 'tumor_death', 
                            'dss_days', 'progression', 'pfs_days', 
                            'progression_factor', 'death_factor', 
                            'os_days'))
  
  cohort$lexicon <- cohort$lexicon %>% 
    map(arrange, class)

# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  cohort$desc_stats <- 
    list(data = list(tesca = tesca$data, 
                     tcga = tcga$clinical), 
         variables = map(cohort$lexicon, ~.$variable)) %>% 
    pmap(explore, 
         what = 'table', 
         pub_styled = TRUE) %>% 
    map2(., 
         map(cohort$lexicon, ~.x[c('variable', 'class')]), 
         left_join, 
         by = 'variable') %>% 
    map(relocate, class) %>% 
    map2(., cohort$lexicon, 
         ~format_tbl(.x, 
                     lexicon = .y, 
                     value = 'table_label'))

  
# END ------
  
  insert_tail()