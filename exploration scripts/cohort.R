# Characteristic of the study cohort

  insert_head()
  
# container -------
  
  cohort <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## classification of the variables
  
  cohort$variables <- ex_globals$variables
  
  cohort$var_class <- tesca$lexicon %>% 
    filter(variable %in% cohort$variables) %>% 
    select(variable, class) %>% 
    arrange(class)
  
  cohort$variables <- cohort$var_class$variable
  
# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  cohort$desc_stats <- tesca$data %>% 
    explore(variables = cohort$var_class$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    left_join(cohort$var_class, by = 'variable') %>% 
    relocate(class) %>% 
    format_tbl(lexicon = tesca$lexicon, 
               value = 'table_label') %>% 
    mutate(class = factor(class, levels(cohort$var_class$class))) %>% 
    arrange(class)
  
# END ------
  
  insert_tail()