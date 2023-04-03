# Report tables

  insert_head()
  
# container -----
  
  tables <- list()
  
# Table 1: study variables -------
  
  insert_msg('Table 1: study variables')
  
  tables$study_vars <- tesca$lexicon %>% 
    select(variable, description, label, levels, unit) %>% 
    set_names(c('Variable name in R', 'Description', 'Label', 
                'Categories', 'Unit')) %>% 
    mdtable(label = 'table_1_study_varables', 
            ref_name = 'study_vars', 
            caption = 'Study variables.')
  
# Table 2: characteristic of the study cohort --------
  
  insert_msg('Table 2: characteristic of the study cohort')
  
  tables$cohort <- cohort$desc_stats %>%
    set_names(c('Variable subset', 
                'Variable', 
                'Statistic')) %>% 
    mdtable(label = 'table_2_cohort_characteristic', 
            ref_name = 'cohort', 
            caption = paste('Characteristic of the study cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 3: missingness and information content of the variables ------
  
  insert_msg('Table 3: missingness and information content of the variables')
  
  tables$miss_inf <- 
    left_join(missing$variable$stats, 
              gini$stats, 
              by = 'variable') %>% 
    mutate(variable = exchange(variable, 
                               dict = tesca$lexicon, 
                               value = 'table_label')) %>% 
    select(variable, n_complete, n_missing, perc_missing, gini) %>% 
    set_names(c('Variable', 'Complete observations', 
                'Missing observations', 'Percentage missing observations', 
                'Gini index')) %>% 
    mdtable(label = 'table_3_variable_missingness_gini', 
            ref_name = 'miss_inf', 
            caption = paste('Percentage of missing records and Gini', 
                            'coefficients as measure of information content', 
                            'ofthe study variables.'))
  
# Table 4: Normality testing: normality testing ------
  
  insert_msg('Table 4: normality testing, best transformations')

  tables$best_transf <- distr$best_trans %>% 
    mutate(source_variable = exchange(source_variable, 
                                      dict = tesca$lexicon, 
                                      value = 'table_label')) %>% 
    select(source_variable, 
           transformation,
           w, p_value, n_complete) %>% 
    arrange(-w) %>% 
    mdtable(label = 'table_4_normality', 
            ref_name = 'best_transformation', 
            caption = paste('Normality assessment by Shapiro-Wilk', 
                            'test for the optimal normality-improving', 
                            'transformations of the numeric study variables.'))
  
# Saving tables on the disc -------
  
  insert_msg('Saving the tables')
  
  tables$cover <- tables %>% 
    map_chr(attr, 'caption') %>% 
    tibble(Table = paste('Table', 1:length(tables)), 
           Caption = .)
  
  tables <- tables[c('cover', names(tables)[names(tables) != 'cover'])]
  
  tables %>% 
    set_names(c('Cover', 
                paste('Table', 1:(length(tables) - 1)))) %>% 
    write_xlsx('./report/tables.xlsx')
  
# END ------
  
  insert_tail()