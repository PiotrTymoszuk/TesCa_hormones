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
  
# Table 2 missingness and information content of the variables ------
  
  insert_msg('Table 2: missingness and information content of the variables')
  
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
    mdtable(label = 'table_2_variable_missingness_gini', 
            ref_name = 'miss_inf', 
            caption = paste('Percentage of missing records and Gini', 
                            'coefficients as measure of information content', 
                            'ofthe study variables.'))
  
# Table 3: Normality testing: normality testing ------
  
  insert_msg('Table 3: normality testing, best transformations')

  tables$best_transf <- distr$best_trans %>% 
    mutate(source_variable = exchange(source_variable, 
                                      dict = tesca$lexicon, 
                                      value = 'table_label')) %>% 
    select(source_variable, 
           transformation,
           w, p_value, n_complete) %>% 
    arrange(-w) %>% 
    mdtable(label = 'table_3_normality', 
            ref_name = 'best_transformation', 
            caption = paste('Normality assessment by Shapiro-Wilk', 
                            'test for the optimal normality-improving', 
                            'transformations of the numeric study variables.'))
  
# Table 4 - 5: characteristic of the study cohort --------
  
  insert_msg('Table 4 and 5: characteristic of the study cohort')
  
  tables[c('cohort_demo_cancer', 
           'cohort_hormones')] <- 
    list(c('demography', 'pathology', 'treatment'),
         c('hormones')) %>% 
    map(~filter(cohort$desc_stats, class %in% .x)) %>% 
    map(select, - class) %>% 
    map(set_names, c('Variable', 'Statistic')) %>% 
    list(x = ., 
         label = c('table_4_cohort_demography_cancer', 
                   'table_5_cohort_sex_hormones'), 
         ref_name = c('cohort_demo_cancer', 
                      'cohort_hormones'), 
         caption = c(paste('Demographic and cancer-related characteristic', 
                           'of the study cohort.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'), 
                     paste('Pre-surgery levels of sex hormones', 
                           'in the study cohort.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'))) %>% 
    pmap(mdtable)
  
# Table 6 - 7: differences between histology types ------
  
  insert_msg('Table 6 - 7: Differences between the histology types')
  
  tables[c('histology_demo_cancer', 
           'histology_hormones')] <- 
    list(c('demography', 'pathology', 'treatment'),
         c('hormones')) %>% 
    map(~filter(histology$result_tbl, class %in% .x)) %>% 
    map(select, - class) %>% 
    map(set_names, c('Variable', 'Seminoma', 'Mixed', 
                     'Significance', 'Effect size')) %>% 
    list(x = ., 
         label = c('table_4_cohort_demography_cancer', 
                   'table_5_cohort_sex_hormones'), 
         ref_name = c('cohort_demo_cancer', 
                      'cohort_hormones'), 
         caption = c(paste('Demographic and cancer-related characteristic', 
                           'of study participants with seminomas', 
                           'and mixed-type cancers.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'), 
                     paste('Pre-surgery levels of sex hormones', 
                           'in study participants with seminomas', 
                           'and mixed-type cancers.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'))) %>% 
    pmap(mdtable)
    
  
# Table 8: differences between hormone classes ------
  
  insert_msg('Table 8')
  
  tables$classes <- class_bcg$result_tbl %>% 
    select(-class) %>% 
    set_names(c('Variable', 
                'Neutral', 
                'Testicle', 
                'Pituitary', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_8_hormone_subsets', 
            ref_name = 'classes', 
            caption = paste('Demographic and clinical characteristic', 
                            'of participant subsets developed by', 
                            'latent class analysis in respect to', 
                            'pre-surgery sex hormone levels.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
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