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
                               value = 'table_label'), 
           perc_missing = signif(perc_missing, 2), 
           gini = signif(gini, 2)) %>% 
    select(variable, n_complete, n_missing, perc_missing, gini) %>% 
    set_names(c('Variable', 'Complete observations', 
                'Missing observations', 'Percentage missing observations', 
                'Gini index')) %>% 
    mdtable(label = 'table_2_variable_missingness_gini', 
            ref_name = 'miss_inf', 
            caption = paste('Percentage of missing records and Gini', 
                            'coefficients as measure of information content', 
                            'of the study variables.'))
  
# Table 3: Normality testing: normality testing ------
  
  insert_msg('Table 3: normality testing, best transformations')

  tables$best_transf <- distr$best_trans %>% 
    filter(source_variable != 'rfs_days') %>% 
    adjust_fdr('p_value', 'none') %>% 
    mutate(source_variable = exchange(source_variable, 
                                      dict = tesca$lexicon, 
                                      value = 'table_label'), 
           w = signif(w, 2)) %>% 
    select(source_variable, 
           transformation,
           w, 
           significance, 
           n_complete) %>% 
    arrange(-w) %>% 
    set_names('Variable', 
              'Optimal transformation', 
              'Shapiro-Wilk W', 
              'Significance', 
              'Complete observations') %>% 
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
    map(set_names, c('Variable', 'Seminoma', 'NSGCT', 
                     'Significance', 'Effect size')) %>% 
    list(x = ., 
         label = c('table_4_cohort_demography_cancer', 
                   'table_5_cohort_sex_hormones'), 
         ref_name = c('histology_demo_cancer', 
                      'histology_hormones'), 
         caption = c(paste('Demographic and cancer-related characteristic', 
                           'of study participants with seminomas', 
                           'and non-seminomatous germ cell tumors.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'), 
                     paste('Pre-surgery levels of sex hormones', 
                           'in study participants with seminomas', 
                           'and non-seminomatous germ cell tumors.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'))) %>% 
    pmap(mdtable)
    
  
# Table 8: posterior probabilities and class assignment ------
  
  insert_msg('Table 8: posterior p and class assigment')
  
  tables$lca_posterior <- 
    left_join(lca$posterior %>% 
                as.data.frame %>% 
                rownames_to_column('ID'), 
              lca$assingment, 
              by = 'ID') %>% 
    set_names(c('Participant ID', 
                levels(lca$assingment$class), 
                'Hormonal subset')) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    mdtable(label = 'table_8_hormonal_subsets', 
            ref_name = 'lca-posterior', 
            caption = paste('Posterior probabilities of the hormonal subset', 
                            'assignment obtained by latent class analysis.', 
                            'The table is available in a', 
                            'supplementary Excel file.'))
  
# Table 9: differences between hormone classes ------
  
  insert_msg('Table 9: hormone subsets')
  
  tables$classes <- class_bcg$result_tbl %>% 
    select(-class) %>% 
    set_names(c('Variable', 
                'Neutral', 
                'Testicle', 
                'Pituitary', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_9_hormone_subsets', 
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

# Table 10: hormone classes and marker status -------
  
  insert_msg('Table 10: hormone subsets and marker status')
  
  tables$classes_markers <- class_mark$result_table %>% 
    compress(names_to = 'subset') %>%
    relocate(subset) %>% 
    set_names(c('Hormonal subset', 
                'Variable', 
                levels(class_mark$analysis_tbl[[1]]$marker_status), 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_10_differences_hormon_subsets_marker', 
            ref_name = 'classes_markers', 
            caption = paste('Significant differences in neutral and pituitary', 
                            'hormonal subset participants split by', 
                            'cancer marker positivity.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
  
# Table 11: Elastic Net Cox regression -----
  
  insert_msg('Table 11: Elastic Net Cox regression')
  
  tables$elastic_net <- multi_cox$analysis_tbl %>% 
    names
  
  tables$elastic_net <- 
    tables$elastic_net[!tables$elastic_net %in% c('relapse', 'rfs_days')] %>% 
    tibble(variable = .) %>% 
    mutate(order = ifelse(stri_detect(variable, regex = '_sec$'), 
                          'second', 'first'), 
           variable = stri_replace(variable, 
                                   regex = '_sec$', 
                                   replacement = ''), 
           label = exchange(variable, 
                            dict = surv_globals$lexicon), 
           label = ifelse(order == 'second',
                          paste0('(', label, ')\u00B2'), 
                          label))
  
  tables$elastic_net <- 
    tibble(`Explanatory variable` = paste(tables$elastic_net$label, 
                                          collapse = ', ')) %>% 
    mdtable(label = 'table_11_elastic_net_variables', 
            ref_name = 'elastic_net', 
            caption = paste('Explanatory variables in multi-parameter', 
                            'Elastic Cox modeling of relapse-free survival.'))
  
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