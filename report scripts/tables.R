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
  
# Table 5: Differences between participants with/without relapse ------
  
  insert_msg('Table 5: study variables and relapse')
  
  tables$relapse <- ex_relapse$result_table %>% 
    mdtable(label = 'table_5_relapse_study_vars', 
            ref_name = 'relapse', 
            caption = paste('Differences in study variable distribution', 
                            'between participant with and without', 
                            'testis cancer relapse.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 5: characteristic of the LASSO models ------
  
  insert_msg('Table 5: characteristic of the LASSO models')

  tables$lasso_stats$resample_stats <- 
    list(numeric = multi_cox, 
         cutoff = multi_cut) %>% 
    map(~.x$resample_stats) %>% 
    compress(names_to = 'model_type')
  
  tables$lasso_stats$ibs <- 
    list(numeric = multi_cox, 
         cutoff = multi_cut) %>% 
    map(~.x$brier_obj) %>%
    map(select, training, test) %>% 
    map(colMeans, na.rm = TRUE) %>% 
    map(compress, names_to = 'dataset', values_to = 'ibs') %>% 
    compress(names_to = 'model_type')
  
  tables$lasso_stats <- tables$lasso_stats %>% 
    reduce(left_join, by = c('dataset', 'model_type')) %>% 
    filter(dataset %in% c('training', 'test')) %>% 
    mutate(dataset = car::recode(dataset, 
                                 "'training' = 'data'; 
                                 'test' = '10-fold cross-validation'"), 
           model_type = car::recode(model_type, 
                                    "'numeric' = 'fist-/second-term PRL'; 
                                     'cutoff' = 'PRL strata'")) %>% 
    select(model_type, dataset, c_index, R2, ibs) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    set_names(c('LASSO model type', 
                'Dataset', 
                'Concordance index', 
                'Nagelkirke R\u00B2', 
                'Integrated Brier Score')) %>% 
    mdtable(label = 'table_5_lasso_model_stats', 
            ref_name = 'lasso_stats', 
            caption = paste('Characteristic of multi-paramater LASSO', 
                            'Cox models of effects of PRL concentration', 
                            'and PRL strata on relapse-free survival.'))
  
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