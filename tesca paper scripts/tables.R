# Tables for the main manuscript text

  insert_head()
  
# container -----
  
  tes_tables <- list()
  
# Table 1: characteristic of the study cohort -------
  
  insert_msg('Table 1: characteristic of the retrospective cohort')
  
  tes_tables$cohort <- cohort$desc_stats$tesca %>% 
    filter(class %in% c('demography', 'pathology', 'treatment'), 
           !stri_detect(variable, fixed = '>='),
           !stri_detect(variable, fixed = '%'), 
           !stri_detect(variable, fixed = 'Type of surgery')) %>% 
    select(-class) %>% 
    set_names(c('Variable', 'Statistic')) %>% 
    mdtable(label = 'table_1_retrospective_cohort', 
            ref_name = 'cohort', 
            caption = paste('Demographic and cancer-related characteristic', 
                            'of the retrospective study cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interquartile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 2: seminoma vs NSGCT -------
  
  insert_msg('Table 2: seminoma vs NSGCT')
  
  tes_tables$histology <- histology$result_tbl %>% 
    filter(!stri_detect(significance, regex = '^ns'), 
           !stri_detect(variable, fixed = 'Free testosterone'), 
           !stri_detect(variable, fixed = 'Type of surgery')) %>% 
    select(-class) %>% 
    set_names(c('Variable', 'Seminoma', 'NSGCT', 
                'Significance', 'Effect size')) %>% 
    mdtable(label = 'table_2_seminoma_nsgct', 
            ref_name = 'histology', 
            caption = paste('Demographic, cancer-related and endocrine', 
                            'parameters differing significantly between',
                            'seminoma and non-seminomatous germ cell tumors', 
                            '(NSGCT)', 
                            'Numeric variables are presented as', 
                            'medians with interquartile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 3: sex hormones in the retrospective cohort ------
  
  insert_msg('Table 3: sex hormones in the retrospective cohort')
  
  tes_tables$hormones <- cohort$desc_stats$tesca %>% 
    filter(class %in% c('hormones'), 
           !stri_detect(variable, fixed = 'SHBG'), 
           !stri_detect(variable, fixed = 'Free testosterone')) %>% 
    select(-class) %>% 
    set_names(c('Variable', 'Statistic')) %>% 
    mdtable(label = 'table_3_sex_hormones', 
            ref_name = 'hormones', 
            caption = paste('Preoperative serum concentrations of sex hormones',
                            'in the retrospective study cohort', 
                            'Numeric variables are presented as', 
                            'medians with interquartile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 4: significant differences between the hormonal subsets -----
  
  insert_msg('Table 4: differences between the hormonal classes')
  
  tes_tables$subsets <- class_bcg$result_tbl %>% 
    select(-class) %>% 
    filter(!stri_detect(significance, regex = '^ns'), 
           !stri_detect(variable, fixed = 'SHBG'), 
           !stri_detect(variable, fixed = 'Free testosterone'), 
           !stri_detect(variable, regex = '%$'), 
           !stri_detect(variable, fixed = '>=')) %>% 
    set_names(c('Variable', 
                'Neutral', 
                'Testicle', 
                'Pituitary', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_4_hormonal_subsets', 
            ref_name = 'subsets', 
            caption = paste('Significant differences in demographic,', 
                            'clinical and pathologic parameters', 
                            'between the hormonal subsets of', 
                            'the retrospective cohort.',
                            'Numeric variables are presented as', 
                            'medians with interquartile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Saving tables on the disc -------
  
  insert_msg('Saving the tables')
  
  tes_tables$cover <- tes_tables %>% 
    map_chr(attr, 'caption') %>% 
    tibble(Table = paste('Table', 1:length(tes_tables)), 
           Caption = .)
  
  tes_tables <- tes_tables[c('cover', names(tes_tables)[names(tes_tables) != 'cover'])]
  
  tes_tables %>% 
    set_names(c('Cover', 
                paste('Table', 1:(length(tes_tables) - 1)))) %>% 
    write_xlsx('./tesca paper/tables.xlsx')
  
# END -----
  
  insert_tail()