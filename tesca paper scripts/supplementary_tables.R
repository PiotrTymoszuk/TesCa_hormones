# Tables for the main manuscript text

  insert_head()
  
# container -----
  
  tes_suptables <- list()
  
# Table S1: absolute hormone concentrations in the hormonal subsets ------
  
  insert_msg('Table S1: absolute hormone concentrations, subsets')
  
  tes_suptables$abs_hormones <- 
    left_join(class_hormo$stats %>% 
                filter(!stri_detect(variable, regex = 'class$')), 
              class_hormo$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(rm_complete = TRUE, value = 'table_label') %>% 
    full_rbind(tibble(variable = 'Participants, n', 
                      neutral = table(class_hormo$assignment$class)[1], 
                      testicle = table(class_hormo$assignment$class)[2], 
                      pituitary = table(class_hormo$assignment$class)[3]), 
               .) %>% 
    set_names(c('Variable', 'Neutral', 'Testicle', 
                'Pituitary', 'Significance', 'Effect size')) %>% 
    mdtable(label = 'table_s1_hormone_levels_subsets', 
            ref_name = 'abs_hormones',
            caption = paste('Differences in preoperative absolute', 
                            'serum concentrations of sex hormones', 
                            'between the hormonal subsets', 
                            'of the retrospective cohort.', 
                            'Concentrations are presented as medians', 
                            'with intequartile ranges (IQR) and ranges.'))
  
# Table S2: characteristic of the TCGA cohort -----
  
  insert_msg('Table S2: characteristic of the TCGA cohort')
  
  ## prognosis is not relevant for the paper
  ## there were no patients with neoadjuvant therapy

  tes_suptables$tcga <- cohort$desc_stats$tcga %>%
    filter(class != 'prognosis', 
           !stri_detect(variable, regex = '^Neo')) %>% 
    select(-class) %>% 
    set_names(c('Variable', 'Statistic')) %>% 
    mdtable(label = 'table_s2_tcga_cohort',
            ref_name = 'tcga', 
            caption = paste('Characteristic of the TCGA cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table S3: differences in mRNA expression between the histological types ------
  
  insert_msg('Table S3: TCGA, seminoma vs NSGCT')
  
  tes_suptables$tcga_histo <- tcga_exp$result_tbl %>% 
    set_names(c('Variable', 'Seminoma', 'NSGCT', 
                'Significance', 'Effect size')) %>% 
    mdtable(label = 'table_s3_tcga_histology', 
            ref_name = 'tcga_histology', 
            caption = paste('Differences in expression of the', 
                            'hormone-related genes of interest between', 
                            'seminoma and NSGCT in the TCGA cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Saving tables on the disc -------
  
  insert_msg('Saving the tables')
  
  tes_suptables$cover <- tes_suptables %>% 
    map_chr(attr, 'caption') %>% 
    tibble(Table = paste('Table', 1:length(tes_suptables)), 
           Caption = .)
  
  tes_suptables <- tes_suptables[c('cover', names(tes_suptables)[names(tes_suptables) != 'cover'])]
  
  tes_suptables %>% 
    set_names(c('Cover', 
                paste('Table', 1:(length(tes_suptables) - 1)))) %>% 
    write_xlsx('./tesca paper/supplementary tables.xlsx')
  
# END -----
  
  insert_tail()