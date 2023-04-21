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
  
# Table 2: differences between the analyzed and excluded patients -------
  
  insert_msg('Table 2: differences between included and excluded patients')

  tables$excluded <- excl$result_tbl %>% 
    set_names(c('Variable', 
                'Included', 
                'Excluded', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_2_included_excluded', 
            ref_name = 'excluded', 
            caption = paste('Significant differences between testic cancer', 
                            'patients included in the analysis and patients', 
                            'excluded from the analysis due to > 50%', 
                            'data missingness.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
    
# Table 3 missingness and information content of the variables ------
  
  insert_msg('Table 3: missingness and information content of the variables')
  
  tables$miss_inf <- 
    left_join(missing$variable$stats, 
              gini$stats$tesca, 
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
    mdtable(label = 'table_3_variable_missingness_gini', 
            ref_name = 'miss_inf', 
            caption = paste('Percentage of missing records and Gini', 
                            'coefficients as measure of information content', 
                            'of the study variables.'))
  
# Table 4: Normality testing: normality testing ------
  
  insert_msg('Table 4: normality testing, best transformations')

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
    mdtable(label = 'table_4_normality', 
            ref_name = 'best_transformation', 
            caption = paste('Normality assessment by Shapiro-Wilk', 
                            'test for the optimal normality-improving', 
                            'transformations of the numeric study variables.'))
  
# Table 5 - 6: characteristic of the study cohort --------
  
  insert_msg('Table 5 and 6: characteristic of the study cohort')
  
  tables[c('cohort_demo_cancer', 
           'cohort_hormones')] <- 
    list(c('demography', 'pathology', 'treatment'),
         c('hormones')) %>% 
    map(~filter(cohort$desc_stats$tesca, class %in% .x)) %>% 
    map(select, - class) %>% 
    map(set_names, c('Variable', 'Statistic')) %>% 
    list(x = ., 
         label = c('table_5_cohort_demography_cancer', 
                   'table_6_cohort_sex_hormones'), 
         ref_name = c('cohort_demo_cancer', 
                      'cohort_hormones'), 
         caption = c(paste('Demographic and cancer-related characteristic', 
                           'of the Innsbruck study cohort.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'), 
                     paste('Pre-surgery levels of sex hormones', 
                           'in the Innsbruck study cohort.', 
                           'Numeric variables are presented as', 
                           'medians with interqurtile ranges (IQR)', 
                           'and ranges.', 
                           'Categorical variables are presented', 
                           'as percentages and counts within the', 
                           'complete observation set.'))) %>% 
    pmap(mdtable)
  
# Table 7 - 8: differences between histology types ------
  
  insert_msg('Table 7 - 8: Differences between the histology types')
  
  tables[c('histology_demo_cancer', 
           'histology_hormones')] <- 
    list(c('demography', 'pathology', 'treatment'),
         c('hormones')) %>% 
    map(~filter(histology$result_tbl, class %in% .x)) %>% 
    map(select, - class) %>% 
    map(set_names, c('Variable', 'Seminoma', 'NSGCT', 
                     'Significance', 'Effect size')) %>% 
    list(x = ., 
         label = c('table_7_cohort_demography_cancer', 
                   'table_8_cohort_sex_hormones'), 
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
  
# Table 9: posterior probabilities and class assignment ------
  
  insert_msg('Table 9: posterior p and class assigment')
  
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
    mdtable(label = 'table_9_hormonal_subsets', 
            ref_name = 'lca-posterior', 
            caption = paste('Posterior probabilities of the hormonal subset', 
                            'assignment obtained by latent class analysis.', 
                            'The table is available in a', 
                            'supplementary Excel file.'))
  
# Table 10: differences between hormone classes ------
  
  insert_msg('Table 10: hormone subsets')
  
  tables$classes <- class_bcg$result_tbl %>% 
    select(-class) %>% 
    set_names(c('Variable', 
                'Neutral', 
                'Testicle', 
                'Pituitary', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_10_hormone_subsets', 
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

# Table 11: hormone classes and marker status -------
  
  insert_msg('Table 11: hormone subsets and marker status')
  
  tables$classes_markers <- class_mark$result_table %>% 
    compress(names_to = 'subset') %>%
    relocate(subset) %>% 
    set_names(c('Hormonal subset', 
                'Variable', 
                levels(class_mark$analysis_tbl[[1]]$marker_status), 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_11_differences_hormon_subsets_marker', 
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
  
  
# Table 12: Elastic Net Cox regression -----
  
  insert_msg('Table 12: Elastic Net Cox regression')
  
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
                          label), 
           label = ifelse(is.na(label), 
                          'Hormonal subset', 
                          label))
  
  tables$elastic_net <- 
    tibble(`Explanatory variable` = paste(tables$elastic_net$label, 
                                          collapse = ', ')) %>% 
    mdtable(label = 'table_12_elastic_net_variables', 
            ref_name = 'elastic_net', 
            caption = paste('Explanatory variables in multi-parameter', 
                            'Elastic Cox modeling of relapse-free survival.'))
  
# Table 13: TCGA cohort characteristic ------
  
  insert_msg('Table 13: characteristic of the TCGA cohort')
  
  tables$tcga_cohort <- cohort$desc_stats$tcga %>% 
    select(variable, statistic) %>% 
    set_names(c('Variable', 'Statistic')) %>% 
    mdtable(label = 'table_13_tcga_cohort_features', 
            ref_name = 'tcga_cohort', 
            caption = paste('Demographic and cancer-related characteristic', 
                            'of the TCGA cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 14: Genes of interest -------
  
  insert_msg('Table 14: genes of interest')
  
  tables$tcga_genes <- tcga$gene_lexicon %>% 
    mutate(entrez_id = exchange(gene_symbol, 
                                dict = tcga$annotation, 
                                key = 'gene_symbol', 
                                value = 'entrez_id')) %>% 
    select(class, gene_symbol, entrez_id) %>% 
    set_names(c('Gene class', 'Gene symbol', 'Entrez ID')) %>% 
    mdtable(label = 'table_14_tcga_genes_of_interest', 
            ref_name = 'tcga_genes', 
            caption = paste('Hormone-related genes of interest investigated', 
                            'in the TCGA cohort.'))
  
# Table 15: differences in hormone genes between the histologies -----
  
  insert_msg('Table 15: TCGA, histologies')
  
  tables$tcga_histology <- tcga_exp$result_tbl %>% 
    set_names(c('Variable', 'Seminoma', 'NSGCT', 
                'Significance', 'Effect size')) %>% 
    mdtable(label = 'table_15_tcga_histology', 
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
  
# Table 16: TCGA, subset assignment ------
  
  insert_msg('Table 16: TCga subset assignment')
  
  tables$tcga_assignment <- 
    left_join(tcga_mix$posterior %>% 
                as.data.frame %>% 
                rownames_to_column('ID'), 
              tcga_mix$assignment, 
              by = 'ID') %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    set_names(c('Patient ID', 
                levels(tcga_mix$assignment$class), 
                'Hormonal subset')) %>% 
    as_tibble %>% 
    mdtable(label = 'table_16_tcga_subset_assignment', 
            ref_name = 'tcga_assignment', 
            caption = paste('Assignment of the TCGA cancer samples to', 
                            'the hormonal subsets by Gaussian mixture',
                            'modeling.', 
                            'Posterior probabilities for the subset assignemnt', 
                            'and the final hormonal subset classification', 
                            'are presented.', 
                            'The table is available in a supplementary', 
                            'Excel file.'))
  
# Table 17: Demographic and clinical characteristic of the subsets -----
  
  insert_msg('Table 17: demography and clinics of the hormonal subsets, TCGA')
  
  tables$tcga_class_clinic <- 
    tcga_bcg$result_tbl %>% 
    set_names(c('Variable', 
                levels(tcga_bcg$analysis_tbl$class), 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_17_tcga_subset_clnics', 
            ref_name = 'tcga_class_clinic', 
            caption = paste('Demographic and clinical characteristic of',
                            'the hormonal subsets of the TCGA cohort cancers.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'Categorical variables are presented', 
                            'as percentages and counts within the', 
                            'complete observation set.'))
  
# Table 18: TCGA, GSVA -----
  
  insert_msg('Table 18: TCGA, GSVA')
  
  tables$tcga_gsva <- tcga_biology$significant_lm %>% 
    map_dfr(filter, regulation != 'ns') %>% 
    transmute( Comparison = paste(level, 'vs #1'), 
               Pathway = resp_label, 
               `Fold-regulation` = signif(estimate, 2), 
               `Lower CI` = signif(lower_ci, 2), 
               `Upper CI` = signif(upper_ci, 2), 
               pFDR = signif(p_adjusted)) %>% 
    mdtable(label = 'table_18_tcga_gsva_reactome', 
            ref_name = 'tcga_gsva', 
            caption = paste('Significant differences in ssGSEA scores', 
                            'of the Reactome pathways between the homonal', 
                            'subsets of the TCGA cohort.', 
                            'The table is available in a supplementary Excel', 
                            'file.'))
  
# Table 19: immunity ------
  
  insert_msg('Table 19: TCGA, immunity')
  
  tables$tcga_infiltration <- 
    list(`QuanTIseq` = tcga_quantiseq$result_tbl, 
         `xCell` = tcga_xcell$result_tbl) %>% 
    compress(names_to = 'algorithm') %>% 
    filter(!stri_detect(significance, regex = '^ns') | is.na(significance)) %>% 
    relocate(algorithm) %>% 
    set_names(c('Algorithm', 
                'Variable',
                levels(tcga_quantiseq$analysis_tbl$class), 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_19_tcga_infiltration', 
            ref_name = 'tcga_infiltration', 
            caption = paste('Significant differences in TME composition', 
                            'and immunity signatures estimated by the QuanTIseq', 
                            'and xCell algorithms between the hormonal subsets', 
                            'of the TCGA cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.'))
  
# Table 20: differential gene expression ------
  
  insert_msg('Table 20: TCGA, differential gene expression')
  
  tables$tcga_dge <- tcga_dge$significant_lm %>% 
    map_dfr(transmute, 
            `Comparison` = paste(level, 'vs #1'), 
            `Gene symbol` = gene_symbol, 
            `Fold-regulation` = signif(estimate, 2), 
            `Lower CI` = signif(lower_ci, 2), 
            `Upper CI` = signif(upper_ci, 2), 
            pFDR = signif(p_adjusted)) %>% 
    mdtable(label = 'table_20_tcga_differential_gene_expression', 
            ref_name = 'tcga_dge', 
            caption = paste('Genes differentially expressed between', 
                            'the hormonal subsets of the TCGA cohort.', 
                            'The table is available as a supplementary', 
                            'Excel file.'))
  
# Table 21: signaling -------
  
  insert_msg('Table 21: signaling')
  
  tables$tcga_signaling <- tcga_spia$test %>% 
    map(filter, pGFdr < 0.05) %>% 
    compress(names_to = 'level') %>% 
    transmute(`Comparison` = paste(level, 'vs #1'), 
              `KEGG Pathway` = Name, 
              `KEGG ID` = ID, 
              `Fold-regulation` = signif(tA, 3), 
              pFDR = signif(pGFdr)) %>% 
    mdtable(label = 'table_21_tcga_signaling', 
            ref_name = 'tcga_signaling', 
            caption = paste('Differential activation of KEGG-listed',
                            'signaling pathways between the homonal subsets', 
                            'of the TCGA cohort predicted by the SPIA', 
                            'algorithm. The table is available as',
                            'a supplementary Excell table.'))
  
# Table 22: differential protein expression ------
  
  insert_msg('Table 22: differential protein expression')
  
  tables$tcga_protein <- tcga_protein$result_tbl %>% 
    filter(!stri_detect(significance, regex = '^ns') | is.na(significance)) %>% 
    set_names(c('Variable', 
                levels(tcga_protein$analysis_tbl$class), 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_22_tcga_differential_protein_expression', 
            ref_name = 'tcga_protein', 
            caption = paste('Differential protein expression', 
                            'between the hormonal subsets of the TCGA cohort.', 
                            'Numeric variables are presented as', 
                            'medians with interqurtile ranges (IQR)', 
                            'and ranges.', 
                            'The table is available as a supplementary', 
                            'Excel file'))

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