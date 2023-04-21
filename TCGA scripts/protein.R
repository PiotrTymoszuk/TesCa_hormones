# Differences in protein expression between the hormonal subsets

  insert_head()
  
# container ------
  
  tcga_protein <- list()

# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## sorting out the undetectable proteins
  
  tcga_protein$lexicon <- tcga$protein_lexicon %>% 
    filter(!variable %in% c('ARID1A|ARID1A', 
                            'BRCA2|BRCA2', 
                            'CASP3|Caspase-3', 
                            'CASP9|Caspase-9')) %>% 
    mutate(label = stri_replace_all(variable, fixed = '_', replacement = ' '))
  
  tcga_protein$analysis_tbl <- 
    inner_join(tcga_mix$assignment, 
               tcga$protein[c('ID', tcga_protein$lexicon$variable)], 
               by = 'ID') %>% 
    map_dfc(unname)
  
  ## normalized protein expression: heat map plotting
  
  tcga_protein$norm_table <- 
    tcga$protein[c('ID', tcga_protein$lexicon$variable)] %>% 
    column_to_rownames('ID') %>% 
    center_data('mean') %>% 
    rownames_to_column('ID') %>%
    inner_join(tcga_mix$assignment, 
               by = 'ID') %>% 
    map_dfc(unname)
  
  ## n numbers 
  
  tcga_protein$strata_n <- tcga_protein$analysis_tbl %>% 
    count(class)
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats ')
  
  ## normality: violated in multiple cases
  
  tcga_protein$normality <- tcga_protein$analysis_tbl %>% 
    explore(variables = tcga_protein$lexicon$variable, 
            split_factor = 'class', 
            what = 'normality', 
            pub_styled = TRUE)
  
  ## descriptive stats
  
  tcga_protein$stats <- tcga_protein$analysis_tbl %>% 
    explore(variables = tcga_protein$lexicon$variable, 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(tcga_protein$analysis_tbl$class)))
  
# Testing --------
  
  insert_msg('Testing')
  
  ## Kruskal-Wallis test
  
  tcga_protein$test <- tcga_protein$analysis_tbl %>% 
    compare_variables(variables = tcga_protein$lexicon$variable, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = FALSE, 
                      adj_method = 'BH', 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('tcga_protein'))) %>% 
    mutate(eff_size = paste('\u03B7\u00B2 =', signif(estimate, 2)), 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting the effects size and significance ------
  
  insert_msg('Plotting the effect size and signicance')
  
  tcga_protein$eff_plot <- 
    plot(tcga_protein$test, 
         point_hjitter = 1, 
         point_wjitter = 0.01, 
         show_labels = 'signif', 
         txt_size = 2.75, 
         cust_theme = globals$common_theme) + 
    labs(title = 'Differences between homonal subsets, TCGA', 
         x = expression('Effect size, ' * eta^2), 
         y = expression('-log'[10] * ' pFDR'), 
         subtitle = paste('n =', nrow(tcga_protein$analysis_tbl)))

# parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# Differences between the subsets ------
  
  insert_msg('Post-hoc testing')
  
  ## Mann-Whitney U test with r effect size metric
  
  tcga_protein$post_hoc <- list('#2' = c('#1', '#2'), 
                                '#3' = c('#1', '#3'), 
                                '#4' = c('#1', '#4')) %>% 
    future_map(~compare_variables(tcga_protein$analysis_tbl %>% 
                                    filter(class %in% .x), 
                                  variables = tcga_protein$lexicon$variable, 
                                  split_factor = 'class', 
                                  what = 'eff_size', 
                                  types = 'wilcoxon_r', 
                                  exact = FALSE, 
                                  ci = FALSE, 
                                  pub_styled = FALSE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE))
  
# Significant proteins ------
  
  insert_msg('Significant proteins')
  
  ## significance and large effect size of regulation
  
  tcga_protein$significant_kruskal <- tcga_protein$test %>% 
    filter(p_adjusted < 0.05, 
           estimate > 0.14) %>% 
    .$variable
  
  ## significant post-hoc results
  ## stong regulation
  
  tcga_protein$signficant_posthoc <- tcga_protein$post_hoc %>% 
    map(filter, 
        variable %in% tcga_protein$significant_kruskal, 
        p_adjusted < 0.05, 
        estimate > 0.3) %>% 
    map(~.x$variable)
  
  ## proteins differentiating between the clusters
  
  tcga_protein$cmm_significant <- 
    tcga_protein$signficant_posthoc %>% 
    reduce(union)
  
# Protein plotting order: hierarchical clustering ------
  
  insert_msg('Protein clusters')
  
  tcga_protein$prot_clust <- tcga_protein$norm_table %>% 
    column_to_rownames('ID') %>% 
    select(all_of(tcga_protein$cmm_significant)) %>% 
    t %>% 
    as.data.frame %>% 
    kcluster(k = 2, clust_fun = 'pam')
  
  tcga_protein$heat_map_order <- tcga_protein$prot_clust$clust_assignment %>% 
    arrange(clust_id) %>% 
    set_names(c('variable', 'clust_id'))

# Heat map ------
  
  insert_msg('Heat map')
  
  ## of the proteins differentially expressed between the subsets
  
  tcga_protein$heat_map <- 
    draw_class_hm(data = tcga_protein$norm_table[c('ID', 'class', tcga_protein$cmm_significant)], 
                  variables = tcga_protein$heat_map_order$variable, 
                  plot_title = 'Hormonal cluster, protein expression, TCGA', 
                  limits = c(-3, 3), 
                  oob = scales::squish, 
                  name = 'Z-score')
  
  ## plotting the protein clustering more explicitly
  
  tcga_protein$heat_map$data <- 
    left_join(tcga_protein$heat_map$data %>% 
                mutate(variable = as.character(variable)), 
              tcga_protein$heat_map_order, 
              by = 'variable')
  
  tcga_protein$heat_map <- tcga_protein$heat_map + 
    facet_grid(clust_id ~ class, 
               scales = 'free', 
               space = 'free') + 
    theme(strip.background.y = element_blank(), 
          strip.text.y = element_blank())
  
# Violin plots for single proteins ------
  
  insert_msg('Plots for single proteins')
  
  tcga_protein$plots <- 
    list(variable = tcga_protein$test$variable, 
         plot_title = paste0(tcga_protein$test$variable, 
                            ', TCGA'), 
         plot_subtitle = tcga_protein$test$plot_cap) %>% 
    future_pmap(plot_variable, 
                tcga_protein$analysis_tbl, 
                split_factor = 'class', 
                type = 'box', 
                cust_theme = globals$common_theme, 
                y_lab = 'Relative expression, AU', 
                x_n_labs = TRUE, 
                point_hjitter = 0, 
                .options = furrr_options(seed = TRUE)) %>% 
    map(~.x + 
          scale_fill_manual(values = tcga_globals$clust_colors)) %>% 
    set_names(tcga_protein$test$variable)
  
# Result table ------
  
  insert_msg('Result table')
  
  tcga_protein$result_tbl <- 
    left_join(tcga_protein$stats, 
              tcga_protein$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(lexicon = tcga_protein$lexicon, 
               rm_complete = TRUE) %>% 
    full_rbind(tibble(variable = 'Samples, n', 
                      `#1` = tcga_protein$strata_n$n[1], 
                      `#2` = tcga_protein$strata_n$n[2], 
                      `#3` = tcga_protein$strata_n$n[3], 
                      `#4` = tcga_protein$strata_n$n[4], 
                      `#5` = tcga_protein$strata_n$n[5]), .)
  
# END ------
  
  plan('sequential')
  
  insert_tail()
  