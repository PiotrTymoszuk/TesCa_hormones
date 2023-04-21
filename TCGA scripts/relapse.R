# Relapse-free Survival in the hormonal classes
# Peto-Peto test

  insert_head()
  
# container ------
  
  tcga_relapse <- list()
  
# Analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## identity, non-transformed gene expression
  
  tcga_relapse$lexicon <- tcga$gene_lexicon %>% 
    filter(!gene_symbol %in% c('FSHB'))
  
  tcga_relapse$analysis_tbl <- tcga$expression %>% 
    select(ID, all_of(tcga_globals$clust_genes)) %>% 
    column_to_rownames('ID') %>% 
    as.matrix
  
  tcga_relapse$analysis_tbl <- 2^(tcga_relapse$analysis_tbl) - 1
  
  tcga_relapse$analysis_tbl <- tcga_relapse$analysis_tbl %>% 
    as.data.frame %>% 
    rownames_to_column('ID') %>% 
    left_join(tcga_mix$assignment, 
              by = 'ID') %>% 
    left_join(tcga$clinical[c('ID', 'relapse', 'rfs_days')], 
              by = 'ID') %>% 
    as_tibble %>% 
    map_dfc(unname) %>% 
    filter(complete.cases(.))
  
  ## numbers of events

  tcga_relapse$event_n <- tcga_relapse$analysis_tbl %>% 
    count(factor(relapse))
  
  tcga_relapse$event_n <- paste0('total: n = ', 
                              sum(tcga_relapse$event_n$n), 
                              ', events: n = ', 
                              tcga_relapse$event_n$n[2])
    
# Gene cutpoints ------
  
  insert_msg('Gene cutpoints')
  
  ## stratification
  
  tcga_relapse$cutpoints$cut_obj <- 
    surv_cutpoint(data = tcga_relapse$analysis_tbl, 
                  time = 'rfs_days', 
                  event = 'relapse', 
                  variables = tcga_relapse$lexicon$gene_symbol, 
                  minprop = 0.2)
  
  tcga_relapse$cutpoints$strata_tbl <- tcga_relapse$cutpoints$cut_obj %>% 
    surv_categorize
  
  ## testing for differences
  
  tcga_relapse$cutpoints$survfit_obj <- tcga_relapse$lexicon$gene_symbol %>% 
    paste('Surv(rfs_days, relapse) ~', .) %>% 
    map(formula) %>% 
    map(survminer::surv_fit, 
        data = tcga_relapse$cutpoints$strata_tbl) %>% 
    set_names(tcga_relapse$lexicon$gene_symbol)

  tcga_relapse$cutpoints$test <- tcga_relapse$cutpoints$survfit_obj %>% 
    map_dfr(surv_pvalue, method = 'S1') %>% 
    adjust_fdr(variable = 'pval')
  
  ## Kaplan-Meier plots
  
  tcga_relapse$cutpoints$strata_n <- 
    tcga_relapse$cutpoints$strata_tbl[tcga_relapse$lexicon$gene_symbol] %>% 
    map(table) %>% 
    map(~map2_chr(names(.x), .x, paste, sep = ': n = '))
  
  tcga_relapse$cutpoints$plots <- 
    list(fit = tcga_relapse$cutpoints$survfit_obj, 
         title = paste0('<b><em>', 
                        names(tcga_relapse$cutpoints$survfit_obj), 
                        '</em>, TCGA<b>'), 
         pval = tcga_relapse$cutpoints$test$significance, 
         legend.labs = tcga_relapse$cutpoints$strata_n) %>% 
    pmap(ggsurvplot, 
         palette = c('firebrick', 'steelblue'), 
         xlab = 'Relapse-free survival, days', 
         pval.size = 2.75, 
         legend.title = '') %>% 
    map(~.x$plot) %>% 
    map2(., 
         paste('Cutpoint = ', 
               signif(tcga_relapse$cutpoints$cut_obj$cutpoint$cutpoint, 3), 
               'gene copies'), 
         ~.x + 
           labs(subtitle = .y) + 
           globals$common_theme + 
           theme(plot.title = element_markdown()))
  
# Survival in the hormonal subsets -----
  
  insert_msg('Survival in the subsets')
  
  ## comparisons with the subset #1 with the longest survival
  
  tcga_relapse$classes$survfit_obj <- 
    list(global = c('#1', '#2', '#3', '#4', '#5'), 
         s2_vs_s1 = c('#2', '#1'), 
         s3_vs_s1 = c('#1', '#3'), 
         s4_vs_s1 = c('#1', '#4'), 
         s5_vs_s1 = c('#1', '#5')) %>% 
    map(~filter(tcga_relapse$analysis_tbl, class %in% .x)) %>% 
    map(survminer::surv_fit, formula = Surv(rfs_days, relapse) ~ class)

  tcga_relapse$classes$test <- tcga_relapse$classes$survfit_obj %>% 
    map(surv_pvalue, method = 'S1') %>% 
    compress(names_to = 'comparison') %>% 
    adjust_fdr(variable = 'pval', 'BH') %>% 
    mutate(comparison = car::recode(comparison, 
                                    "'global' = 'global'; 
                                    's2_vs_s1' = '#2 vs #1'; 
                                    's3_vs_s1' = '#3 vs #1'; 
                                    's4_vs_s1' = '#4 vs #1'; 
                                    's5_vs_s1' = '#5 vs #1'"))
  
  ## Kaplan-Meier plots
  
  tcga_relapse$classes$pval_lab <- 
    map2_chr(tcga_relapse$classes$test$comparison, 
             tcga_relapse$classes$test$significance, 
             paste, sep = ': ') %>% 
    paste(collapse = '\n')
  
  tcga_relapse$classes$strata_n <- tcga_relapse$analysis_tbl %>% 
    count(class)
  
  tcga_relapse$classes$strata_n <- 
    map2_chr(tcga_relapse$classes$strata_n[[1]], 
             tcga_relapse$classes$strata_n[[2]], 
             paste, sep = ': n = ')
  
  tcga_relapse$classes$plot <- 
    ggsurvplot(fit = tcga_relapse$classes$survfit_obj$global, 
               title = 'Hormonal subsets, TCGA', 
               pval = tcga_relapse$classes$pval_lab, 
               legend.title = '', 
               legend.labs = tcga_relapse$classes$strata_n, 
               palette = unname(tcga_globals$clust_colors), 
               pval.size = 2.75, 
               pval.coord = c(0.16, 0.15), 
               xlab = 'Relapse-free survival, days')$plot + 
    globals$common_theme + 
    labs(subtitle = tcga_relapse$event_n)
  
# END -----
  
  insert_tail()