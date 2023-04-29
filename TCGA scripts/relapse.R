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
               'mRNA copies'), 
         ~.x + 
           labs(subtitle = .y) + 
           globals$common_theme + 
           theme(plot.title = element_markdown()))
  
# Survival in the hormonal subsets -----
  
  insert_msg('Survival in the subsets')
  
  ## subset pairs
  
  tcga_relapse$classes$comparisons <- 
    levels(tcga_relapse$analysis_tbl$class) %>% 
    combn(2, simplify = FALSE)
  
  tcga_relapse$classes$comparisons <- tcga_relapse$classes$comparisons %>% 
    set_names(map_chr(tcga_relapse$classes$comparisons, 
                      paste, collapse = '_'))
  
  tcga_relapse$classes$comparisons <- 
    c(list(global = levels(tcga_relapse$analysis_tbl$class)), 
      tcga_relapse$classes$comparisons)
    
  ## comparisons with the subset #1 with the longest survival
  
  tcga_relapse$classes$survfit_obj <- 
    tcga_relapse$classes$comparisons %>% 
    map(~filter(tcga_relapse$analysis_tbl, class %in% .x)) %>% 
    map(survminer::surv_fit, formula = Surv(rfs_days, relapse) ~ class)

  tcga_relapse$classes$test <- tcga_relapse$classes$survfit_obj %>% 
    map(surv_pvalue, method = 'S1') %>% 
    compress(names_to = 'comparison') %>% 
    adjust_fdr(variable = 'pval', 'BH') %>% 
    mutate(comparison = stri_replace(comparison, 
                                     fixed = '_', replacement = ' vs '))
  
  ## Kaplan-Meier plots
  
  tcga_relapse$classes$pval_lab <- tcga_relapse$classes$test %>% 
    filter(comparison == 'global' | pval < 0.1) 
  
  tcga_relapse$classes$pval_lab <- 
    map2(tcga_relapse$classes$pval_lab$comparison, 
         tcga_relapse$classes$pval_lab$significance, 
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
  
# Result tables -------
  
  insert_msg('Result tables')
  
  ## Q25% and median survival times
  
  tcga_relapse$result_tbl$quantiles <- tcga_relapse$cutpoints$survfit_obj %>% 
    map(quantile, probs = c(0.25, 0.5)) %>% 
    map(~.x$quantile) %>% 
    map(set_colnames, c('quant25', 'median')) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'strata') %>% 
    compress(names_to = 'gene_symbol') %>% 
    mutate(strata = stri_extract(strata, regex = 'high|low'), 
           strata = factor(strata, c('low', 'high')))
  
  ## cutoffs
  
  tcga_relapse$result_tbl$cutoffs <- 
    tcga_relapse$cutpoints$cut_obj$cutpoint %>% 
    as.data.frame %>% 
    rownames_to_column('gene_symbol')
  
  ## n numbers
  
  tcga_relapse$result_tbl$strata_n <- 
    tcga_relapse$cutpoints$strata_tbl[tcga_relapse$lexicon$gene_symbol] %>% 
    map(table) %>% 
    map(~tibble(strata = factor(names(.x), c('low', 'high')), 
                n = as.numeric(.x))) %>% 
    compress(names_to = 'gene_symbol')
  
  ## testing results
  
  tcga_relapse$result_tbl$test <- tcga_relapse$cutpoints$test %>% 
    transmute(gene_symbol = variable, 
              significance = significance)
  
  ## the entire result table
  
  tcga_relapse$result_tbl <- 
    full_join(tcga_relapse$result_tbl$cutoffs, 
              tcga_relapse$result_tbl$strata_n, 
              multiple = 'all', 
              by = 'gene_symbol') %>% 
    left_join(tcga_relapse$result_tbl$quantiles, 
              by = c('gene_symbol', 'strata')) %>% 
    left_join(tcga_relapse$result_tbl$test, 
              by = 'gene_symbol') %>% 
    as_tibble
    
# END -----
  
  insert_tail()
  