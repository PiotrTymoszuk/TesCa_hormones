# Differential gene expression in the TCGA cohort - differences between
# the hormonal subsets
#
# Done with one-way ANOVA and linear modeling with log2-transformed gene counts
# Significant genes are identified by the following criteria: 
# 1) Significant main effect of ANOVA (FDR-corrected)
# 2) Significant, at least 2-fold regulation in LM for the given subset

  insert_head()
  
# container -----
  
  tcga_dge <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  tcga_dge$lexicon <- tcga$annotation
  
  ## analysis table: log2-counts
  
  tcga_dge$analysis_tbl <- 
    left_join(tcga_mix$assignment, 
              tcga$expression[c('ID', tcga_dge$lexicon$gene_symbol)],
              by = 'ID')
  
# Identification of variable genes ------
  
  insert_msg('Identification of variable genes')
  
  ## Gini coefficient > 0.05
  
  tcga_dge$gini <- tcga_dge$analysis_tbl[tcga_dge$lexicon$gene_symbol] %>% 
    map_dbl(Gini) %>% 
    compress(names_to = 'gene_symbol', 
             values_to = 'gini_coef')
  
  tcga_dge$var_genes <- tcga_dge$gini %>% 
    filter(gini_coef > 0.05) %>% 
    .$gene_symbol
  
# Testing -------
  
  insert_msg('Testing')
  
  ## done with the SEM1 and NS PRL as baselines
  
  tcga_dge$test <- list(SEM = tcga_dge$analysis_tbl, 
                        NSGCT = tcga_dge$analysis_tbl %>% 
                          mutate(class = new_baseline(class, 'NS PRL'))) %>% 
    map(~.x[c('ID', 'class', tcga_dge$var_genes)]) %>% 
    map(test_anova, 
        split_fct = 'class', 
        variables = tcga_dge$var_genes, 
        adj_method = 'BH', 
        .parallel = TRUE)

  ## annotation of the results with Entrez IDs
  
  for(i in names(tcga_dge$test)) {
    
    tcga_dge$test[[i]] <- tcga_dge$test[[i]] %>% 
      map(mutate, 
          gene_symbol = response,
          entrez_id = exchange(gene_symbol, 
                               dict = tcga_dge$lexicon, 
                               key = 'gene_symbol', 
                               value = 'entrez_id'), 
          entrez_id = unname(entrez_id))
    
    tcga_dge$test[[i]]$lm <- tcga_dge$test[[i]]$lm %>% 
      select(-counfounder)
    
  }
  
# Significant genes -------
  
  insert_msg('Significant genes')
  
  ## ANOVA, main effect
  
  tcga_dge$significant_anova <- tcga_dge$test[[1]]$anova %>% 
    filter(p_adjusted < 0.05) %>% 
    .$gene_symbol
  
  ## particular subsets compared with the subset #1
  
  tcga_dge$cleared_lm <- tcga_dge$test %>% 
    map(~.x$lm) %>% 
    map(filter, 
        gene_symbol %in% tcga_dge$significant_anova, 
        level != '(Intercept)') %>% 
    map(mutate, 
        regulation = ifelse(p_adjusted >= 0.05, 
                            'ns', 
                            ifelse(estimate > log(1.5), 
                                   'upregulated', 
                                   ifelse(estimate < -log(1.5), 
                                          'downregulated', 
                                          'ns'))), 
        regulation = factor(regulation, 
                            c('upregulated', 'downregulated', 'ns')))
  
  tcga_dge$significant_lm <- tcga_dge$cleared_lm %>% 
    map(filter, regulation != 'ns')
  
# Similarity -------
  
  insert_msg('Similarity of the hormonal subsets')
  
  ## dimensionality reduction
  
  tcga_dge$similarity$red_obj <- 
    tcga_dge$analysis_tbl[c('ID', tcga_dge$var_genes)] %>% 
    column_to_rownames('ID') %>% 
    center_data('median') %>% 
    reduce_data(distance_method = 'cosine', 
                kdim = 2, 
                red_fun = 'mds')
  
  ## plotting
  
  set.seed(1234)
  
  tcga_dge$similarity$plots <- 
    plot_similarity(tcga_dge$similarity$red_obj, 
                    class_assignment = tcga_dge$analysis_tbl[c('ID', 'class')], 
                    plot_title = 'Similarity of hormonal subsets, protein expression', 
                    plot_subtitle = '2D MDS, cosine distance', 
                    distance = 'cosine', 
                    min_max_similarity = TRUE, 
                    weighting_order = 1, 
                    net_theme = theme_void() + 
                      theme(plot.margin = globals$common_margin))
  
# END ------
  
  rm(i)
  
  insert_tail()