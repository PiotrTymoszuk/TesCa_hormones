# Analysis of amplifications and deletions of particular genes

  insert_head()
  
# contaioner -------
  
  tcga_cna <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## separate analysis tables for amplifications and deletions
  
  tcga_cna$gistic_mtx <- tcga$gistic %>% 
    select(-sample_id) %>% 
    column_to_rownames('ID') %>% 
    as.matrix
  
  tcga_cna$analysis_mtx$amplified <- tcga_cna$gistic_mtx > 0
  tcga_cna$analysis_mtx$deleted <- tcga_cna$gistic_mtx < 0
  
# Frequency of amplification and deletions in the entire collective -------
  
  insert_msg('Frequency of CNA, whole cohort')
  
  tcga_cna$cna_freq <- tcga_cna$analysis_mtx %>% 
    map(colSums) %>% 
    map(compress, 
        names_to = 'gene_symbol', 
        values_to = 'n') %>% 
    map(mutate, 
        percent = n/nrow(tcga_cna$gistic_mtx) * 100) %>% 
    map(arrange, 
        -percent)
  
  ## top CNA present in at least 5% of the samples
  
  tcga_cna$top_cna <- tcga_cna$cna_freq %>% 
    map(filter, percent >= 5) %>% 
    map(~.x$gene_symbol)
  
  ## plotting
  
  tcga_cna$top_plot <- 
    list(x = tcga_cna$cna_freq %>% 
           map(top_n, n = 20, percent), 
         y = c('Top amplified genes', 'Top deleted genes'), 
         z = c('coral3', 'steelblue')) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = percent, 
                      y = reorder(gene_symbol, percent))) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = z) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank(), 
                 axis.text.y = element_text(face = 'italic')) + 
           labs(title = y, 
                x = '% of samples'))
  
# Analysis tables with the top most frequent CNA -------
  
  insert_msg('Analysis tables with the top CNA')
  
  tcga_cna$analysis_tbl <- 
    map2(tcga_cna$analysis_mtx, 
         list(c('amplified', 'non-amplified'), 
              c('deleted', 'non-deleted')), 
         ~ifelse(.x, .y[1], .y[2])) %>% 
    map2(., tcga_cna$top_cna, 
         ~.x[, .y]) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'ID') %>% 
    map(as_tibble)
  
  ## setting the factor levels
  
  levs <- list(amplified = c('non-amplified', 'amplified'), 
               deleted = c('non-deleted', 'deleted'))
  
  for(i in names(tcga_cna$analysis_tbl)) {
    
    tcga_cna$analysis_tbl[[i]][tcga_cna$top_cna[[i]]] <- 
      tcga_cna$analysis_tbl[[i]][tcga_cna$top_cna[[i]]] %>% 
      map_dfc(factor, levels = levs[[i]])
    
  }
  
  tcga_cna$analysis_tbl <- tcga_cna$analysis_tbl %>% 
    map(inner_join, tcga_mix$assignment, by = 'ID') %>% 
    map(~map_dfc(.x, unname))
  
# Number of samples in the hormonal subsets --------
  
  insert_msg('N numbers')
  
  tcga_cna$strata_n <- tcga_cna$analysis_tbl %>% 
    map(count, class) %>% 
    map(set_names, c('class', 'n_subset'))
  
# Descriptive stats --------
  
  insert_msg('Descriptive stats')
  
  ## frequencies per hormonal subset
  
  tcga_cna$stats <- tcga_cna$analysis_tbl %>% 
    map(column_to_rownames, 'ID') %>% 
    map(blast, class)
  
  for(i in names(tcga_cna$stats)) {
    
    tcga_cna$stats[[i]]  <- tcga_cna$stats[[i]] %>% 
      map(~map_dfc(.x, ~as.numeric(.x) - 1))
    
  }
  
  tcga_cna$stats <- tcga_cna$stats %>% 
    map(~map(.x, colSums) %>% 
          map(compress, 
              names_to = 'gene_symbol', 
              values_to = 'n')) %>% 
    map(compress, 
        names_to = 'class') %>% 
    map2(., tcga_cna$strata_n, 
         left_join, by = 'class') %>% 
    map(mutate, percent_subset = n/n_subset  * 100)
  
# Serial testing ------
  
  insert_msg('Serial testing')
  
  tcga_cna$test <- 
    map2(tcga_cna$analysis_tbl, 
         tcga_cna$top_cna, 
         ~compare_variables(.x, 
                            variables = .y, 
                            split_factor = 'class', 
                            what = 'eff_size', 
                            types = 'cramer_v', 
                            exact = FALSE, 
                            ci = FALSE, 
                            pub_styled = FALSE, 
                            adj_method = 'BH', 
                            .parallel = TRUE, 
                            .paropts = furrr_options(seed = TRUE, 
                                                     globals = c('tcga_cna')))) %>% 
    map(mutate, 
        plot_cap = paste('V =', signif(estimate, 2)), 
        plot_cap = paste(plot_cap, significance, sep = ', '))
  
# Similarity -------
  
  insert_msg('Similarity of the hormonal subsets')
  
  ## dimensionality reduction
  
  tcga_cna$similarity$red_obj <- tcga_cna$analysis_tbl %>% 
    map(~map_dfc(.x, function(x) if(is.factor(x)) as.numeric(x) - 1 else x) %>% 
          column_to_rownames('ID') %>% 
          select(-class) %>% 
          reduce_data(distance_method = 'jaccard', 
                      kdim = 2, 
                      red_fun = 'mds'))
  
  ## plotting
  
  set.seed(1234)
  
  tcga_cna$similarity$plots <- 
    list(red_obj = tcga_cna$similarity$red_obj, 
         plot_title = paste('Similarity of hormonal subsets,', 
                            c('gene amplification', 
                              'gene deletion'))) %>% 
    pmap(plot_similarity, 
         class_assignment = tcga_cna$analysis_tbl[[1]][c('ID', 'class')], 
         plot_subtitle = '2D MDS, Jaccard distance', 
         distance = 'Jaccard', 
         min_max_similarity = TRUE, 
         weighting_order = 1, 
         net_theme = theme_void() + 
           theme(plot.margin = globals$common_margin))

# saving the results ------
  
  insert_msg('Saving the results')
  
  save(tcga_cna, file = './cache/cna.RData')
  
# END -------
  
  rm(i, levs)

  insert_tail()