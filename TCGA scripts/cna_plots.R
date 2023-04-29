# Plots for the top amplifications and deletions differentiating between 
# the hormonal subsets

  insert_head()
  
# container ------
  
  tcga_cplots <- list()
  
# parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')

  tcga_cplots$top_genes <- tcga_cna$test %>% 
    map(filter, 
        p_value < 0.05, 
        estimate > 0.25) %>% 
    map(arrange, estimate) %>% 
    map(mutate, 
        gene_symbol = variable, 
        eff_size = paste('V =', signif(estimate, 2)))
  
  tcga_cplots$stats <- tcga_cna$stats %>% 
    map2(., map(tcga_cplots$top_genes, ~.x$variable), 
         ~filter(.x, gene_symbol %in% .y))
    
  tcga_cplots$analysis_tbl <- tcga_cna$analysis_tbl %>% 
    map2(., map(tcga_cplots$top_genes, ~.x$variable), 
        ~.x[c('ID', 'class', .y)])
  
# Plotting --------
  
  insert_msg('Plotting')
  
  tcga_cplots$plots <- 
    list(x = tcga_cplots$analysis_tbl, 
         y =  tcga_cplots$top_genes, 
         z = list(c('cornsilk', 'coral3'), 
                  c('cornsilk', 'steelblue3'))) %>% 
    pmap(function(x, y, z) list(variable = y$variable, 
                                plot_title = paste0('<b><em>', y$variable, 
                                                    '</em>, TCGA</b>'), 
                                plot_subtitle = y$plot_cap) %>% 
           future_pmap(plot_variable, 
                       x, 
                       split_factor = 'class', 
                       type = 'stack', 
                       scale = 'percent', 
                       cust_theme = globals$common_theme, 
                       y_lab = '% of subset', 
                       x_n_labs = TRUE, 
                       .options = furrr_options(seed = TRUE)) %>% 
           map(~.x + 
                 scale_fill_manual(values = z, 
                                   name = '') + 
                 theme(plot.title = element_markdown(), 
                       axis.title.x = element_blank())) %>% 
           set_names(y$variable))
  
# Table with the results -----
  
  insert_msg('Table with the results for top CNA')
  
  tcga_cplots$result_tbl <- 
    map2(tcga_cplots$stats, 
         map(tcga_cplots$top_genes, 
             ~.x[c('gene_symbol', 'significance', 'eff_size')]), 
         left_join, by = 'gene_symbol') %>% 
    map(mutate, 
        entrez_id = exchange(gene_symbol, 
                             dict = tcga$annotation, 
                             key = 'gene_symbol', 
                             value = 'entrez_id'), 
        class = factor(class, levels(tcga_cplots$analysis_tbl[[1]]$class))) %>% 
    map(arrange, gene_symbol, class) %>% 
    compress(names_to = 'cna_type')
  
# END -------
  
  plan('sequential')
  
  insert_tail()