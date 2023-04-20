# Hormone-related gene expression in the GMM classes

  insert_head()
  
# container -----
  
  tcga_class <- list()
  
# Analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## normalized gene expression
  
  tcga_class$lexicon <- tcga$gene_lexicon
  
  tcga_class$analysis_tbl <- tcga_mix$analysis_tbl %>% 
    rownames_to_column('ID') %>% 
    left_join(tcga_mix$assignment, 
              by = 'ID') %>% 
    as_tibble %>% 
    map_dfc(unname)
  
  ## genuine scale
  
  tcga_class$identity_tbl <- tcga$expression %>% 
    select(ID, tcga_globals$clust_genes) %>% 
    left_join(tcga_mix$assignment, 
              by = 'ID') %>% 
    as_tibble %>% 
    map_dfc(unname)
  
  
  ## n numbers
  
  tcga_class$class_n <- tcga_class$analysis_tbl %>% 
    count(class)
  
  tcga_class$class_labels <- 
    map2_chr(tcga_class$class_n[[1]], 
             tcga_class$class_n[[2]], 
             paste, sep = ': n = ')
  
# Descriptive stats -----
  
  insert_msg('Descriptive stats')

  tcga_class$stats <- tcga_class$identity_tbl %>% 
    explore(split_factor = 'class', 
            variables = tcga_class$lexicon$gene_symbol, 
            pub_styled = TRUE, 
            what = 'table')
  
# Testing for differences: Kruskal-Wallis test -----
  
  insert_msg('Testing for differences')
  
  tcga_class$test <- tcga_class$identity_tbl %>% 
    compare_variables(variables = tcga_class$lexicon$gene_symbol, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           ax_lab = paste0('<em>', variable, '</em><br>', 
                           plot_cap))
  
# Plots for single variables ------
  
  insert_msg('Single variables')
  
  tcga_class$plots <- 
    list(variable = tcga_class$test$variable, 
         plot_title = paste0('<b><em>', 
                             tcga_class$test$variable, 
                             '</em>, TCGA</b>'), 
         plot_subtitle = tcga_class$test$plot_cap) %>% 
    pmap(plot_variable, 
         tcga_class$identity_tbl, 
         split_factor = 'class', 
         type = 'violin', 
         cust_theme = globals$common_theme, 
         y_lab = expression('log'[2] * ' gene count'), 
         x_n_labs = TRUE, 
         point_hjitter = 0) %>% 
    map(~.x + 
          theme(plot.title = element_markdown()) + 
          scale_fill_manual(values = tcga_globals$clust_colors)) %>% 
    set_names(tcga_class$test$variable)

# Ribbon plots ------
  
  insert_msg('Ribbon plots')
  
  tcga_class$ribbon_plot <- tcga_class$analysis_tbl %>% 
    draw_stat_panel(data = tcga_class$analysis_tbl, 
                    variables = tcga_class$lexicon$gene_symbol, 
                    split_factor = 'class', 
                    stat = 'mean', 
                    err_stat = '2se', 
                    alpha = 0.5, 
                    form = 'line', 
                    plot_title = 'Hormone-related gene expression', 
                    plot_subtitle = 'Hormonal subsets of testicle cancers', 
                    cust_theme = globals$common_theme, 
                    x_lab = 'mean expression Z scores, 2 \u00D7 SEM') + 
    scale_fill_manual(values = tcga_globals$clust_colors, 
                      labels = tcga_class$class_labels, 
                      name = '') + 
    scale_color_manual(values = tcga_globals$clust_colors, 
                       labels = tcga_class$class_labels, 
                       name = '') + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_markdown()) + 
    scale_y_discrete(limits = c('HSD17B3', 
                                'CYP17A1', 
                                'HSD3B2', 
                                'CYP11A1', 
                                'LHB', 
                                'HSD17B1', 
                                'CYP19A1', 
                                'HSD3B1', 
                                'CGA', 
                                'PRL', 
                                'FSHB'), 
                     labels = set_names(tcga_class$test$ax_lab, 
                                        tcga_class$test$variable))
  
# END ------
  
  insert_tail()