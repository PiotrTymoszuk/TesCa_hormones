# Plots for the results of differential gene expression

  insert_head()
  
# container ------
  
  tcga_dplots <- list()
  
# Numbers of differentially regulated genes ------
  
  insert_msg('Numbers of differentially regulated genes')
  
  ## plotting data
  
  tcga_dplots$gene_n$data <- tcga_dge$significant_lm %>% 
    map(count, regulation) %>% 
    compress(names_to = 'class') %>% 
    mutate(n_anova = length(tcga_dge$significant_anova), 
           n_total = nrow(tcga_dge$lexicon), 
           perc_transcriptome = n/n_total * 100, 
           perc_plot = ifelse(regulation == 'downregulated', 
                              -perc_transcriptome, perc_transcriptome), 
           class = factor(class))
  
  ## plotting 
  
  tcga_dplots$gene_n$plot <- tcga_dplots$gene_n$data %>% 
    ggplot(aes(x = perc_plot, 
               y = class, 
               fill = regulation)) +
    geom_bar(stat = 'identity', 
             color = 'black') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    scale_fill_manual(values = c(downregulated = 'steelblue', 
                                 upregulated = 'firebrick'), 
                      name = '') + 
    scale_x_continuous(labels = function(x) abs(x)) + 
    scale_y_discrete(limits = rev(levels(tcga_dplots$gene_n$data$class))) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Differential gene regulation, TCGA', 
         subtitle = paste('versus #1, total genes: n =', 
                          tcga_dplots$gene_n$data$n_total[1]), 
         x = '% of detected genes')
  
# Volcano plots --------
  
  insert_msg('Volcano plots')
  
  tcga_dplots$volcano <- 
    list(data = tcga_dge$cleared_lm, 
         plot_title = levels(tcga_dge$analysis_tbl$class)[-1] %>% 
           paste(levels(tcga_dge$analysis_tbl$class)[1], sep = ' vs ')) %>% 
    pmap(plot_volcano, 
         regulation_variable = 'estimate', 
         p_variable = 'p_adjusted', 
         signif_level = 0.05, 
         regulation_level = log(1.5), 
         x_lab = expression('log'[2] * ' fold-regulation, vs #1'), 
         y_lab = expression('-log'[10] * ' pFDR'), 
         top_significant = 20, 
         label_variable = 'gene_symbol', 
         label_type = 'text', 
         txt_size = 2.3, 
         txt_face = 'italic', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(subtitle = .x$labels$tag) + 
          theme(plot.tag = element_blank()))
  
# Top genes: Forest plots --------
  
  insert_msg('Top genes, Forest plots')
  
  tcga_dplots$top_genes <- 
    list(data = tcga_dge$significant_lm, 
         plot_title = levels(tcga_dge$analysis_tbl$class)[-1] %>% 
           paste(levels(tcga_dge$analysis_tbl$class)[1], sep = ' vs ')) %>% 
    pmap(plot_top, 
         regulation_variable = 'estimate', 
         label_variable = 'gene_symbol', 
         p_variable = 'p_adjusted', 
         lower_ci_variable = 'lower_ci', 
         upper_ci_variable = 'upper_ci', 
         top_regulated = 20, 
         plot_subtitle = 'Top regulated vs #1', 
         x_lab = expression('log'[2] * ' fold-regulation, vs #1'), 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          theme(axis.text.y = element_text(face = 'italic')) + 
          scale_x_continuous(limits = c(-10, 15), 
                             breaks = seq(-10, 15, by = 2.5)))
  
# Heat map ------
  
  insert_msg('Heat map of the top significant genes')
  
  ## genes regulated between the subsets, top 500
  
  tcga_dplots$heat_map$variables <- tcga_dge$significant_lm %>% 
    map(top_n, 500, abs(estimate)) %>% 
    map(~.x$gene_symbol) %>% 
    reduce(union)
  
  ## normalized gene expression levels
  
  tcga_dplots$heat_map$data <- 
    tcga_dge$analysis_tbl[c('ID', tcga_dplots$heat_map$variables)] %>% 
    column_to_rownames('ID') %>% 
    center_data('mean') %>% 
    rownames_to_column('ID') %>% 
    left_join(tcga_dge$analysis_tbl[c('ID', 'class')], 
              by = 'ID') %>% 
    relocate(class) %>% 
    relocate(ID) %>% 
    as_tibble
  
  ## PAM clustering of the genes
  
  tcga_dplots$heat_map$cluster <- 
    tcga_dplots$heat_map$data[c('ID', tcga_dplots$heat_map$variables)] %>% 
    column_to_rownames('ID') %>% 
    t %>% 
    as.data.frame %>% 
    kcluster(k = 3, 
             clust_fun = 'pam', 
             variant = 'faster')
  
  tcga_dplots$heat_map$order <- 
    tcga_dplots$heat_map$cluster$clust_assignment %>% 
    arrange(clust_id) %>% 
    set_names(c('variable', 'clust_id'))

  ## heat map, cluster assignment information
  
  tcga_dplots$heat_map$plot <- 
    draw_class_hm(data = tcga_dplots$heat_map$data, 
                  variables = tcga_dplots$heat_map$order$variable, 
                  plot_title = 'Top genes differentiating between subsets, TCGA',
                  limits = c(-3, 3), 
                  oob = scales::squish, 
                  name = 'Z-score') + 
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  tcga_dplots$heat_map$plot$data <- tcga_dplots$heat_map$plot$data %>% 
    left_join(tcga_dplots$heat_map$order, by = 'variable')
  
  tcga_dplots$heat_map$plot <- tcga_dplots$heat_map$plot + 
    facet_grid(clust_id ~ class, 
               space = 'free', 
               scales = 'free') + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())
  
# END ------
  
  insert_tail()