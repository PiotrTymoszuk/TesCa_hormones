# Plots for the results of differential gene expression

  insert_head()
  
# container ------
  
  tcga_dplots <- list()
  
# Numbers of differentially regulated genes ------
  
  insert_msg('Numbers of differentially regulated genes')
  
  ## plotting data
  
  tcga_dplots$gene_n$data <- tcga_dge$significant_lm %>% 
    map(blast, level) %>% 
    map(~map(.x, count, regulation) %>% 
          compress(names_to = 'class') %>% 
          mutate(n_anova = length(tcga_dge$significant_anova), 
                 n_total = nrow(tcga_dge$lexicon), 
                 perc_transcriptome = n/n_total * 100, 
                 perc_plot = ifelse(regulation == 'downregulated', 
                                    -perc_transcriptome, perc_transcriptome), 
                 class = factor(class)))

  ## plotting 
  
  tcga_dplots$gene_n$plots <- 
    map2(tcga_dplots$gene_n$data, 
         c('Regulation vs SEM1', 
           'Regulation vs NS PRL'), 
         ~.x %>% 
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
           labs(title = .y, 
                subtitle = paste('total genes: n =', 
                                 tcga_dplots$gene_n$data[[1]]$n_total[1]), 
                x = '% of detected genes'))

# Volcano plots --------
  
  insert_msg('Volcano plots')
  
  tcga_dplots$volcano <- 
    list(x = tcga_dge$cleared_lm %>% 
           map(blast, level), 
         y = c('SEM1', 'NS PRL')) %>% 
    pmap(function(x, y) list(data = x %>% 
                               map(filter, 
                                   !gene_symbol %in% tcga_globals$clust_genes), 
                             plot_title = paste(names(x), y, sep = ' vs ')) %>% 
           pmap(plot_volcano, 
                regulation_variable = 'estimate', 
                p_variable = 'p_adjusted', 
                signif_level = 0.05, 
                regulation_level = log(1.5), 
                x_lab = expression('log'[2] * ' fold-regulation'), 
                y_lab = expression('-log'[10] * ' pFDR'), 
                top_significant = 20, 
                label_variable = 'gene_symbol', 
                label_type = 'text', 
                txt_size = 2.3, 
                txt_face = 'italic', 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 labs(subtitle = .x$labels$tag) + 
                 theme(plot.tag = element_blank())))
  

      
  
# Top genes: Forest plots --------
  
  insert_msg('Top genes, Forest plots')
  
  tcga_dplots$top_genes <- 
    list(x = tcga_dge$significant_lm %>% 
           map(blast, level), 
         y = c('SEM1', 'NS PRL')) %>% 
    pmap(function(x, y) list(data = x %>% 
                               map(filter, 
                                   !gene_symbol %in% tcga_globals$clust_genes), 
                             plot_title = paste(names(x), y, sep = ' vs ')) %>% 
           pmap(plot_top, 
                regulation_variable = 'estimate', 
                label_variable = 'gene_symbol', 
                p_variable = 'p_adjusted', 
                lower_ci_variable = 'lower_ci', 
                upper_ci_variable = 'upper_ci', 
                top_regulated = 20, 
                x_lab = expression('log'[2] * ' fold-regulation'), 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 theme(axis.text.y = element_text(face = 'italic')) + 
                 scale_x_continuous(limits = c(-10, 15), 
                                    breaks = seq(-10, 15, by = 2.5))))

  
# Heat map ------
  
  insert_msg('Heat map of the top significant genes')
  
  ## genes regulated between the subsets, top 500
  
  tcga_dplots$heat_map$variables <- tcga_dge$significant_lm %>% 
    map(blast, level) %>% 
    map(~map(.x, 
             top_n, 500, abs(estimate)) %>% 
          map(~.x$gene_symbol) %>% 
          reduce(union)) %>% 
    reduce(union)
  
  ## plotting data 
  
  tcga_dplots$heat_map$data <- 
    tcga_dge$analysis_tbl[c('ID', 'class', tcga_dplots$heat_map$variables)]
  
  tcga_dplots$heat_map$data[tcga_dplots$heat_map$variables] <- 
    tcga_dplots$heat_map$data[tcga_dplots$heat_map$variables] %>% 
    map_dfc(~scale(.x)[, 1])

  ## plotting
  
  tcga_dplots$heat_map$plot <- 
    draw_clustered_hm(data = tcga_dplots$heat_map$data, 
                      variables = tcga_dplots$heat_map$variables, 
                      plot_title = paste('Transcriptome differentiating', 
                                         'between the hormonal subsets'), 
                      x_lab = 'Sample', 
                      k = 3, 
                      limits = c(-3, 3), 
                      name = 'Expression Z score', 
                      oob = scales::squish)
  
# END ------
  
  insert_tail()