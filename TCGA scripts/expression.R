# Expression of the genes of interest in the TCGA dataset: in the whole one
# and the collective split by histology types

  insert_head()
  
# container -------
  
  tcga_exp <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  tcga_exp$lexicon <- tcga$gene_lexicon
  
  tcga_exp$analysis_tbl <- tcga$expression %>% 
    select(histology, all_of(tcga_exp$lexicon$gene_symbol))
  
  ## expression maximum: needed for the common scale
  
  tcga_exp$exp_max <- tcga_exp$analysis_tbl[tcga_exp$lexicon$gene_symbol] %>% 
    as.matrix %>% 
    max
  
  tcga_exp$exp_breaks <- seq(0, tcga_exp$exp_max, by = 2.5)
  
# Plotting expression of the genes of interest in the entire cohort -----
  
  insert_msg('Plotting, the entire cohort')
  
  ## violin plot panels, common x axis scale
  
  tcga_exp$entire_plots <- 
    list(variables = tcga_exp$lexicon %>% 
           blast(class) %>% 
           map(~.x$gene_symbol), 
         plot_title = c('Gonadal hormone synthesis genes, TCGA', 
                        'Pituitary hormone genes, TCGA'), 
         fill = tcga_globals$gene_class_colors) %>% 
    pmap(draw_violin_panel, 
         data = tcga_exp$analysis_tbl, 
         distr_geom = 'violin', 
         non_zero = FALSE, 
         cust_theme = globals$common_theme, 
         scale = 'width', 
         x_lab = expression('log'[2] * ' mRNA count'), 
         point_hjitter = 0.1, 
         point_size = 1.5, 
         plot_subtitle = paste('n =', nrow(tcga_exp$analysis_tbl))) %>% 
    map2(., 
         tcga_exp$lexicon %>% 
           blast(class) %>% 
           map(~.x$gene_symbol), 
         ~.x + 
           scale_y_discrete(limits = rev(.y)) + 
           scale_x_continuous(limits = c(0, tcga_exp$exp_max), 
                              breaks = tcga_exp$exp_breaks) + 
           theme(axis.text.y = element_text(face = 'italic')))
  
# Comparison between the histologies ------
  
  insert_msg('Comparison between the histologies')
  
  ## descriptive stats
  
  tcga_exp$stats <- tcga_exp$analysis_tbl %>% 
    filter(!is.na(histology)) %>% 
    explore(split_factor = 'histology', 
            variables = tcga_exp$lexicon$gene_symbol, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, 
           by = 'variable') %>% 
    set_names(c('variable', levels(tcga_exp$analysis_tbl$histology)))
  
  ## testing: working with Mann-Whitney U test and r effect size statistic
  
  tcga_exp$test <- tcga_exp$analysis_tbl %>% 
    compare_variables(variables = tcga_exp$lexicon$gene_symbol, 
                      split_factor = 'histology', 
                      what = 'eff_size', 
                      types = 'wilcoxon_r', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Violin plots ------
  
  insert_msg('Violin plots')
  
  tcga_exp$plots <- 
    list(variable = tcga_exp$test$variable, 
         plot_title = paste0('<b><em>', 
                             tcga_exp$test$variable, 
                             '</em>, TCGA</b>'), 
         plot_subtitle = tcga_exp$test$plot_cap) %>% 
    pmap(plot_variable, 
         tcga_exp$analysis_tbl %>% 
           filter(!is.na(histology)), 
         split_factor = 'histology', 
         cust_theme = globals$common_theme, 
         y_lab = expression('log'[2] * ' mRNA count'), 
         x_n_labs = TRUE, 
         point_hjitter = 0) %>% 
    map(~.x + 
          theme(plot.title = element_markdown()) + 
          scale_fill_brewer()) %>% 
    set_names(tcga_exp$test$variable)
  
# Result table -------
  
  insert_msg('Result table')
  
  tcga_exp$result_n <- tcga_exp$analysis_tbl %>% 
    filter(!is.na(histology)) %>% 
    count(histology)
  
  tcga_exp$result_tbl <- 
    left_join(tcga_exp$stats, 
              tcga_exp$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(lexicon = tcga_exp$lexicon, 
               rm_complete = TRUE,
               key = 'gene_symbol', 
               value = 'gene_symbol') %>% 
    full_rbind(tibble(variable = 'Samples, n', 
                      seminoma = tcga_exp$result_n$n[1], 
                      NSGCT = tcga_exp$result_n$n[2]), .)
  
# END ------
  
  insert_tail()