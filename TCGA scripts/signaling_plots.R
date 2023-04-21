# Signaling analysis results in plots

  insert_head()
  
# container -------
  
  tcga_splots <- list()

# Volcano plots -------
  
  insert_msg('Volcano plots')
  
  tcga_splots$volcano <- 
    list(data = tcga_spia$test, 
         plot_title = names(tcga_spia$test) %>% 
           paste0(' vs #1')) %>% 
    pmap(plot_volcano, 
         regulation_variable = 'tA', 
         p_variable = 'pGFdr', 
         regulation_level = 0, 
         x_lab = 'Fold-regulation vs #1, tA', 
         y_lab = expression('-log'[10] * ' pFDR'), 
         top_significant = 10, 
         label_variable = 'Name', 
         label_type = 'text', 
         txt_size = 2.3, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(subtitle = .x$labels$tag) + 
          geom_vline(xintercept = 0, 
                     linetype = 'dashed') + 
          scale_x_continuous(limits = c(-140, 240)))
  
# Bubble plot with regulated pathways ------
  
  insert_msg('Bubble plot with the regulation estimates')
  
  ## all significant regulated pathways
  
  tcga_splots$bubble$variables <- tcga_spia$test %>% 
    map(filter, pGFdr < 0.05) %>% 
    map(~.x$Name) %>% 
    reduce(union)
  
  ## regulation estimates
  
  tcga_splots$bubble$data <- tcga_spia$test %>% 
    map(filter, Name %in% tcga_splots$bubble$variables) %>% 
    map(~.x[c('Name', 'tA', 'pGFdr')]) %>% 
    compress(names_to = 'class') %>% 
    mutate(class = factor(class, 
                          c('#1', names(tcga_spia$test))), 
           regulation = ifelse(pGFdr >= 0.05, 
                               'ns', 
                               ifelse(tA > 0, 
                                      'activated', 'inhibited')), 
           regulation = factor(regulation, c('activated', 'inhibited', 'ns')), 
           fontface = ifelse(regulation != 'ns', 'bold', 'plain'), 
           significance = ifelse(regulation == 'ns', 'ns', 'significant'))
  
  ## plotting 
  
  tcga_splots$bubble$plot <- tcga_splots$bubble$data %>% 
    ggplot(aes(x = class, 
               y = reorder(Name, tA), 
               color = regulation, 
               size = abs(tA))) + 
    geom_point(shape = 16) + 
    geom_text(aes(label = signif(tA, 2), 
                  fontface = fontface, 
                  alpha = significance), 
              size = 2.3, 
              hjust = -1.4, 
              vjust = 0, 
              color = 'black', 
              show.legend = FALSE) +
    scale_color_manual(values = c(activated = 'firebrick', 
                                  inhibited = 'steelblue', 
                                  ns = 'gray70')) + 
    scale_size_area(max_size = 4) + 
    scale_alpha_manual(values = c(ns = 0.15, 
                                  significant = 1)) + 
    guides(alpha = 'none') + 
    globals$common_theme + 
    theme(axis.title = element_blank()) +
    labs(title = 'Regulation of signaling between hormonal subsets', 
         subtitle = 'SPIA, baseline: subset #1')
  
# END ------
  
  insert_tail()