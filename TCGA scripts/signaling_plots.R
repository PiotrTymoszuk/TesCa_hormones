# Signaling analysis results in plots

  insert_head()
  
# container -------
  
  tcga_splots <- list()

# Volcano plots -------
  
  insert_msg('Volcano plots')
  
  ## plot titles
  
  tcga_splots$plot_titles <- tcga_spia$test %>% 
    names %>% 
    map(stri_split, fixed = '.') %>% 
    unlist(recursive = FALSE) %>% 
    map_dfr(~tibble(level = .x[[2]], 
                    reference = .x[[1]])) %>% 
    mutate(reference = car::recode(reference, 
                                   "'SEM' = 'SEM1';  
                                   'NSGCT' = 'NS PRL'"), 
           plot_title = paste(level, reference, sep = ' vs ')) %>% 
    .$plot_title
  
  
  ## plots
  
  tcga_splots$volcano <- 
    list(data = tcga_spia$test, 
         plot_title = tcga_splots$plot_titles) %>% 
    pmap(plot_volcano, 
         regulation_variable = 'tA', 
         p_variable = 'pGFdr', 
         regulation_level = 0, 
         x_lab = 'Fold-regulation vs tA', 
         y_lab = expression('-log'[10] * ' pFDR'), 
         top_significant = 10, 
         label_variable = 'Name', 
         label_type = 'text', 
         txt_size = 2.3, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(subtitle = .x$labels$tag) + 
          geom_vline(xintercept = 0, 
                     linetype = 'dashed'))
  
# Bubble plot with regulated pathways ------
  
  insert_msg('Bubble plot with the regulation estimates')
  
  ## all significant regulated pathways
  
  tcga_splots$bubble$variables <- tcga_spia$test %>% 
    map(filter, pGFdr < 0.05, tA != 0) %>% 
    map(~.x$Name) %>% 
    reduce(union)
  
  ## regulation estimates
  
  tcga_splots$bubble$data <- 
    list(SEM = tcga_spia$test[c("SEM.NS PRL", "SEM.NS E2", 
                                "SEM.NS T", "SEM.SEM2")], 
         NSGCT = tcga_spia$test[c("NSGCT.NS E2", "NSGCT.NS T", 
                                  "NSGCT.SEM1", "NSGCT.SEM2")]) %>% 
    map(~map(.x, 
             filter, 
             Name %in% tcga_splots$bubble$variables) %>% 
          map(~.x[c('Name', 'tA', 'pGFdr')]) %>% 
          compress(names_to = 'class') %>% 
          mutate(class = stri_replace(class, 
                                      regex = '^.*\\.', 
                                      replacement = ''), 
                 class = factor(class, levels(tcga_mix$assignment$class)), 
                 regulation = ifelse(pGFdr >= 0.05, 
                                     'ns', 
                                     ifelse(tA > 0, 
                                            'activated', 
                                            ifelse(tA < 0, 'inhibited', 'ns'))), 
                 regulation = factor(regulation, 
                                     c('activated', 'inhibited', 'ns')), 
                 fontface = ifelse(regulation != 'ns', 'bold', 'plain'), 
                 significance = ifelse(regulation == 'ns', 'ns', 'significant')))

  ## plotting 
  
  tcga_splots$bubble$plots <- 
    map2(tcga_splots$bubble$data, 
         paste('Signaling modulation, vs', 
               c('SEM1', 'NS PRL')), 
         ~.x %>% 
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
           labs(title = .y))

# END ------
  
  insert_tail()