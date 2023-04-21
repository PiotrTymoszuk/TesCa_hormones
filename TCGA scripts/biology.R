# Comparing ssGSVA scores for the Reactome pathways 
# between the hormonal subsets
# Done with one-way ANOVA and lm. ssGSVA score are usually nicele normally 
# distributed

  insert_head()
  
# container -----
  
  tcga_biology <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')

  tcga_biology$analysis_tbl <- 
    left_join(tcga_mix$assignment, 
              tcga$reactome, 
              by = 'ID')
  
  tcga_biology$lexicon <- tcga$reactome_lexicon %>% 
    filter(variable %in% names(tcga_biology$analysis_tbl))
  
  ## labeller
  
  tcga_biology$reactome_wrapper <- function(x, ...) {
    
    x %>% 
      exchange(dict = tcga_biology$lexicon) %>% 
      stri_wrap(simplify = FALSE, ...) %>% 
      map_chr(paste, collapse = '\n')
    
  }
  
# GSVA -----
  
  insert_msg('GSVA')
  
  tcga_biology$test <- 
    test_anova(data = tcga_biology$analysis_tbl, 
               split_fct = 'class', 
               variables = tcga_biology$lexicon$variable, 
               adj_method = 'BH', 
               .parallel = TRUE)
  
# Significant factors ------
  
  insert_msg('Significant factors')
  
  ## singificance in ANOVA
  
  tcga_biology$significant_anova <- tcga_biology$test$anova %>% 
    filter(p_adjusted < 0.05) %>% 
    .$response
  
  ## significance in linear modeling
  
  tcga_biology$significant_lm <- tcga_biology$test$lm %>% 
    filter(response %in% tcga_biology$significant_anova, 
           level != '(Intercept)') %>% 
    blast(level) %>% 
    map(mutate, 
        resp_label = exchange(response, dict = tcga_biology$lexicon), 
        regulation = ifelse(p_adjusted >= 0.05, 
                            'ns', 
                            ifelse(estimate > 0, 
                                   'upregulated', 'downregulated')), 
        regulation = factor(regulation, 
                            c('upregulated', 'downregulated', 'ns')))
  
# Top regulated pathways -----
  
  insert_msg('Top regulated pathways per subset')
  
  ## >0.3 fold regulation
  
  tcga_biology$top_reactome <- tcga_biology$significant_lm %>% 
    map(filter, regulation != 'ns') %>% 
    map(blast, regulation) %>% 
    map(~map_dfr(.x, filter, abs(estimate) > 0.3)) %>% 
    map(arrange, estimate)
  
# Heat map with the top regulated signatures -----
  
  insert_msg('Heat maps with the top regulated scores')
  
  ## common top regulated Reactome pathways
  
  tcga_biology$heat_map$variables <- 
    tcga_biology$top_reactome %>% 
    map(~.x$response) %>% 
    reduce(union)
  
  ## data 
  
  tcga_biology$heat_map$data <- 
    tcga_biology$analysis_tbl[c('ID', 'class', tcga_biology$heat_map$variables)]
  
  ## clusters of signatures
  
  tcga_biology$heat_map$cluster <- tcga_biology$heat_map$data %>% 
    column_to_rownames('ID') %>% 
    select(-class) %>% 
    t %>% 
    as.data.frame %>% 
    kcluster(k = 3, clust_fun = 'pam', variant = 'faster')
  
  tcga_biology$heat_map$order <- 
    tcga_biology$heat_map$cluster$clust_assignment %>% 
    arrange(clust_id) %>% 
    set_names(c('variable', 'clust_id'))
  
  ## heat map with the signature clustering
  
  tcga_biology$heat_map$plot <- 
    draw_class_hm(data = tcga_biology$heat_map$data, 
                  variables = tcga_biology$heat_map$order$variable, 
                  plot_title = 'Reactome pathways differentiating between subsets, TCGA',
                  limits = c(-1, 1), 
                  oob = scales::squish, 
                  name = expression(Delta * 'ssGSEA')) + 
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  tcga_biology$heat_map$plot$data <- tcga_biology$heat_map$plot$data %>% 
    left_join(tcga_biology$heat_map$order, by = 'variable')
  
  tcga_biology$heat_map$plot <- tcga_biology$heat_map$plot + 
    facet_grid(clust_id ~ class, 
               space = 'free', 
               scales = 'free') + 
    theme(strip.background.y = element_blank(), 
          strip.text.y = element_blank())
  
# Forest plots with the top regulated signatures -----
  
  insert_msg('Forest plots for the top regulated signatures')
  
  tcga_biology$top_forests <- 
    list(data = tcga_biology$top_reactome, 
         plot_title = paste0('Top signatures, subset ', 
                             levels(tcga_biology$analysis_tbl$class)[-1], 
                             ', TCGA')) %>% 
    pmap(plot_top, 
         regulation_variable = 'estimate', 
         label_variable = 'response', 
         p_variable = 'p_adjusted', 
         regulation_level = 0, 
         top_regulated = 10, 
         lower_ci_variable = 'lower_ci', 
         upper_ci_variable = 'upper_ci', 
         plot_subtitle = 'Baseline: subset #1', 
         x_lab = expression(Delta * ' ssGSEA, 95% CI'), 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) tcga_biology$reactome_wrapper(x, 30)) + 
          scale_x_continuous(limits = c(-1.35, 1.35)) + 
          theme(legend.position = 'bottom'))
  
# END -----
  
  insert_tail()