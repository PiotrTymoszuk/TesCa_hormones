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
  
  tcga_biology$top_reactome <- tcga_biology$significant_lm %>% 
    map(filter, regulation != 'ns') %>% 
    map(blast, regulation) %>% 
    map(~map_dfr(.x, top_n, 20, abs(estimate))) %>% 
    map(arrange, estimate)
  
# Heat maps with the top regulated signatures -----
  
  insert_msg('Heat maps with the top regulated scores')

  tcga_biology$top_heat_maps <- 
    list(variables = tcga_biology$top_reactome %>% 
           map(~.x$response), 
         plot_title = paste0('Top signatures, subset ', 
                             levels(tcga_biology$analysis_tbl$class)[2:4], 
                             ', TCGA')) %>% 
    pmap(draw_class_hm, 
         data = tcga_biology$analysis_tbl, 
         limits = c(-1, 1), 
         oob = scales::squish) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) tcga_biology$reactome_wrapper(x, 50)))
  
# Forest plots with the top regulated signatures -----
  
  insert_msg('Forest plots for the top regulated signatures')
  
  tcga_biology$top_forests <- 
    list(data = tcga_biology$top_reactome, 
         plot_title = paste0('Top signatures, subset ', 
                             levels(tcga_biology$analysis_tbl$class)[2:4], 
                             ', TCGA')) %>% 
    pmap(plot_top, 
         regulation_variable = 'estimate', 
         label_variable = 'response', 
         p_variable = 'p_adjusted', 
         regulation_level = 0, 
         lower_ci_variable = 'lower_ci', 
         upper_ci_variable = 'upper_ci', 
         plot_subtitle = 'Baseline: subset #1', 
         x_lab = expression(Delta * ' ssGSEA, 95% CI'), 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) tcga_biology$reactome_wrapper(x, 50)) + 
          theme(legend.position = 'bottom'))
  
# END -----
  
  insert_tail()