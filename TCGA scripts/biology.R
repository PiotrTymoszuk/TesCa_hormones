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
  
  ## the tests are done in two flavors: 
  ## with the SEM1 and NS PRL as baselines, respectively
  
  tcga_biology$test <- list(SEM = tcga_biology$analysis_tbl, 
                            NSGCT = tcga_biology$analysis_tbl %>% 
                              mutate(class = new_baseline(class, 
                                                          'NS PRL'))) %>% 
    map(test_anova, 
        split_fct = 'class', 
        variables = tcga_biology$lexicon$variable, 
        adj_method = 'BH', 
        .parallel = TRUE)

# Significant factors ------
  
  insert_msg('Significant factors')
  
  ## significance in ANOVA
  
  tcga_biology$significant_anova <- tcga_biology$test[[1]]$anova %>% 
    filter(p_adjusted < 0.05) %>% 
    .$response

  ## significance in linear modeling
  
  tcga_biology$cleared_lm <- tcga_biology$test %>% 
    map(~.x$lm) %>% 
    map(filter, 
        response %in% tcga_biology$significant_anova, 
        level != '(Intercept)') %>% 
    map(mutate, 
        resp_label = exchange(response, dict = tcga_biology$lexicon), 
        regulation = ifelse(p_adjusted >= 0.05, 
                            'ns', 
                            ifelse(estimate > 0, 
                                   'upregulated', 'downregulated')), 
        regulation = factor(regulation, 
                            c('upregulated', 'downregulated', 'ns')))
  
  tcga_biology$significant_lm <- tcga_biology$cleared_lm %>% 
    map(filter, regulation != 'ns')

# Heat map with the top regulated signatures -----
  
  insert_msg('Heat map with the top regulated scores')
  
  ## common significantly regulated Reactome pathways
  
  tcga_biology$heat_map$variables <- 
    tcga_biology$significant_lm %>% 
    map(~.x$response) %>% 
    map(reduce, union) %>% 
    reduce(union)
  
  ## plotting
  
  tcga_biology$heat_map$plot <- 
    draw_clustered_hm(data = tcga_biology$analysis_tbl, 
                      variables = tcga_biology$heat_map$variables, 
                      plot_title = paste('Reactome pathways differentiating', 
                                         'between the hormonal subsets'), 
                      k = 3, 
                      name = expression(Delta * 'ssGSEA'), 
                      limits = c(-1, 1), 
                      oob = scales::squish)
  
# Forest plots with the top regulated signatures -----
  
  insert_msg('Forest plots for the top regulated signatures')
  
  tcga_biology$top_forests <- 
    list(x = tcga_biology$significant_lm %>% 
           map(blast, level), 
         y = c('SEM1', 'NS PRL')) %>% 
    pmap(function(x, y)  list(data = x, 
                              plot_title = paste0('Subset ', names(x), 
                                                  ' vs ', y)) %>% 
           pmap(plot_top, 
                regulation_variable = 'estimate', 
                label_variable = 'response', 
                p_variable = 'p_adjusted', 
                regulation_level = 0, 
                top_regulated = 10, 
                lower_ci_variable = 'lower_ci', 
                upper_ci_variable = 'upper_ci', 
                x_lab = expression(Delta * ' ssGSEA, 95% CI'), 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 scale_y_discrete(labels = function(x) tcga_biology$reactome_wrapper(x, 30)) + 
                 scale_x_continuous(limits = c(-1.35, 1.35)) + 
                 theme(legend.position = 'bottom')))
  
# Similarity of the hormonal subsets -----
  
  insert_msg('Similarity of the hormonal subsets')
  
  ## dimensionality reduction
  
  tcga_biology$similarity$red_obj <- tcga_biology$analysis_tbl %>% 
    column_to_rownames('ID') %>% 
    select(-class) %>% 
    reduce_data(distance_method = 'cosine', 
                kdim = 2, 
                red_fun = 'mds')
  
  ## plotting
  
  set.seed(1234)
  
  tcga_biology$similarity$plots <- 
    plot_similarity(tcga_biology$similarity$red_obj, 
                    class_assignment = tcga_biology$analysis_tbl[c('ID', 'class')], 
                    plot_title = 'Similarity of hormonal subsets, Reactome pathways', 
                    plot_subtitle = '2D MDS, cosine distance', 
                    distance = 'cosine', 
                    min_max_similarity = TRUE, 
                    weighting_order = 1, 
                    net_theme = theme_void() + 
                      theme(plot.margin = globals$common_margin))
  
# END -----
  
  insert_tail()