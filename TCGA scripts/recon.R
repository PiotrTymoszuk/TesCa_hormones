# Comparison ssGSEA score of the Recon subsystem gene signatures between
# the hormonal subsets

  insert_head()
  
# container -----
  
  tcga_recon <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  tcga_recon$analysis_tbl <- 
    inner_join(tcga_mix$assignment, 
               tcga$recon, by = 'ID')
  
  tcga_recon$lexicon <- tcga$recon_lexicon
  
# GSVA -----
  
  insert_msg('GSVA')
  
  ## the tests are done in two flavors: 
  ## with the SEM1 and NS PRL as baselines, respectively
  
  tcga_recon$test <- list(SEM = tcga_recon$analysis_tbl, 
                          NSGCT = tcga_recon$analysis_tbl %>% 
                            mutate(class = new_baseline(class, 
                                                        'NS PRL'))) %>% 
    map(test_anova, 
        split_fct = 'class', 
        variables = tcga_recon$lexicon$variable, 
        adj_method = 'BH', 
        .parallel = FALSE)
  
# Significant factors ------
  
  insert_msg('Significant factors')
  
  ## significance in ANOVA
  
  tcga_recon$significant_anova <- tcga_recon$test[[1]]$anova %>% 
    filter(p_adjusted < 0.05) %>% 
    .$response
  
  ## significance in linear modeling
  
  tcga_recon$cleared_lm <- tcga_recon$test %>% 
    map(~.x$lm) %>% 
    map(filter, 
        response %in% tcga_recon$significant_anova, 
        level != '(Intercept)') %>% 
    map(mutate, 
        resp_label = exchange(response, dict = tcga_recon$lexicon), 
        regulation = ifelse(p_adjusted >= 0.05, 
                            'ns', 
                            ifelse(estimate > 0, 
                                   'upregulated', 'downregulated')), 
        regulation = factor(regulation, 
                            c('upregulated', 'downregulated', 'ns')))
  
  tcga_recon$significant_lm <- tcga_recon$cleared_lm %>% 
    map(filter, regulation != 'ns')
  
# Heat map with the top regulated signatures -----
  
  insert_msg('Heat map with the top regulated scores')
  
  ## common significantly regulated Reactome pathways
  
  tcga_recon$heat_map$variables <- 
    tcga_recon$significant_lm %>% 
    map(~.x$response) %>% 
    map(reduce, union) %>% 
    reduce(union)
  
  ## plotting
  
  tcga_recon$heat_map$plot <- 
    draw_clustered_hm(data = tcga_recon$analysis_tbl, 
                      variables = tcga_recon$heat_map$variables, 
                      plot_title = paste('Metabolis pathways differentiating', 
                                         'between the hormonal subsets'), 
                      k = 3, 
                      name = expression(Delta * 'ssGSEA'), 
                      limits = c(-1, 1), 
                      oob = scales::squish)
# Forest plots with the top regulated signatures -----
  
  insert_msg('Forest plots for the top regulated signatures')
  
  tcga_recon$top_forests <- 
    list(x = tcga_recon$significant_lm %>% 
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
                 scale_y_discrete(labels = function(x) exchange(x, dict = tcga_recon$lexicon)) + 
                 scale_x_continuous(limits = c(-1.35, 1.35)) + 
                 theme(legend.position = 'bottom')))
  
# Similarity of the hormonal subsets -----
  
  insert_msg('Similarity of the hormonal subsets')
  
  ## dimensionality reduction
  
  tcga_recon$similarity$red_obj <- tcga_recon$analysis_tbl %>% 
    column_to_rownames('ID') %>% 
    select(-class) %>% 
    reduce_data(distance_method = 'cosine', 
                kdim = 2, 
                red_fun = 'mds')
  
  ## plotting
  
  set.seed(1234)
  
  tcga_recon$similarity$plots <- 
    plot_similarity(tcga_recon$similarity$red_obj, 
                    class_assignment = tcga_recon$analysis_tbl[c('ID', 'class')], 
                    plot_title = 'Similarity of hormonal subsets, metabolic pathways', 
                    plot_subtitle = '2D MDS, cosine distance', 
                    distance = 'cosine', 
                    min_max_similarity = TRUE, 
                    weighting_order = 1, 
                    net_theme = theme_void() + 
                      theme(plot.margin = globals$common_margin))
  
# END -----
  
  insert_tail()  