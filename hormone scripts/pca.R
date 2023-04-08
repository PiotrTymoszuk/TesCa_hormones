# Principal component analysis for blood concentrations of sex hormones
# and investigation of clustering tendency

  insert_head()
  
# container -------
  
  pca <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')

  plan('multisession')
    
# analysis globals --------
  
  insert_msg('Analysis globals')

  ## analysis table, renaming for nice plot labels
  
  pca$analysis_tbl <- hor_globals$numeric_data %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
  names(pca$analysis_tbl) <- names(pca$analysis_tbl) %>% 
    exchange(dict = tesca$lexicon)
  
# PCA: 4-dimensions ------
  
  insert_msg('PCA object')
  
  pca$pca_obj <- pca$analysis_tbl %>% 
    reduce_data(kdim = 4, 
                red_fun = 'pca')
  
# PCA: plots ------
  
  insert_msg('PCA plots')
  
  pca$pca_plots[c('scree', 'scores', 'loadings')] <- 
    c('scree', 'scores', 'loadings') %>% 
    map(~plot(pca$pca_obj, 
              type = .x, 
              cust_theme = globals$common_theme)) %>% 
    map(~.x + 
          labs(subtitle = paste('Numeric hormone levels, n =', 
                             nrow(pca$analysis_tbl))))
  
# UMAP ------
  
  insert_msg('UMAP')
  
  pca$umap_obj <- pca$analysis_tbl %>% 
    reduce_data(distance_method = 'cosine', 
                kdim = 2, 
                red_fun = 'umap', 
                reandom_state = 1234)
  
  pca$umap_plot <- pca$umap_obj %>% 
    plot(cust_theme = globals$common_theme)
  
# clustering tendency -------
  
  insert_msg('Clustering tendency')
  
  set.seed(1234)
  
  pca$clust_tendency <- 
    list(data = pca$analysis_tbl, 
         pca = pca$pca_obj$component_tbl %>% 
           column_to_rownames('observation'), 
         umap = pca$umap_obj$component_tbl %>% 
           column_to_rownames('observation')) %>% 
    future_map(get_clust_tendency, 
               n = 0.5 * nrow(pca$analysis_tbl), 
               .options = furrr_options(seed = TRUE))
  
# END ------
  
  plan('sequential')
  
  insert_tail()