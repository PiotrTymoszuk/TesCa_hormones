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
  
  ## analysis variables and analyis table: no transformation is applied
  ## except for normalization with median-centering
  
  pca$variables <- c('E2', 'T_total', 'PRL', 'FSH', 'LH', 'HCG')
  
  ## analysis table
  
  pca$analysis_tbl <- tesca$data %>% 
    select(ID, all_of(pca$variables)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
# PCA: 4-dimensions ------
  
  insert_msg('PCA object')
  
  pca$pca_obj <- pca$analysis_tbl %>% 
    reduce_data(kdim = 6, 
                red_fun = 'pca')
  
# PCA: plots ------
  
  insert_msg('PCA plots')
  
  pca$pca_plots[c('scree', 'scores', 'loadings')] <- 
    c('scree', 'scores', 'loadings') %>% 
    map(~plot(pca$pca_obj, 
              type = .x, 
              cust_theme = globals$common_theme))
  
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