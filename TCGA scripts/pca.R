# PCA to reveal associations between 
# the hormone-related genes
# done for the entire cohort and separately for seminoma and NSCGT
#
# Assessing the clustering tendency of the entire dataset 
# (data, PCA scores and UMAP scores) by Hopkins stat

  insert_head()

# container -----

  tcga_pca <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')

# analysis globals -------

insert_msg('Analysis globals')

  tcga_pca$lexicon <- tcga$gene_lexicon
  
  tcga_pca$analysis_tbl$tcga <- tcga$expression %>% 
    select(ID, histology, all_of(tcga_pca$lexicon$gene_symbol)) %>% 
    filter(complete.cases(.))
  
  tcga_pca$analysis_tbl[c('seminoma', 'NSGCT')] <- 
    tcga_pca$analysis_tbl$tcga %>% 
    blast(histology)
  
  tcga_pca$analysis_tbl <- tcga_pca$analysis_tbl %>% 
    map(select, - histology) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(as.data.frame) %>% 
    map(center_data, 'median')
  
# PCA objects: 8 dimensions ------
  
  insert_msg('8D PCA')
  
  tcga_pca$pca_obj <- tcga_pca$analysis_tbl %>% 
    map(~reduce_data(.x, red_fun = 'pca', kdim = 8))
  
# Plots -------
  
  insert_msg('Plots')
  
  tcga_pca$plots <- c('scree', 'loadings', 'scores') %>% 
    map(function(plot_type) list(x = tcga_pca$pca_obj, 
                                 point_color = c('cornsilk4',
                                                 'lightblue1', 
                                                 'lightblue3')) %>% 
          pmap(plot, 
               type = plot_type, 
               cust_theme = globals$common_theme, 
               txt_type = 'text')) %>% 
    transpose %>% 
    map(set_names, 
        c('scree', 'loadings', 'scores'))
  
# UMAP ------
  
  insert_msg('UMAP')
  
  ## the entire dataset
  
  tcga_pca$umap_obj <-  c('euclidean', 'manhattan', 'cosine') %>% 
    map(~reduce_data(tcga_pca$analysis_tbl$tcga, 
                     distance_method = .x, 
                     red_fun = 'umap', 
                     kdim = 2, 
                     random.state = 1234)) %>% 
    set_names(c('euclidean', 'manhattan', 'cosine'))
  
  tcga_pca$umap_plot <- tcga_pca$umap_obj %>% 
    map(plot, 
        cust_theme = globals$common_theme)
  
# Clustering tendencies ------
  
  insert_msg('Clustering tendency')
  
  tcga_pca$clust_tendency <- 
    list(data = tcga_pca$analysis_tbl$tcga, 
         pca = tcga_pca$pca_obj$tcga$component_tbl, 
         umap_euclidean = tcga_pca$umap_obj$euclidean$component_tbl, 
         umap_manhattan = tcga_pca$umap_obj$manhattan$component_tbl, 
         umap_cosine = tcga_pca$umap_obj$cosine$component_tbl) %>% 
    map(~.x[names(.x) != 'observation']) %>% 
    future_map(get_clust_tendency, 
               n = 70, 
               .options = furrr_options(seed = TRUE))

# END ------
  
  plan('sequential')
  
  insert_tail()