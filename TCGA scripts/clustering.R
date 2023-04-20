# Development of a stable clustering for 
# the TCGA dataset of the hormone-related genes

  insert_head()

# container -----

  tcga_clustdev <- list()
  
# parallel backend -----
  
  plan('multisession')

# analysis globals -------

  insert_msg('Analysis globals')
  
  tcga_clustdev$lexicon <- tcga$gene_lexicon %>% 
    filter(gene_symbol %in% tcga_globals$clust_genes)
  
  tcga_clustdev$analysis_tbl <- tcga$expression %>% 
    select(ID, all_of(tcga_clustdev$lexicon$gene_symbol)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
  tcga_clustdev$distances <- c('euclidean', 'manhattan', 'cosine')
  
  tcga_clustdev$distances <- tcga_clustdev$distances %>% 
    set_names(tcga_clustdev$distances)
  
# algorithms -----
  
  insert_msg('Algorithms')
  
  ## Hierarchical clustering
  
  tcga_clustdev$algos[paste0('HCL_', tcga_clustdev$distances)] <- 
    list(distance_method = tcga_clustdev$distances, 
         k = 3) %>% 
    pmap(hcluster, 
         data = tcga_clustdev$analysis_tbl)
  
  ## K-means and PAM
  
  tcga_clustdev$algos[paste0('KMEANS_', tcga_clustdev$distances)] <- 
    list(distance_method = tcga_clustdev$distances, 
         k = 3) %>% 
    pmap(kcluster, 
         data = tcga_clustdev$analysis_tbl, 
         clust_fun = 'kmeans')
  
  tcga_clustdev$algos[paste0('PAM_', tcga_clustdev$distances)] <- 
    list(distance_method = tcga_clustdev$distances, 
         k = 3) %>% 
    pmap(kcluster, 
         data = tcga_clustdev$analysis_tbl, 
         clust_fun = 'pam')

# Clustering variance ------
  
  insert_msg('Clustering variance')
  
  tcga_clustdev$variance <- tcga_clustdev$algos %>% 
    map(var) %>% 
    map_dbl(~.x$frac_var) %>% 
    compress(names_to = 'algorithm', 
             values_to = 'variance')
  
# Cross-validation -------
  
  insert_msg('Cross-validation')
  
  tcga_clustdev$cv <- tcga_clustdev$algos %>% 
    future_map(cv, 
               nfolds = 10, 
               simple_vote = FALSE, 
               resolve_ties = TRUE, 
               .parallel = FALSE, 
               .options = furrr_options(seed = TRUE))
  
  tcga_clustdev$cv <- tcga_clustdev$cv %>% 
    map(~.x$summary) %>%
    compress(names_to = 'algorithm')
  
# Common table with the cluster structure stats ------
  
  insert_msg('Common table with the clustering structure stats')
  
  tcga_clustdev$stats <- 
    left_join(tcga_clustdev$variance, 
              tcga_clustdev$cv, 
              by = 'algorithm') %>% 
    mutate(accuracy = 1 - mean_error)

# Plotting the stats -----
  
  insert_msg('Plotting the stats')
  
  tcga_clustdev$plot <- tcga_clustdev$stats %>% 
    pivot_longer(cols = c('variance', 'accuracy'), 
                 names_to = 'stat', 
                 values_to = 'value') %>% 
    mutate(algorithm = stri_replace(algorithm, 
                                    fixed = '_', 
                                    replacement = ', ')) %>% 
    ggplot(aes(x = value, 
               y = reorder(algorithm, value), 
               fill = stat)) + 
    geom_bar(stat = 'identity',
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(accuracy = 'steelblue', 
                                 variance = 'darkolivegreen4'), 
                      labels = c(accuracy = 'accuracy, 10-fold CV', 
                                 variance = 'frac. explained variance')) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Clustering algoritms', 
         subtitle = paste('TCGA, n =', 
                          nrow(tcga_clustdev$analysis_tbl)), 
         x = 'Statistic value')
  
# END -----
  
  plan('sequential')
  
  insert_tail()