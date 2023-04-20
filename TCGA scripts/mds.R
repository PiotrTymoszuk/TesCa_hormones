# Multi-dimensional scaling to reveal associations between 
# the hormone-related genes
# done for the entire cohort and separately for seminoma and NSCGT

  insert_head()
  
# container -----
  
  tcga_mds <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  tcga_mds$lexicon <- tcga$gene_lexicon
  
  tcga_mds$analysis_tbl$tcga <- tcga$expression %>% 
    select(ID, histology, all_of(tcga_mds$lexicon$gene_symbol)) %>% 
    filter(complete.cases(.))
  
  tcga_mds$analysis_tbl[c('seminoma', 'NSGCT')] <- 
    tcga_mds$analysis_tbl$tcga %>% 
    blast(histology)
  
  tcga_mds$analysis_tbl <- tcga_mds$analysis_tbl %>% 
    map(select, - histology) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(t) %>%
    map(as.data.frame) %>% 
    map(center_data, 'median')
  
# distance matrices -------
  
  insert_msg('Spearman distance matrices')
 
  tcga_mds$dists <- tcga_mds$analysis_tbl %>% 
    map(as.matrix) %>% 
    map(t) %>% 
    map(cor, method = 'spearman') %>% 
    map(~(1 - .x)/2) %>% 
    map(as.dist, diag = TRUE)
  
# MDS ------
  
  insert_msg('MDS objects')
  
  tcga_mds$mds_coords <- tcga_mds$dists %>% 
    map(cmdscale, k = 2) %>% 
    map(set_colnames, c('comp_1', 'comp_2')) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'observation')
  
  tcga_mds$mds_obj <- 
    list(data = map(tcga_mds$analysis_tbl, quo), 
         component_tbl = tcga_mds$mds_coords) %>%
    pmap(function(data, component_tbl) list(data = data, 
                                            red_obj = NULL, 
                                            red_fun = 'mds', 
                                            component_tbl = component_tbl, 
                                            loadings = NULL)) %>% 
    map(red_analysis)

# Plots ------
  
  insert_msg('Plots')
  
  tcga_mds$plots <- 
    list(x = tcga_mds$mds_obj, 
         point_color = c('cornsilk4', 
                         'lightblue1', 
                         'lightblue3'), 
         plot_subtitle = paste('n =', 
                               map_dbl(tcga_mds$analysis_tbl, 
                                       ncol))) %>% 
    pmap(plot,
         cust_theme = globals$common_theme) %>% 
    map2(., 
         paste('2D MDS,', 
               c('TCGA entire cohort', 
                 'TCGA seminoma', 
                 'TCGA NSGCT')), 
         ~.x + 
           labs(title = .y) + 
           theme(plot.tag = element_blank()) + 
           geom_text_repel(aes(label = observation), 
                           size = 2.5, 
                           fontface = 'italic'))
  
# END ------
  
  insert_tail()