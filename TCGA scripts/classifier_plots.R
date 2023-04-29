# Visualization of the to differences between the hormonal subsets
# as identified by permutation importance of explanatory variables
# of the Random Forest classifier

  insert_head()
  
# container --------
  
  tcga_rfvar <- list()
  
# Analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## the most relevant variables among signatures, infiltration 
  ## and expression estimates
  
  tcga_rfvar$variables <- tcga_rf$importance$measures %>% 
    mutate(plot_cap = paste('\u0394 accuracy =', 
                            signif(delta_accuracy, 2))) %>% 
    filter(delta_accuracy > 0) %>% 
    blast(var_type) %>% 
    map_dfr(top_n, 20, delta_accuracy) %>% 
    mutate(var_label = ifelse(var_type == 'expression',
                            paste0('<em>', var_label, '</em>'), 
                            var_label))
  
  tcga_rfvar$analysis_tbl <- tcga_rf$analysis_tbl %>% 
    rownames_to_column('ID') %>% 
    select(ID, class, all_of(tcga_rfvar$variables$variable)) %>% 
    mutate(class = stri_replace(class, fixed = '_', replacement = ' '), 
           class = factor(class, levels(tcga_mix$assignment$class))) %>% 
    as_tibble
  
# Single plots --------
  
  insert_msg('Single plots')
  
  tcga_rfvar$plots <- tcga_rfvar$variables %>% 
    select(variable, var_label, plot_cap) %>% 
    set_names(c('variable', 'plot_title', 'plot_subtitle')) %>% 
    pmap(plot_variable, 
         tcga_rfvar$analysis_tbl, 
         split_factor = 'class', 
         type = 'box', 
         cust_theme = globals$common_theme, 
         y_lab = 'Z-score', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_manual(values = tcga_globals$clust_colors) + 
          theme(plot.title = element_markdown())) %>% 
    set_names(tcga_rfvar$variables$variable)
  
# Clustered heat map -----
  
  insert_msg('Clustered heat map')
  
  tcga_rfvar$heat_map <- 
    draw_clustered_hm(tcga_rfvar$analysis_tbl, 
                      variables = tcga_rfvar$variables$variable, 
                      plot_title = 'Top most important variables, Random Forest', 
                      k = 4, 
                      distance = 'cosine', 
                      return_clust = FALSE, 
                      limits = c(-3, 3), 
                      oob = scales::squish, 
                      name = 'Z-score') + 
    scale_y_discrete(labels = function(x) exchange(x, dict = tcga_rfvar$variables, value = 'var_label')) + 
    theme(axis.text.y = element_markdown(size = 6, hjust = 1))
  
# END -----
  
  insert_tail()