# Counts of copy number alterations in the hormonal subsets

  insert_head()
  
# container -------
  
  tcga_cnb <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## counting the alterations
  
  tcga_cnb$gistic_mtx <- tcga$gistic %>% 
    select(-sample_id) %>% 
    column_to_rownames('ID') %>% 
    as.matrix
  
  tcga_cnb$gistic_mtx <- tcga_cnb$gistic_mtx != 0
  
  ## analyis table
  
  tcga_cnb$analysis_tbl <- tcga_cnb$gistic_mtx %>% 
    rowSums %>% 
    compress(names_to = 'ID', 
             values_to = 'cna_count')
  
  tcga_cnb$analysis_tbl <- 
    inner_join(tcga_cnb$analysis_tbl, 
               tcga_mix$assignment, 
               by = 'ID') %>% 
    map_dfc(unname)
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  tcga_cnb$normality <- tcga_cnb$analysis_tbl %>% 
    explore(variables = 'cna_count', 
            split_factor = 'class', 
            what = 'normality', 
            pub_styled = TRUE) %>% 
    compress(names_to = 'class')
  
  tcga_cnb$stats <- tcga_cnb$analysis_tbl %>% 
    explore(variables = 'cna_count', 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(tcga_cnb$analysis_tbl$class)))
  
# Testing ------
  
  insert_msg('Testing')
  
  ## Kruskal-Wallis test
  
  tcga_cnb$test <- tcga_cnb$analysis_tbl %>% 
    compare_variables(variables = 'cna_count', 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')
  
  tcga_cnb$plot <- 
    plot_variable(tcga_cnb$analysis_tbl, 
                  variable = 'cna_count', 
                  split_factor = 'class', 
                  type = 'box', 
                  point_hjitter = 0, 
                  cust_theme = globals$common_theme, 
                  y_lab = '# amplified or deleted genes', 
                  plot_title = 'Copy number alterations, TCGA', 
                  plot_subtitle = tcga_cnb$test$plot_cap, 
                  x_n_labs = TRUE) + 
    scale_fill_manual(values = tcga_globals$clust_colors)
  
# END ------
  
  insert_tail()