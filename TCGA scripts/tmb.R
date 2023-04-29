# Comparison of total mutation burden (TMB) expressed as the number of mutated 
# genes per genome identified by the Mutect2 algorithm between the hormonal 
# subsets.

  insert_head()

# container ------
  
  tcga_tmb <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  tcga_tmb$analysis_tbl <- 
    inner_join(tcga_mix$assignment, 
               tcga$mutect_tmb, 
               by = 'ID') %>% 
    map_dfc(unname)

# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  tcga_tmb$normality <- tcga_tmb$analysis_tbl %>% 
    explore(variables = 'mut_count', 
            split_factor = 'class', 
            what = 'normality', 
            pub_styled = TRUE) %>% 
    compress(names_to = 'class')
  
  tcga_tmb$stats <- tcga_tmb$analysis_tbl %>% 
    explore(variables = 'mut_count', 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(tcga_tmb$analysis_tbl$class)))
  
# Testing -------
  
  insert_msg('Testing')
  
  ## Kruskal-Wallis test, the TMB is not normally distributed
  
  tcga_tmb$test <- tcga_tmb$analysis_tbl %>% 
    compare_variables(variables = 'mut_count', 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting -------
  
  insert_msg('Plotting')
  
  tcga_tmb$plot <- 
    plot_variable(tcga_tmb$analysis_tbl, 
                  variable = 'mut_count', 
                  split_factor = 'class', 
                  type = 'box', 
                  cust_theme = globals$common_theme, 
                  y_lab = '# mutated genes', 
                  plot_title = 'Total mutation burden, TCGA', 
                  plot_subtitle = tcga_tmb$test$plot_cap, 
                  x_n_labs = TRUE) + 
    scale_fill_manual(values = tcga_globals$clust_colors)
  
# END -------
  
  insert_tail()