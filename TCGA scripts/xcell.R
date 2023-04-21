# Differences in xCell infiltration estimates between the hormonal cancer 
# subsets of the TCGA cohort

  insert_head()
  
# container -----
  
  tcga_xcell <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  tcga_xcell$analysis_tbl <- 
    left_join(tcga_mix$assignment, 
              tcga$xcell, 
              by = 'ID') %>% 
    map_dfc(unname)

  tcga_xcell$lexicon <- tcga$xcell_lexicon
  
  ## normalized, median centered values for the plot panels
  
  tcga_xcell$norm_table <- tcga$xcell %>% 
    column_to_rownames('ID') %>% 
    center_data('median') %>% 
    rownames_to_column('ID') %>% 
    left_join(tcga_mix$assignment, ., by = 'ID') %>% 
    map_dfc(unname)
  
  ## n numbers
  
  tcga_xcell$strata_n <- tcga_xcell$analysis_tbl %>% 
    count(class)
  
  tcga_xcell$strata_labs <- 
    map2_chr(tcga_xcell$strata_n[[1]], 
             tcga_xcell$strata_n[[2]], 
             paste, 
             sep = '\nn = ')
  
# Descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  tcga_xcell$stats <- tcga_xcell$analysis_tbl %>% 
    explore(split_factor = 'class', 
            variables = tcga_xcell$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(tcga_xcell$analysis_tbl$class)))
  
# Testing for differences: Kruskal-Wallis test ----
  
  insert_msg('Testing for differences')
  
  tcga_xcell$test <- tcga_xcell$analysis_tbl %>% 
    compare_variables(variables = tcga_xcell$lexicon$variable, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           ax_lab = paste(exchange(variable, dict = tcga_xcell$lexicon), 
                          plot_cap, 
                          sep = '\n'))
  
# significant differences -----
  
  insert_msg('Significant differences')
  
  tcga_xcell$significant <- tcga_xcell$test %>% 
    filter(p_adjusted < 0.05, 
           variable != 'uncharacterized.cell') %>% 
    .$variable
  
# Plots -------
  
  insert_msg('Plots')
  
  tcga_xcell$plots <- 
    list(variable = tcga_xcell$test$variable, 
         plot_title = tcga_xcell$test$variable %>% 
           exchange(dict = tcga_xcell$lexicon), 
         plot_subtitle = tcga_xcell$test$plot_cap) %>% 
    pmap(plot_variable, 
         tcga_xcell$analysis_tbl, 
         split_factor = 'class', 
         type = 'box', 
         cust_theme = globals$common_theme, 
         y_lab = 'fraction of tumor cells, xCell', 
         x_n_labs = TRUE, 
         point_hjitter = 0) %>% 
    map(~.x + 
          scale_fill_manual(values = tcga_globals$clust_colors)) %>% 
    set_names(tcga_xcell$test$variable)
  
# Panel plot, significant differences ------
  
  insert_msg('Plot panel, significant differences')
  
  tcga_xcell$panel_plot <- 
    draw_violin_panel(data = tcga_xcell$norm_table, 
                      variables = tcga_xcell$significant, 
                      split_factor = 'class', 
                      distr_geom = 'violin', 
                      non_zero = FALSE, 
                      point_size = 1.5, 
                      point_wjitter = 0, 
                      plot_title = 'Immune infiltration, xCell', 
                      cust_theme = globals$common_theme, 
                      x_lab = 'Z score, quanTIseq', 
                      scale = 'width', 
                      dodge_w = 0.9) + 
    scale_fill_manual(values = tcga_globals$clust_colors, 
                      labels = tcga_xcell$strata_labs, 
                      name = '') + 
    scale_y_discrete(labels = function(x) exchange(x, 
                                                   dict = tcga_xcell$test, 
                                                   key = 'variable', 
                                                   value = 'ax_lab')) + 
    facet_grid(variable ~ ., 
               scales = 'free', 
               space = 'free') + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())

# Result table ------
  
  insert_msg('Result table')
  
  tcga_xcell$result_tbl <- 
    left_join(tcga_xcell$stats, 
              tcga_xcell$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(lexicon = tcga_xcell$lexicon, 
               rm_complete = TRUE) %>% 
    full_rbind(tibble(variable = 'Samples, n', 
                      `#1` = tcga_xcell$strata_n$n[1], 
                      `#2` = tcga_xcell$strata_n$n[2], 
                      `#3` = tcga_xcell$strata_n$n[3], 
                      `#4` = tcga_xcell$strata_n$n[4], 
                      `#5` = tcga_xcell$strata_n$n[5]), .)
  
# END -----
  
  insert_tail()