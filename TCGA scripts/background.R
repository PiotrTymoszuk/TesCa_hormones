# Demographic and clinical background of the hormonal classes

  insert_head()
  
# container ------
  
  tcga_bcg <- list()
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  tcga_bcg$lexicon <- tcga$lexicon %>% 
    filter(!variable %in% c('rfs_days', 
                            'relapse', 
                            'death', 
                            'os_days', 
                            'tumor_death', 
                            'dss_days', 
                            'progression', 
                            'pfs_days', 
                            'ID')) %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'kruskal_etasq', 
                              'cramer_v'), 
           plot_type = ifelse(format == 'numeric', 
                              'violin', 'stack'), 
           y_lab = ifelse(format == 'numeric', 
                          axis_label, '% of subset'))
  
  ## analysis table
  
  tcga_bcg$analysis_tbl <- 
    left_join(tcga_mix$assignment, 
              tcga$clinical, by = 'ID') %>% 
    map_dfc(unname) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x)
  
# Descriptive stats ----
  
  insert_msg('Descriptive stats')
  
  tcga_bcg$stats <- tcga_bcg$analysis_tbl %>% 
    explore(variables = tcga_bcg$lexicon$variable, 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, 
           by = 'variable') %>% 
    set_names(c('variable', levels(tcga_bcg$analysis_tbl$class)))
  
# Testing for difference between hormonal classes ------
  
  insert_msg('Testing for differences between the classes')
  
  tcga_bcg$test <- tcga_bcg$analysis_tbl %>% 
    compare_variables(variables = tcga_bcg$lexicon$variable, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = tcga_bcg$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(significance = ifelse(stri_detect(eff_size, fixed = 'Inf'), 
                                 NA, significance), 
           eff_size = ifelse(stri_detect(eff_size, fixed = 'Inf'), 
                             NA, eff_size), 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots for single variables ------
  
  insert_msg('Plotting')
  
  tcga_bcg$plots <- 
    list(variable = tcga_bcg$test$variable, 
         plot_title = tcga_bcg$test$variable %>% 
           exchange(tcga_bcg$lexicon) %>% 
           paste0(', TCGA'), 
         plot_subtitle = tcga_bcg$test$plot_cap, 
         type = tcga_bcg$lexicon$plot_type, 
         y_lab = tcga_bcg$lexicon$y_lab) %>% 
    pmap(plot_variable, 
         tcga_bcg$analysis_tbl, 
         split_factor = 'class', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE, 
         scale = 'percent') %>% 
    set_names(tcga_bcg$test$variable)
  
  ## fill scale for numeric variables
  
  tcga_bcg$variable_type <- 
    tcga_bcg$lexicon %>%
    blast(format) %>% 
    map(~.x$variable)
  
  tcga_bcg$plots[tcga_bcg$variable_type$numeric] <- 
    tcga_bcg$plots[tcga_bcg$variable_type$numeric] %>% 
    map(~.x + 
          scale_fill_manual(values = tcga_globals$clust_colors))
  
  tcga_bcg$plots[tcga_bcg$variable_type$factor] <- 
    tcga_bcg$plots[tcga_bcg$variable_type$factor] %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Green'))
  
# Result table ----
  
  insert_msg('Result table')
  
  tcga_bcg$result_tbl <- 
    left_join(tcga_bcg$stats, 
              tcga_bcg$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(lexicon = tcga_bcg$lexicon, 
               value = 'table_label')
  
# END -----
  
  insert_tail()

    
  
  

  

  

