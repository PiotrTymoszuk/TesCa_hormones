# Demographic and clinical characterstic of the clusters

# Characteristic of the latent classes: hormone strata and hormone levels
  
  insert_head()
  
# container -------
  
  class_bcg <- list()
  
# analysis globals ----
  
  insert_msg('Analysis globals')
  
  class_bcg$assignment <- lca$assingment
  
  ## analysis tables
  
  class_bcg$analysis_tbl <- 
    left_join(class_bcg$assignment, 
              tesca$data, 
              by = 'ID') %>% 
    map_dfc(unname) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x)
  
  ## demographic and clinical variables
  
  class_bcg$variables <- 
    names(tesca$data)[!names(tesca$data) %in% c('ID', 
                                                'birth_date', 
                                                'surgery_date', 
                                                'relapse', 
                                                'fup_date', 
                                                'relapse_date', 
                                                'chorion_ca_percent', 
                                                'relapse', 
                                                'rfs_days', 
                                                'relapse_factor')]
  
  class_bcg$variables <- 
    class_bcg$variables[!class_bcg$variables %in% c(hor_globals$numeric_variables, 
                                                    hor_globals$factor_variables)]
  
  ## variable lexicon
  
  class_bcg$lexicon <- tesca$lexicon %>% 
    filter(variable %in% class_bcg$variables) %>% 
    mutate(test_type = ifelse(format == 'factor', 
                              'cramer_v', 'kruskal_eta'), 
           plot_type = ifelse(format == 'factor', 
                              'stack', 'violin'), 
           axis_label = ifelse(format == 'factor', 
                               '% of subset', axis_label))
  
  ## numeric and factor variables
  
  class_bcg[c('factor_variables', 
              'numeric_variables')] <- class_bcg$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  class_bcg$stats <- class_bcg$analysis_tbl %>% 
    explore(variables = class_bcg$lexicon$variable, 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(class_bcg$assignment$class)))
  
# Testing -----
  
  insert_msg('Testing')
  
  class_bcg$test <- class_bcg$analysis_tbl %>% 
    compare_variables(variables = class_bcg$lexicon$variable, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = class_bcg$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Significant differences -----
  
  insert_msg('Significant differences')
  
  class_bcg$significant <- class_bcg$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Plots -----
  
  insert_msg('Plots')
  
  class_bcg$plots <- 
    list(variable = class_bcg$lexicon$variable, 
         plot_title = class_bcg$lexicon$label, 
         plot_subtitle = class_bcg$test$plot_cap, 
         y_lab = class_bcg$lexicon$axis_label, 
         type = class_bcg$lexicon$plot_type) %>% 
    pmap(plot_variable, 
         class_bcg$analysis_tbl, 
         split_factor = 'class', 
         x_lab = 'Hormonal subset', 
         cust_theme = globals$common_theme, 
         scale = 'percent', 
         x_n_labs = TRUE) %>% 
    set_names(class_bcg$lexicon$variable)
  
  ## adjustment of the numeric and factor variables
  
  class_bcg$plots[class_bcg$factor_variables] <- 
    class_bcg$plots[class_bcg$factor_variables] %>% 
    map(~.x + scale_fill_brewer())

  class_bcg$plots[class_bcg$numeric_variables] <- 
    class_bcg$plots[class_bcg$numeric_variables] %>% 
    map(~.x + 
          scale_fill_manual(values = hor_globals$class_colors) + 
          theme(legend.position = 'none'))
  
# Result table --------
  
  insert_msg('Result table')
  
  class_bcg$result_tbl <- 
    left_join(class_bcg$stats, 
              class_bcg$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    left_join(class_bcg$lexicon[c('variable', 'class')], 
              by = 'variable') %>% 
    format_tbl(value = 'table_label')
  
# END -------
  
  insert_tail()