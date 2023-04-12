# comparison of study variables between patients with seminoma 
# and non-seminoma tumors. Doen with non-parametric tests.

  insert_head()

# container -------

  histology <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  ## variables: histology-related variables are removed
  
  histology$lexicon <- tesca$lexicon %>% 
    filter(variable %in% histo_globals$variables) %>% 
    arrange(class)
  
  histology$variables <- histology$lexicon$variable
  
  ## analysis table
  
  histology$analysis_tbl <- tesca$data %>% 
    select(ID, histology, all_of(histology$variables)) %>% 
    filter(!is.na(histology)) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x)
  
  ## types of tests and plots
  
  histology$lexicon <- histology$lexicon %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 'cramer_v'), 
           plot_type = ifelse(format == 'numeric',
                              'violin', 'stack'), 
           axis_label = ifelse(format == 'numeric', 
                               axis_label, '% of strata'))
  
# Descriptive stats --------
  
  insert_msg('Descriptive stats')
  
  histology$stats <- histology$analysis_tbl %>% 
    explore(variables = histology$lexicon$variable, 
            split_factor = 'histology', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(histology$analysis_tbl$histology)))
  
# Testing -------
  
  insert_msg('Testing')
  
  histology$test <- histology$analysis_tbl %>% 
    compare_variables(variables = histology$lexicon$variable, 
                      split_factor = 'histology', 
                      what = 'eff_size', 
                      types = histology$lexicon$test_type, 
                      ci = FALSE, 
                      exact = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance))
  
# Significant factors ------
  
  histology$significant <- histology$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Plots ------
  
  insert_msg('Plots')
  
  histology$plots <- 
    list(variable = histology$lexicon$variable, 
         plot_title = histology$lexicon$label, 
         plot_subtitle = histology$test$plot_cap, 
         y_lab = histology$lexicon$axis_label, 
         type = histology$lexicon$plot_type) %>% 
    pmap(plot_variable, 
         split_factor = 'histology', 
         histology$analysis_tbl, 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer()) %>%  
    set_names(histology$lexicon$variable)
  
# Result table --------
  
  insert_msg('Result table')
  
  histology$result_tbl <- 
    left_join(histology$stats, 
              histology$test[c('variable', 'significance', 'eff_size')],
              by = 'variable') %>%
    left_join(histology$lexicon[c('variable', 'class')], 
              by = 'variable') %>% 
    format_tbl(value = 'table_label')
   
# END ------
  
  insert_tail()