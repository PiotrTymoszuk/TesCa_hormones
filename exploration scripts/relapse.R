# Comparison of the study variables in participants with 
# and without relapse
#
# since multiple variables are non-normally distributed, non-parametric tests 
# are used

  insert_head()
  
# container ------
  
  ex_relapse <- list()

# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variable lexicon
  ## categorical variables with single levels are removed
  ## from the analysis
  ## highly missing values like T free are removed as well
  
  ex_relapse$lexicon <- tesca$lexicon %>% 
    filter(variable %in% ex_globals$variables) %>% 
    mutate(plot_type = ifelse(format == 'numeric', 
                              'violin', 'stack'), 
           n_levels = levels %>% 
             stri_split(fixed = ', '), 
           n_levels = map_dbl(n_levels, length), 
           test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 
                              ifelse(n_levels == 1, 
                                     NA, 
                                     'cramer_v')), 
           axis_label = ifelse(format == 'factor', 
                               '% of strata', 
                               axis_label)) %>% 
    filter(!variable %in% c('rfs_days', 'relapse_factor')) %>% 
    filter(!variable %in% c('T_free', 
                            'SHBG', 
                            'chorion_cancer')) %>% 
    filter(!is.na(test_type)) %>% 
    arrange(class)
  
  ## analysis table
  
  ex_relapse$analysis_tbl <- tesca$data %>% 
    select(relapse_factor, all_of(ex_relapse$lexicon$variable)) %>% 
    mutate(relapse_factor = car::recode(relapse_factor, 
                                        "'no' = 'no relapse'; 
                                        'yes' = 'relapse'")) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x)
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  ex_relapse$stats <- ex_relapse$analysis_tbl %>% 
    explore(split_factor = 'relapse_factor', 
            variables = ex_relapse$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'no_relapse', 'relapse'))
  
# Testing for differences -------
  
  insert_msg('Testing for differences')

  ex_relapse$test <- ex_relapse$analysis_tbl %>% 
    compare_variables(variables = ex_relapse$lexicon$variable, 
                      split_factor = 'relapse_factor', 
                      what = 'eff_size', 
                      types = ex_relapse$lexicon$test_type, 
                      pub_styled = TRUE, 
                      ci = FALSE, 
                      exact = FALSE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots ------
  
  insert_msg('Plots')
  
  ex_relapse$plots <- 
    list(variable = ex_relapse$lexicon$variable,
         type = ex_relapse$lexicon$plot_type, 
         plot_title = ex_relapse$lexicon$label, 
         plot_subtitle = ex_relapse$test$plot_cap, 
         y_lab = ex_relapse$lexicon$axis_label) %>% 
    pmap(plot_variable, 
         ex_relapse$analysis_tbl, 
         split_factor = 'relapse_factor', 
         scale = 'percent', 
         point_hjitter = 0, 
         x_n_labs = TRUE, 
         cust_theme = globals$common_theme) %>%
    map(~.x + scale_fill_brewer()) %>%
    set_names(ex_relapse$lexicon$variable)
  
# Result table ------
  
  insert_msg('Result table')
  
  ## descriptive stats and testing reults
  
  ex_relapse$result_table <- 
    left_join(ex_relapse$stats, 
               ex_relapse$test[c('variable', 'significance', 'eff_size')], 
               by = 'variable') %>% 
    format_tbl(value = 'table_label')
  
# END ------
  
  insert_tail()