# Differences between marker-negative and marker-positive patients
# in the neutral and pituitary class
#
# Justification for the separate analysis/comparison of differences
# according to the marker status within the neutral and pituitary subset:
#
# 1) Nearly all testicle subset participants are positive for AFP or HCG
# 2) Modeling with the hormonal subset and marker status as explanatory 
# variables would be feasible (e.g. ANOVA)
# but require extensive transformation of numeric response variables
# 3) Construction of traditional regression models would be complicated 
# for categorical response variables with multiple levels, where some of 
# categories are rare (e.g staging) -> risk of over-fitting and lack 
# of convergence

  insert_head()
  
# container -------

  class_mark <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')

# analysis globals ----

  insert_msg('Analysis globals')
  
  class_mark$assignment <- lca$assingment
  
  ## analysis tables: one for the neutral and one for the pituitary subset
  
  class_mark$analysis_tbl <- 
    left_join(class_mark$assignment, 
              tesca$data, 
              by = 'ID') %>% 
    filter(class != 'testicle', 
           !is.na(marker_status)) %>% 
    map_dfc(unname) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x) %>% 
    blast(class)
    
  
  ## demographic and clinical variables
  
  class_mark$variables <- 
    names(tesca$data)[!names(tesca$data) %in% c('ID', 
                                                'birth_date', 
                                                'surgery_date', 
                                                'relapse', 
                                                'fup_date', 
                                                'relapse_date', 
                                                'chorion_ca_percent', 
                                                'relapse', 
                                                'rfs_days', 
                                                'relapse_factor', 
                                                'marker_status', 
                                                'AFP', 
                                                'AFP_class', 
                                                'HCG', 
                                                'HCG_class')]
  
  class_mark$variables <- 
    class_mark$variables[!class_mark$variables %in% c(hor_globals$numeric_variables)]
  
  ## variable lexicon
  
  class_mark$lexicon <- tesca$lexicon %>% 
    filter(variable %in% class_mark$variables) %>% 
    mutate(test_type = ifelse(format == 'factor', 
                              'cramer_v', 'wilcoxon_r'), 
           plot_type = ifelse(format == 'factor', 
                              'stack', 'violin'), 
           axis_label = ifelse(format == 'factor', 
                               '% of subset', axis_label))
  
  ## numeric and factor variables
  
  class_mark[c('factor_variables', 
              'numeric_variables')] <- class_mark$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  class_mark$stats <- class_mark$analysis_tbl %>% 
    future_map(explore, 
               variables = class_mark$lexicon$variable, 
               split_factor = 'marker_status', 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, 
        left_join, 
        by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(class_mark$analysis_tbl[[1]]$marker_status)))
  
# Testing for differences ------
  
  insert_msg('Testing')
  
  class_mark$test <- class_mark$analysis_tbl %>% 
    future_map(~compare_variables(.x, 
                                  variables = class_mark$lexicon$variable, 
                                  split_factor = 'marker_status', 
                                  what = 'eff_size', 
                                  types = class_mark$lexicon$test_type, 
                                  exact = FALSE, 
                                  ci = FALSE, 
                                  adj_method = 'BH', 
                                  pub_styled = TRUE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Significant differences between the marker status ----
  
  insert_msg('Significant differences')
  
  class_mark$significant <- class_mark$test %>% 
    map(filter, 
        p_adjusted < 0.05, 
        !stri_detect(eff_size, fixed = 'Inf')) %>% 
    map(~.x$variable)
  
# Plots -------
  
  insert_msg('Plots')
  
  for(i in names(class_mark$analysis_tbl)) {
    
    class_mark$plots[[i]] <- 
      list(variable = class_mark$lexicon$variable,  
           plot_title = paste(class_mark$lexicon$label, 
                              c(neutral = 'neutral', 
                                pituitary  = 'pituitary')[i], 
                              sep = ', '), 
           plot_subtitle = class_mark$test[[i]]$plot_cap, 
           y_lab = class_mark$lexicon$axis_label, 
           type = class_mark$lexicon$plot_type) %>% 
      pmap(plot_variable, 
           class_mark$analysis_tbl[[i]], 
           split_factor = 'marker_status', 
           scale = 'percent', 
           cust_theme = globals$common_theme, 
           x_lab = 'Marker status', 
           x_n_labs = TRUE) %>% 
      map(~.x + 
            scale_fill_brewer(palette = 'Purples')) %>% 
      set_names(class_mark$lexicon$variable)
    
  }
  
# Result table -------
  
  insert_msg('Result table with the significant effects')
  
  class_mark$result_table <- 
    map2(class_mark$stats, 
         class_mark$test %>% 
           map(filter, 
               p_adjusted < 0.05, 
               !stri_detect(eff_size, fixed = 'Inf')) %>% 
           map(select, variable, significance, eff_size), 
         right_join, by = 'variable') %>% 
    map(format_tbl, 
        value = 'table_label')
  
# END -----
  
  rm(i)
  
  plan('sequential')
  
  insert_tail()