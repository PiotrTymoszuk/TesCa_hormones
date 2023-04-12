# Descriptive stats and normality of numeric explanatory variables

  insert_head()
  
# container -----
  
  distr <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## numeric variables
  
  distr$variables <- tesca$lexicon %>% 
    filter(format == 'numeric', 
           variable %in% ex_globals$variables, 
           !variable %in% c('relapse', 'rfs_days')) %>% 
    .$variable
  
  ## analysis tables: variables with sqrt(x) and log(x + 1) transformations
  
  distr$analysis_tbl <- tesca$data %>% 
    select(all_of(distr$variables))
  
  for(i in distr$variables){
    
    distr$analysis_tbl <- distr$analysis_tbl %>% 
      mutate(!!paste0(i, '_sqrt') := sqrt(.data[[i]]), 
             !!paste0(i, '_log') := sqrt(.data[[i]]))
    
  }
  
  ## variable lexicon
  
  distr$lexicon <- 
    tibble(variable = names(distr$analysis_tbl)) %>% 
    mutate(transformation = stri_extract(variable, 
                                         regex = 'log|sqrt'), 
           source_variable = stri_replace(variable, 
                                          regex = '_log|_sqrt', 
                                          replacement = ''), 
           label = exchange(source_variable, 
                            dict = tesca$lexicon), 
           label = ifelse(is.na(transformation), 
                          label, 
                          paste(transformation, label)))
  
# Mean and variance -------  
  
  insert_msg('Mean and variance')
  
  distr$mean_var$stats <- distr$analysis_tbl %>% 
    map(~tibble(mean = mean(.x, na.rm = TRUE), 
                variance = var(.x, na.rm = TRUE))) %>% 
    compress(names_to = 'variable') %>% 
    mutate(mvr = mean/variance)
  
  ## plot of the mean variance ratio
  
  distr$mean_var$plot <- distr$mean_var$stats %>% 
    ggplot(aes(x = mean, 
               y = variance, 
               fill = log(mvr))) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                linetype = 'dashed') + 
    geom_point(shape = 21, 
               alpha = 0.9, 
               size = 2) + 
    geom_text_repel(aes(label = exchange(variable, 
                                         dict = distr$lexicon)), 
                    size = 2.5, 
                    color = 'gray60') +
    scale_fill_gradient2(low = 'steelblue', 
                         mid = 'white', 
                         high = 'firebrick', 
                         name = 'log Mean:variance') + 
    scale_x_continuous(trans = 'log') + 
    scale_y_continuous(trans = 'log') + 
    globals$common_theme + 
    labs(title = 'Mean and variances, numeric variables', 
         x = 'Mean', 
         y = 'Variance')
  
# Normality -------
  
  insert_msg('Normality')
  
  ## Shapiro-Wilk test
  
  distr$normality$stats <- distr$analysis_tbl %>% 
    explore(what = 'normality', 
            pub_styled = FALSE) %>% 
    select(variable, stat, p_value, n) %>% 
    set_names(c('variable', 'w', 'p_value', 'n_complete')) %>% 
    left_join(distr$lexicon, by = 'variable') %>% 
    mutate(transformation = ifelse(is.na(transformation), 
                                   'identity', transformation))
  
  ## plot: W statistics
  
  distr$normality$plot <- distr$normality$stats %>% 
    ggplot(aes(x = w, 
               y = reorder(label, w), 
               fill = transformation)) + 
    geom_bar(stat = 'identity',
             color = 'black') +
    geom_vline(xintercept = 0.90, 
               linetype = 'dashed', 
               color = 'orangered3', 
               linewidth = 1) + 
    scale_fill_manual(values = c(identity = 'darkolivegreen4', 
                                 log = 'steelblue3', 
                                 sqrt = 'gray60')) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Normality of numeric variables', 
         subtitle = 'Shapiro-Wilk test', 
         x = 'W, test statistic')
  
  ## QQ plots
  
  distr$normality$qq_plots <- distr$analysis_tbl %>% 
    explore(what = 'plots', 
            pub_styled = FALSE, 
            type = 'qq', 
            cust_theme = globals$common_theme)
  
  distr$normality$qq_plots <- 
    list(x = distr$normality$qq_plots, 
         y = exchange(names(distr$normality$qq_plots), 
                      dict = distr$lexicon, 
                      value = 'label')) %>% 
    pmap(function(x, y) x + 
           labs(title = y, 
                subtitle = x$labels$tag %>% 
                  stri_replace_all(fixed = '\n', 
                                   replacement = ', ')) + 
           theme(plot.tag = element_blank()))
  
# Optimal transformations -------
  
  insert_msg('Determining optimal transformations')
  
  distr$best_trans <- distr$normality$stats %>% 
    group_by(source_variable) %>% 
    filter(w == max(w)) %>% 
    ungroup
  
# END -------
  
  rm(i)
  
  insert_tail()