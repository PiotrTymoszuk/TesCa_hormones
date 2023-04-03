# Ginic coefficients for the study variables

  insert_head()
  
# container ------
  
  gini <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')

  gini$variables <- ex_globals$variables

  gini$analysis_vec <- tesca$data %>% 
    select(all_of(gini$variables)) %>% 
    map(~.x[!is.na(.x)]) %>% 
    map(as.numeric)
  
  ## numbers of complete cases
  
  gini$n_complete <- gini$analysis_vec %>% 
    map_dbl(length) %>% 
    compress(names_to = 'variable', 
             values_to = 'n_complete')
      
# Gini coefficients -----
  
  insert_msg('Gini coefficients and numbers of complete observations')
  
  gini$stats <- gini$analysis_vec %>% 
    map_dbl(Gini) %>% 
    compress(names_to = 'variable', 
             values_to = 'gini') %>% 
    left_join(gini$n_complete, 
              by = 'variable')
  
  gini$plot <- gini$stats %>%
    ggplot(aes(x = gini, 
               y = reorder(variable, gini))) + 
    geom_bar(stat = 'identity', 
             fill = 'coral3', 
             color = 'black') +
    scale_y_discrete(labels = exchange(gini$variables,
                                       dict = tesca$lexicon)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Information content of variables', 
         subtitle = 'Gini coefficient', 
         x = 'Gini coefficient')
  
# END --------
  
  insert_tail()