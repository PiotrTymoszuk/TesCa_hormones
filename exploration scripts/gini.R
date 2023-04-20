# Ginic coefficients for the study variables

  insert_head()
  
# container ------
  
  gini <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')

  gini$analysis_vec <- 
    map2(list(tesca = tesca$data, 
              tcga = tcga$expression), 
         list(ex_globals$variables, 
              tcga$genes), 
         ~select(.x, all_of(.y))) %>% 
    map(~map(.x, ~.x[!is.na(.x)]) %>% 
          map(as.numeric))

  ## numbers of complete cases
  
  gini$n_complete <- gini$analysis_vec %>% 
    map(~map_dbl(.x, length)) %>% 
    map(compress,
        names_to = 'variable', 
        values_to = 'n_complete')

# Gini coefficients -----
  
  insert_msg('Gini coefficients and numbers of complete observations')
  
  gini$stats <- gini$analysis_vec %>% 
    map(~map_dbl(.x, Gini)) %>% 
    map(compress, 
        names_to = 'variable', 
        values_to = 'gini') %>% 
    map2(., gini$n_complete, 
         left_join, 
         by = 'variable')

  gini$plots <- gini$stats %>% 
    map(~.x %>% 
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
               x = 'Gini coefficient'))
  
# END --------
  
  insert_tail()