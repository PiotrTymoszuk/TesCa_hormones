# Correspondence analysis for sex hormone strata

  insert_head()
  
# container ------
  
  hor_corresp <- list()
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## analysis data, renaming for nice plot labels
  
  hor_corresp$analysis_tbl <- hor_globals$factor_data %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')
  
  names(hor_corresp$analysis_tbl) <- 
    names(hor_corresp$analysis_tbl) %>% 
    exchange(dict = tesca$lexicon) %>% 
    stri_replace(regex = '\\s{1}strata$', 
                 replacement = '')
  
  ## analysis formula
  
  hor_corresp$formula <- hor_globals$factor_variables %>% 
    paste(collapse = ' + ') %>% 
    paste('~', .) %>% 
    as.formula
  
# Correspondence analysis object -------
  
  insert_msg('Correspondence analysis object')
  
  ## wrapping it into a red_analysis object to have
  ## an access the nice plotting interface
  
  hor_corresp$corr_obj <- mca(hor_corresp$analysis_tbl,
                              nf = 2)
  
  hor_corresp$corr_obj <- 
    red_analysis(list(red_obj = hor_corresp$corr_obj, 
                      red_fun = 'mca', 
                      component_tbl = hor_corresp$corr_obj$rs %>% 
                        as.data.frame %>% 
                        set_names(c('comp_1', 'comp_2')) %>% 
                        rownames_to_column('observation') %>% 
                        as_tibble, 
                      loadings = hor_corresp$corr_obj$cs  %>% 
                        as.data.frame %>%
                        set_names(c('comp_1', 'comp_2')) %>% 
                        rownames_to_column('variable') %>% 
                        as_tibble, 
                      data = quo(hor_corresp$analysis_tbl)))
  
# Plots of the row and column factors ------
  
  insert_msg('Plots')
  
  hor_corresp$plots[c('row', 'column')] <- 
    list(type = c('score', 'loadings'), 
         jitter_width = c(0.001, 0), 
         jitter_height = c(0.001, 0)) %>% 
    pmap(plot, 
         x = hor_corresp$corr_obj, 
         cust_theme = globals$common_theme) %>% 
    map2(., c('Correspondence analysis: row factors', 
              'Correspondence analysis: column factors'), 
         ~.x + 
           labs(title = .y, 
                x = .x$labels$x %>% 
                  stri_replace(fixed = 'PC', 
                               replacement = 'Dimension '), 
                y = .x$labels$y %>% 
                  stri_replace(fixed = 'PC', 
                               replacement = 'Dimension '))) %>% 
    map(~.x + 
          labs(subtitle = paste('Clinical strata of blood hormones, n = ', 
                                nrow(hor_corresp$analysis_tbl))))
  
# END -------
  
  insert_tail()