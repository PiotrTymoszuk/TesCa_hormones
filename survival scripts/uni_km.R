# Kaplan-Meier analysis: categorical variables and numeric variables split
# by tertiles

  insert_head()
  
# container -------
  
  uni_km <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## models
  
  uni_km$models <- uni_cox$models
  
  ## total n numbers
  
  uni_km$total_n <- uni_cox$models %>% 
    map(~map(.x, summary, 'fit')) %>% 
    map(compress, names_to = 'variable') %>% 
    map(mutate, 
        plot_cap = paste0('total: n = ', 
                          n_complete, 
                          ', events: n = ', 
                          n_events)) %>% 
    map(~.x$plot_cap)

# categorical variables ------
  
  insert_msg('Categorical variables')
  
  ## survfit objects
  
  uni_km$factors$surv_fit_obj <- surv_globals$variables$factor %>% 
    paste('Surv(rfs_days, relapse) ~', .) %>% 
    map(as.formula) %>% 
    map(~survminer::surv_fit(.x, data = surv_globals$data)) %>% 
    set_names(surv_globals$variables$factor)
  
  ## p values for differences between the strata
  
  uni_km$factors$test <- uni_km$factors$surv_fit_obj %>% 
    map_dfr(surv_pvalue) %>% 
    adjust_fdr('pval') %>% 
    as_tibble
  
  ## n numbers
  
  uni_km$factors$n_strata <- uni_km$models$factors %>% 
    map2(., names(.), 
         ~count(model.frame(.x), .data[[.y]])) %>% 
    map(~map2_chr(.x[[1]], .x[[2]], paste, sep = '\nn = '))

  ## Kaplan-Meier plots
  
  uni_km$factors$plots <- 
    list(fit = uni_km$factors$surv_fit_obj, 
         title = exchange(surv_globals$variables$factor, 
                          dict = surv_globals$lexicon), 
         pval = uni_km$factors$test$significance, 
         legend.labs = uni_km$factors$n_strata) %>% 
    pmap(ggsurvplot, 
         pval.size = 2.75, 
         xlab = 'Relapse-free survival, days') %>% 
    map(~.x$plot) %>% 
    map2(., uni_km$total_n$factors, 
         ~.x + 
           scale_color_manual(values = c('steelblue', 
                                         'coral3', 
                                         'coral4')) + 
           labs(subtitle = .y) + 
           globals$common_theme + 
           theme(legend.position = 'right'))
  
# numeric variables -------
  
  insert_msg('Numeric variables')
  
  ## calibrator objects
  
  uni_km$numeric$calibrator_obj <- 
    uni_km$models[c("first_order", "second_order")] %>% 
    map(~map(.x, 
             calibrate, 
             n = 3, 
             labels = c('low', 'int', 'high')))
  
  ## survfit objects for the linear predictor score tertiles
  ## an p values (log-rank test) for differences between the tertiles
  
  uni_km$numeric$test <- uni_km$numeric$calibrator_obj %>% 
    map(~map(.x, ~.x$surv_fit) %>% 
          map(surv_pvalue)) %>% 
    map(compress, names_to = 'variable') %>% 
    map(adjust_fdr, variable = 'pval')
  
  
  ## Kaplan-Meier plots
  
  for(i in names(uni_km$numeric$calibrator_obj)) {
    
    uni_km$numeric$plots[[i]] <- 
      list(x = uni_km$numeric$calibrator_obj[[i]], 
           title = surv_globals$variables$numeric %>% 
             exchange(dict = surv_globals$lexicon) %>% 
             paste(i, sep = ', ')) %>% 
      pmap(plot, 
           palette = c('steelblue', 
                       'coral2', 
                       'coral4'), 
           show_cox = FALSE, 
           cust_theme = globals$common_theme, 
           xlab = 'Relapse-free survival, days')
    
    uni_km$numeric$plots[[i]] <- 
      list(x = uni_km$numeric$plots[[i]], 
           y = uni_km$numeric$test[[i]]$significance, 
           z = uni_km$total_n[[i]]) %>% 
      pmap(function(x, y, z) x + 
             labs(subtitle = z) + 
             theme(legend.position = 'right', 
                   legend.title = element_blank(), 
                   plot.tag = element_blank()) + 
             annotate('text', 
                      label = y, 
                      size = 2.75, 
                      x = 0.1, 
                      y = 0.1, 
                      hjust = 0))
    
  }
  
# END --------
  
  rm(i)
  
  insert_tail()