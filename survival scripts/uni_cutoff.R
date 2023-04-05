# Optimal cutoffs of maximal survival differences, numeric variables
#
# Since this is a non-parametric analysis, explanatory variables 
# are not transformed

  insert_head()
  
# Container ------
  
  uni_cut <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  uni_cut$variables <- surv_globals$variables$numeric %>% 
    stri_replace(regex = '(_sqrt|_log)$', replacement = '')
  
  ## analysis tables
  
  uni_cut$analysis_tbl <- uni_cut$variables %>% 
    map(~select(tesca$data, relapse, rfs_days, .data[[.x]])) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    set_names(uni_cut$variables)

# Cut objects ------
  
  insert_msg('Cut objects')
  
  uni_cut$survcut_obj <- 
    list(data = uni_cut$analysis_tbl, 
         variable = uni_cut$variables, 
         min_n = 0.2 * map_dbl(uni_cut$analysis_tbl, nrow)) %>% 
    future_pmap(find_cutoff, 
                time = 'rfs_days', 
                event = 'relapse', 
                .options = furrr_options(seed = TRUE))
  
# Cutoff stats -------
  
  insert_msg('Cutoff stats')
  
  uni_cut$stats <- uni_cut$survcut_obj %>% 
    map(summary) %>% 
    map(~.x[1, ]) %>% 
    compress(names_to = 'variable') %>% 
    adjust_fdr('p_value', 'BH') %>% 
    mutate(unit = exchange(variable, 
                           dict = tesca$lexicon, 
                           value = 'unit'), 
           cutoff_lab = paste(signif(cutoff, 2), unit), 
           cutoff_lab = paste('optimal cutoff:', cutoff_lab))
  
# Diagnostic and KM Plots -------
  
  insert_msg('Diagnostic and Kaplan-Meier plots')
  
  ## diagnostic
  
  uni_cut$diagnostic_plots <- uni_cut$survcut_obj %>% 
    map(plot, 
        type = 'diagnostic', 
        ggtheme = globals$common_theme)
  
  ## strata n numbers
  
  uni_cut$n_strata <- 
    map2(uni_cut$stats$n_low, 
         uni_cut$stats$n_high, 
         ~c(paste('low\nn =', .x), 
            paste('high\nn =', .y)))
  
  ## Kaplan-Meier
  
  uni_cut$km_plots <- 
    list(x = uni_cut$survcut_obj, 
         title = exchange(uni_cut$variables, 
                          dict = tesca$lexicon)) %>% 
    pmap(plot, 
         type = 'km', 
         ggtheme = globals$common_theme) %>% 
    map(~.x$plot)
  
  uni_cut$km_plots <- 
    list(x = uni_cut$km_plots, 
         y = uni_cut$n_strata, 
         z = uni_cut$stats$significance, 
         v = uni_cut$stats$cutoff_lab) %>% 
    pmap(function(x, y, z, v) x + 
           scale_color_manual(values = c(low = 'steelblue', 
                                         high = 'firebrick'), 
                              labels = y) + 
           globals$common_theme + 
           theme(legend.position = 'right', 
                 plot.tag = element_blank()) + 
           annotate('text', 
                    label = z, 
                    size = 2.75, 
                    x = 0.1, 
                    y = 0.1, 
                    hjust = 0) + 
           labs(subtitle = v))

# END -------
  
  plan('sequential')
  
  insert_tail()