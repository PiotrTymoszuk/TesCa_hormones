# Comparison of included and excluded registry patients

  insert_head()
  
# container -------
  
  excl <- list()
  
# parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## IDs of included and excluded patients
  
  excl$participants[c('included', 'excluded')] <- 
    list(tesca$data$ID, 
         tesca$cleared$ID[!tesca$cleared$ID %in% tesca$data$ID])
  
  ## analysis table with an index for the analysis inclusion status
  
  excl$analysis_tbl <- tesca$cleared %>% 
    mutate(analysis = ifelse(ID %in% excl$participants$included, 
                             'included', 'excluded'), 
           analysis = factor(analysis, c('included', 'excluded'))) %>% 
    column_to_rownames('ID') %>% 
    select(analysis, all_of(ex_globals$variables))
  
  ## analysis variables and their lexicon
  
  excl$variables <- ex_globals$variables
  
  excl$lexicon <- tesca$lexicon %>% 
    filter(variable %in% excl$variables) %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 'cramer_v'), 
           plot_type = ifelse(format == 'numeric', 
                              'violin', 'stack'), 
           axis_label = ifelse(format == 'numeric', 
                               axis_label, '% of strata'))
  
# comparing the data missingness -------
  
  insert_msg('Data missingness')
  
  excl$missing$mtx <- excl$analysis_tbl %>% 
    blast(analysis) %>% 
    map(select, -analysis) %>% 
    map(~map_dfc(.x, is.na)) %>% 
    map(as.matrix) %>% 
    map2(., excl$participants, set_rownames)
  
  ## counts and frequencies of missing observations per participant
  ## and per variable
  
  excl$missing$counts_participant <- excl$missing$mtx %>% 
    map(rowSums) %>% 
    map(compress, 
        names_to = 'ID', 
        values_to = 'n_missing') %>% 
    map2(., excl$mi$mtx, 
         ~mutate(.x, 
                 perc_missing = n_missing/ncol(.y) * 100)) %>% 
    compress(names_to = 'analysis')
  
  excl$missing$counts_variable <- excl$missing$mtx %>% 
    map(colSums) %>% 
    map(compress, 
        names_to = 'ID', 
        values_to = 'n_missing') %>% 
    map2(., excl$mi$mtx, 
         ~mutate(.x, 
                 perc_missing = n_missing/nrow(.y) * 100)) %>% 
    compress(names_to = 'analysis')
  
  excl$missing[c('counts_participant', 
                 'counts_variable')] <- excl$missing[c('counts_participant', 
                                                       'counts_variable')] %>% 
    map(mutate, 
        analysis = factor(analysis, c('included', 'excluded')))
  
  ## plotting: histograms
  
  excl$missing[c('plot_participant', 
                 'plot_variable')] <- 
    list(x = excl$missing[c('counts_participant', 
                            'counts_variable')], 
         y = c('fraction of participants', 
               'fraction of variables'), 
         z = c('Missing data per participant', 
               'Missing data per variable')) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = perc_missing, 
                      color = analysis)) + 
           geom_density() + 
           scale_color_manual(values = c(included = 'steelblue', 
                                         excluded = 'firebrick'), 
                              name = 'Analysis\nstatus') + 
           globals$common_theme + 
           labs(title = z, 
                x = '% missing data', 
                y = y))
  
# Comparison of the included and excluded participants -------
  
  insert_msg('Analysis status: comparison')
  
  ## descriptive stats
  
  excl$comparison$stats <- excl$analysis_tbl %>% 
    explore(variables = excl$lexicon$variable, 
            split_factor = 'analysis', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(excl$analysis_tbl$analysis)))
  
  ## testing for differences
  
  excl$comparison$test <- 
    list(variables = excl$lexicon$variable, 
         types = excl$lexicon$test_type) %>% 
    future_pmap(safely(compare_variables), 
                excl$analysis_tbl, 
                split_factor = 'analysis', 
                what = 'eff_size', 
                exact = FALSE, 
                ci = FALSE, 
                pub_styled = TRUE, 
                .options = furrr_options(seed = TRUE)) %>% 
    map_dfr(~.x$result)
  
# Significant differences --------
  
  insert_msg('Significant differences')
  
  excl$significant <- excl$comparison$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# A table with significant differences ------
  
  insert_msg('A table with significant differences')
  
  excl$result_tbl <- 
    right_join(excl$comparison$stats, 
               excl$comparison$test[c('variable', 'significance', 'eff_size')], 
               by = 'variable') %>% 
    filter(variable %in% excl$significant) %>% 
    format_tbl(value = 'table_label')
  
# END ------
  
  plan('sequential')
  
  insert_tail()