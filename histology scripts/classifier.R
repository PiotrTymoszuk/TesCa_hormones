# Multi-parameter signature of histologies 
# developed by conditional Random Forests

  insert_head()
  
# container ---------
  
  histo_class <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables and analysis table

  histo_class$variables <- c('age_surgery', 
                             'surgery_type', 
                             'pt_stage', 
                             'residual_tumor', 
                             'max_size_cm', 
                             'infiltration_rete_testis', 
                             'lymphovas_invasion', 
                             'cs_lugano', 
                             'LH', 
                             'LH_class', 
                             'FSH', 
                             'FSH_class', 
                             'PRL', 
                             'PRL_class', 
                             'T_total', 
                             'T_total_class', 
                             'E2', 
                             'E2_class', 
                             'HCG', 
                             'HCG_class', 
                             'AFP', 
                             'AFP_class', 
                             'LDH',
                             'LDH_class', 
                             'chemotherapy', 
                             'radiotherapy', 
                             'RLA', 
                             'testosterone_replacement')
  
  histo_class$analysis_tbl <- tesca$data %>% 
    select(ID, histology, all_of(histo_class$variables)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')
  
  ## observation lexicon
  
  histo_class$obs_lexicon <- 
    tibble(ID = rownames(histo_class$analysis_tbl), 
           .observation = 1:nrow(histo_class$analysis_tbl))
  
  ## model formula
  
  histo_class$formula <- 
    paste('histology ~', 
          paste(histo_class$variables, collapse = ' + ')) %>% 
    as.formula
  
  ## n numbers
  
  histo_class$n_numbers <- histo_class$analysis_tbl %>% 
    count(histology)
  
  histo_class$n_numbers <- 
    map2_chr(histo_class$n_numbers[[1]], 
             histo_class$n_numbers[[2]], 
             paste, sep =': n = ') %>% 
    paste(collapse = ', ')
  
# tuning of the model -------
  
  insert_msg('Tuning of the model')
  
  registerDoParallel(cores = 7)
  
  set.seed(1234)
  
  histo_class$model <- 
    caret::train(form = histo_class$formula, 
                 data = histo_class$analysis_tbl, 
                 method = 'cforest', 
                 metric = 'Kappa', 
                 trControl = trainControl(method = 'cv', 
                                          number = 10, 
                                          savePredictions = 'final', 
                                          returnData = TRUE, 
                                          returnResamp = 'final', 
                                          classProbs = TRUE), 
                 tuneGrid = data.frame(mtry = 2:(length(histo_class$variables) - 3)), 
                 controls = cforest_classical()) %>% 
    as_caretx
  
  stopImplicitCluster()
  
# Fit stats -------
  
  insert_msg('Fit stats')
  
  histo_class$stats <- histo_class$model %>% 
    summary %>% 
    map(select, statistic, estimate) %>% 
    map(column_to_rownames, 'statistic') %>% 
    map(t) %>% 
    map(as.data.frame) %>% 
    compress(names_to = 'dataset') %>% 
    as_tibble
  
# Model predictions -----
  
  insert_msg('Model predictions')
  
  histo_class$predictions <- histo_class$model %>% 
    predict %>% 
    compact
  
# Squared errors and Brier scores -------
  
  insert_msg('Squared errors and brier scores')
  
  histo_class$sq_errors <- histo_class$predictions %>% 
    map(~.x$data) %>% 
    map(transmute, 
        .observation = .observation, 
        .outcome = as.numeric(.outcome) - 1, 
        .fitted = NSGCT, 
        sq_error = (.fitted - .outcome)^2)
  
  ## integrated Brier scores
  
  histo_class$stats <- histo_class$stats %>% 
    mutate(bs = histo_class$sq_errors %>% 
             map_dbl(~mean(.x$sq_error)))

# Plots of the Brier scores -------
  
  insert_msg('Brier score plots')
  
  ## common caption
  
  histo_class$brier_caption <- 
    paste0('IBS, training: ', 
           signif(histo_class$stats$bs[1], 2), 
           ', IBS CV: ', 
           signif(histo_class$stats$bs[2], 2), 
           ', n = ', nrow(histo_class$analysis_tbl))
  
  ## observation number and BS
  
  histo_class$brier_plots$obs_score <- 
    histo_class$sq_errors %>% 
    compress(names_to = 'dataset') %>% 
    ggplot(aes(x = .observation, 
               y = sq_error, 
               color = dataset)) +
    geom_path() + 
    scale_color_manual(values = c(train = 'steelblue', 
                                  cv = 'coral3'), 
                       labels = c(train = 'data', 
                                  cv = '10-fold CV'), 
                       name = '') + 
    globals$common_theme + 
    labs(title = 'Squared error per observation', 
         subtitle = histo_class$brier_caption, 
         x = 'Observation number', 
         y = 'Squared error')
  
  ## sorted BS and observation number
  
  histo_class$brier_plots$sorted_score <- 
    histo_class$sq_errors %>% 
    compress(names_to = 'dataset') %>% 
    ggplot(aes(x = reorder(.observation, sq_error), 
               y = sq_error, 
               color = dataset)) +
    geom_point(shape = 16, 
               size = 1) + 
    scale_color_manual(values = c(train = 'steelblue', 
                                  cv = 'coral3'), 
                       labels = c(train = 'data', 
                                  cv = '10-fold CV'), 
                       name = '') + 
    globals$common_theme + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          panel.grid.major.x = element_blank()) + 
    labs(title = 'Squared error per observation', 
         subtitle = histo_class$brier_caption, 
         x = 'Observation, sorted by squared error', 
         y = 'Squared error')
  
  ## histogram
  
  histo_class$brier_plots$histogram <- 
    histo_class$sq_errors %>% 
    compress(names_to = 'dataset') %>% 
    ggplot(aes(x = sq_error, 
               color = dataset)) +
    geom_density() + 
    scale_color_manual(values = c(train = 'steelblue', 
                                 cv = 'coral3'), 
                      labels = c(train = 'data', 
                                 cv = '10-fold CV'), 
                      name = '') + 
    globals$common_theme + 
    labs(title = 'Squared error distribution', 
         subtitle = histo_class$brier_caption, 
         x = 'Squared error', 
         y = '# observations')
  
# ROC curves -----
  
  insert_msg('ROC curves')
  
  histo_class$roc_plots <- 
    list(x = histo_class$predictions, 
         plot_title = paste('ROC,', c('data', '10-fold CV')), 
         line_color = c('steelblue', 'coral3')) %>% 
    pmap(plot, 
         type = 'roc', 
         cust_theme = globals$common_theme, 
         annotation_x = 0.45) %>% 
    map(~.x + 
          labs(subtitle = .x$labels$subtitle %>% 
                 stri_replace(fixed = 'Kappa', 
                              replacement = '\u03BA') %>% 
                 stri_replace(fixed = 'Rsq', 
                              replacement = 'R\u00B2')))
  
# Variable importance -------
  
  insert_msg('Variable importance')
  
  ## retrieved from the model by the genuine party's varimp
  ## to get a more informative delta accuracy measure
  
  histo_class$var_importance <- histo_class$model$finalModel %>% 
    varimp %>% 
    compress(names_to = 'parameter', 
             values_to = 'delta_accuracy')
  
  ## retrieving variable and level names

  histo_class$var_importance <- 
    histo_class$var_importance %>% 
    mutate(variable = stri_extract(parameter, 
                                   regex = paste(sort(histo_class$variables, 
                                                      decreasing = TRUE), 
                                                 collapse = '|')), 
           level = stri_replace(parameter, 
                                regex = paste(sort(histo_class$variables, 
                                                   decreasing = TRUE), 
                                              collapse = '|'), 
                                replacement = ''), 
           var_label = exchange(variable, 
                                dict = tesca$lexicon), 
           var_label = ifelse(level == '', 
                              var_label, 
                              paste(var_label, level, sep = ': ')))
  
# Plotting of the variable importance -------
  
  insert_msg('Plotting of the variable importance')
  
  histo_class$importance_plot <- 
    histo_class$var_importance %>% 
    ggplot(aes(x = delta_accuracy, 
               y = reorder(var_label, delta_accuracy))) + 
    geom_bar(stat = 'identity',
             color = 'black', 
             fill = 'steelblue') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Variable importance', 
         subtitle = histo_class$n_numbers, 
         x = expression(Delta * ' accuracy'))
  
# END -------
  
  insert_tail()