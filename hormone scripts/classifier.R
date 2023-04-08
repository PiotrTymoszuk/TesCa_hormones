# Multi-parameter signature of latent classes 
# developed by conditional Random Forests

  insert_head()

# container ---------

  class_rf <- list()

# analysis globals -------

  insert_msg('Analysis globals')

  ## variables and analysis table

  class_rf$variables <- c('age_surgery', 
                          'surgery_type', 
                          'pt_stage', 
                          'residual_tumor', 
                          'max_size_cm', 
                          'histology', 
                          'teratoma_percent', 
                          'embryonal_percent', 
                          'yolk_sac_ca_percent',
                          'seminoma_percent', 
                          'infiltration_rete_testis', 
                          'lymphovas_invasion', 
                          'cs_lugano', 
                          'AFP', 
                          'AFP_class', 
                          'HCG', 
                          'HCG_class', 
                          'LDH',
                          'LDH_class', 
                          'chemotherapy', 
                          'radiotherapy', 
                          'RLA', 
                          'testosterone_replacement')

  class_rf$analysis_tbl <- tesca$data %>% 
    select(ID, all_of(class_rf$variables)) %>% 
    left_join(lca$assingment, ., by = 'ID') %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')

  ## observation lexicon
  
  class_rf$obs_lexicon <- 
    tibble(ID = rownames(class_rf$analysis_tbl), 
           .observation = 1:nrow(class_rf$analysis_tbl))
  
  ## model formula
  
  class_rf$formula <- 
    paste('class ~', 
          paste(class_rf$variables, collapse = ' + ')) %>% 
    as.formula
  
  ## n numbers
  
  class_rf$n_numbers <- class_rf$analysis_tbl %>% 
    count(class)
  
  class_rf$n_numbers <- 
    map2_chr(class_rf$n_numbers[[1]], 
             class_rf$n_numbers[[2]], 
             paste, sep =': n = ') %>% 
    paste(collapse = ', ')
  
# tuning of the model -------
  
  insert_msg('Tuning of the model')
  
  registerDoParallel(cores = 7)
  
  set.seed(1234)
  
  class_rf$model <- 
    caret::train(form = class_rf$formula, 
                 data = class_rf$analysis_tbl, 
                 method = 'cforest', 
                 metric = 'Kappa', 
                 trControl = trainControl(method = 'cv', 
                                          number = 10, 
                                          savePredictions = 'final', 
                                          returnData = TRUE, 
                                          returnResamp = 'final', 
                                          classProbs = TRUE), 
                 tuneGrid = data.frame(mtry = 2:(length(class_rf$variables) - 3)), 
                 controls = cforest_classical()) %>% 
    as_caretx
  
  stopImplicitCluster()
  
# Fit stats -------
  
  insert_msg('Fit stats')
  
  class_rf$stats <- class_rf$model %>% 
    summary %>% 
    map(select, statistic, estimate) %>% 
    map(column_to_rownames, 'statistic') %>% 
    map(t) %>% 
    map(as.data.frame) %>% 
    compress(names_to = 'dataset') %>% 
    as_tibble
  
  class_rf$stats <- class_rf$stats %>% 
    mutate(plot_cap = paste0('Accuracy: ', 
                             signif(correct_rate, 2), 
                             ', \u03BA: ', 
                             signif(kappa, 2), 
                             ', n = ', nrow(class_rf$analysis_tbl)))
  
# Model predictions -----
  
  insert_msg('Model predictions')
  
  class_rf$predictions <- class_rf$model %>% 
    predict %>% 
    compact
  
# Class-specific accuracy -------
  
  insert_msg('Class-specific accuracy')
  
  class_rf$class_accuracy <- class_rf$predictions %>% 
    map(~.x$data) %>% 
    map(mutate, correct = .outcome == .fitted) %>% 
    map(blast, .outcome) %>% 
    map(~map_dbl(.x, ~sum(.x$correct)/nrow(.x))) %>% 
    map(compress, 
        names_to = 'class', 
        values = 'accuracy')
  
  class_rf$class_labs <- class_rf$class_accuracy %>% 
    map(~map2_chr(.x[[1]], .x[[2]], 
                  ~paste(.x, signif(.y, 2), sep = '\ncorrect: ')))
    
  
# Plotting the confusion matrix -------
  
  insert_msg('Confusion matrices')
  
  class_rf$confusion_plots <- 
    list(x = class_rf$predictions, 
         plot_title = c('Prediction accuracy, data', 
                        'Prediction accuracy, 10-fold CV'), 
         plot_subtitle = class_rf$stats$plot_cap) %>% 
    pmap(plot, 
         type = 'confusion', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Outome', 
         y_lab = 'Predicted') %>% 
    map2(., class_rf$class_labs, 
         ~.x + 
           scale_x_discrete(labels = .y) + 
           theme(plot.tag = element_blank()) +
           scale_fill_gradient(low = 'white', 
                               high = 'firebrick', 
                               name = '% of observations', 
                               limits = c(0, 50)))
  
# Variable importance -------
  
  insert_msg('Variable importance')
  
  ## retrieved from the model by the genuine party's varimp
  ## to get a more informative delta accuracy measure
  
  class_rf$var_importance <- class_rf$model$finalModel %>% 
    varimp %>% 
    compress(names_to = 'parameter', 
             values_to = 'delta_accuracy')
  
  ## retrieving variable and level names
  
  class_rf$var_importance <- 
    class_rf$var_importance %>% 
    mutate(variable = stri_extract(parameter, 
                                   regex = paste(sort(class_rf$variables, 
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
  
  class_rf$importance_plot <- 
    class_rf$var_importance %>% 
    ggplot(aes(x = delta_accuracy, 
               y = reorder(var_label, delta_accuracy))) + 
    geom_bar(stat = 'identity',
             color = 'black', 
             fill = 'steelblue') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Variable importance', 
         subtitle = class_rf$n_numbers, 
         x = expression(Delta * ' accuracy'))
  
# END -------
  
  insert_tail()