# Development of a Random Forest classifier emulating the hormonal subsets
# of the TCGA cohort based on Reactome and Recon signatures, 
# xCell infiltration estimates, gene and protein expression 
# and the most frequent somatic mutations

  insert_head()
  
# container ------
  
  tcga_rf <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## explanatory variables and analysis table
  
  ## protein: not included, patient dropout and hence bias!
  
  #tcga_rf$analysis_tbl$protein <- tcga$protein %>% 
   # set_colnames(make.names(names(tcga$protein))) %>% 
    #select(-sample_id, 
     #      -ARID1A.ARID1A, 
      #     -BRCA2.BRCA2, 
       #    -CASP3.Caspase.3, 
        #   -CASP9.Caspase.9) ## no or constant expression
  
  tcga_rf$transcripts <- 
    tcga_dge$var_genes[!tcga_dge$var_genes %in% tcga_globals$clust_genes]

  tcga_rf$analysis_tbl$expression <- 
    tcga$expression[c('ID', tcga_rf$transcripts)] %>% 
    set_colnames(make.names(c('ID', tcga_rf$transcripts)))
  
  tcga_rf$analysis_tbl[c('reactome', 
                         'recon', 
                         'xcell')] <- 
    list(tcga$reactome, 
         tcga$recon, 
         tcga$xcell)
  
  tcga_rf$analysis_tbl <- tcga_rf$analysis_tbl %>% 
    reduce(left_join, by = 'ID') %>% 
    column_to_rownames('ID') %>% 
    center_data('median') %>% 
    rownames_to_column('ID') %>% 
    inner_join(tcga_mix$assignment, by = 'ID')
  
  ## adding binary data for the most frequent somatic mutations
  ## note: not done, leads to a dropout of the SEM2 and NS T patients
  ## and hence bias!
  
 # tcga_rf$analysis_tbl <- 
  #  inner_join(tcga_rf$analysis_tbl, 
   #            tcga_mut$analysis_tbl[c('ID', tcga_mut$top_mutations)] %>% 
    #             map_dfc(function(x) if(is.factor(x)) as.numeric(x) - 1 else x) %>% 
     #            set_colnames(c('ID', paste0(tcga_mut$top_mutations, '_mut'))), 
      #         by = 'ID')
  
  ## converting class names to "proper" R names
  ## removal of the constant/nearly constant variables
  ## which turn to NA after normalization
  
  tcga_rf$analysis_tbl <- tcga_rf$analysis_tbl %>% 
    mutate(class = car::recode(class, 
                               "'NS PRL' = 'NS_PRL'; 
                               'NS E2' = 'NS_E2'; 
                               'NS T' = 'NS_T'")) %>% 
    relocate(class) %>% 
    column_to_rownames('ID')
  
  tcga_rf$class_lexicon <- 
    tibble(variable = levels(tcga_rf$analysis_tbl$class), 
           label = stri_replace_all(levels(tcga_rf$analysis_tbl$class),
                                    fixed = '_', 
                                    replacement = ' '))
  
  tcga_rf$na_test <- tcga_rf$analysis_tbl %>% 
    map_lgl(~any(is.na(.x))) %>% 
    compress(names_to = 'variable', 
             values_to = 'na_status')
  
  tcga_rf$missing_vars <-  tcga_rf$na_test %>% 
    filter(na_status) %>% 
    .$variable ## variables with constant or nearly constant expression or missing
  
  tcga_rf$analysis_tbl <- 
    tcga_rf$analysis_tbl[!names(tcga_rf$analysis_tbl) %in% c(tcga_rf$missing_vars)]
  
  ## variable lexicon
  
  tcga_rf$lexicon <- 
    list(tcga$reactome_lexicon, 
         tcga$recon_lexicon, 
         tcga$xcell_lexicon, 
         tcga$protein_lexicon %>% 
           mutate(variable = make.names(variable)), 
         tcga$annotation %>% 
           mutate(variable = make.names(gene_symbol), 
                  label = gene_symbol), 
         tibble(variable = paste0(tcga_mut$top_mutations, '_mut'), 
                label = tcga_mut$top_mutations)) %>% 
    set_names(c('reactome', 'recon', 'xcell', 
                'protein', 'expression', 'mutations')) %>%
    map(~.x[c('variable', 'label')]) %>% 
    compress(names = 'var_type')
  
  tcga_rf$lexicon <- tcga_rf$lexicon %>% 
    filter(variable %in% names(tcga_rf$analysis_tbl)) %>% 
    mutate(var_type = factor(var_type, 
                             c('reactome', 'recon', 'xcell', 
                               'protein', 'expression', 'mutations')))
  
  ## model formula
  
  tcga_rf$formula <- 
    paste('class ~', 
          paste(tcga_rf$lexicon$variable, 
                collapse = ' + ')) %>% 
    as.formula
  
  ## n numbers
  
  tcga_rf$n_numbers <- tcga_rf$analysis_tbl %>% 
    count(class)
  
  tcga_rf$n_numbers <- 
    map2_chr(tcga_rf$n_numbers[[1]], 
             tcga_rf$n_numbers[[2]], 
             paste, sep =': n = ') %>% 
    paste(collapse = ', ') %>% 
    stri_replace_all(fixed = '_', 
                     replacement = ' ')
  
  ## CV folds
  
  tcga_rf$cv_folds <- createFolds(y = tcga_rf$analysis_tbl$class, 
                                  k = 50, 
                                  list = TRUE, 
                                  returnTrain = TRUE)
  
  ## labeller function
  
  tcga_rf$label_wrapper <- function(x, ...) {
    
    x %>% 
      stri_wrap(simplify = FALSE, ...) %>% 
      map_chr(paste, collapse = '\n')
    
  }
  
# Training ------
  
  insert_msg('Training of the model')
  
  registerDoParallel(cores = 7)
  
  set.seed(1234)
  
  tcga_rf$model <- 
    caret::train(form = tcga_rf$formula, 
                 data = tcga_rf$analysis_tbl, 
                 method = 'ranger', 
                 metric = 'Kappa', 
                 trControl = trainControl(method = 'cv', 
                                          number = 50, 
                                          savePredictions = 'final', 
                                          returnData = TRUE, 
                                          returnResamp = 'final', 
                                          classProbs = TRUE, 
                                          index = tcga_rf$cv_folds), 
                 tuneGrid = expand.grid(mtry = ncol(tcga_rf$analysis_tbl)/3, 
                                       splitrule = c('gini'), 
                                       min.node.size = 1), 
                 num.trees = 1000) %>% 
    as_caretx
  
  stopImplicitCluster()
  
# Fit stats -------
  
  insert_msg('Fit stats')
  
  tcga_rf$stats <- tcga_rf$model %>% 
    summary %>% 
    map(select, statistic, estimate) %>% 
    map(column_to_rownames, 'statistic') %>% 
    map(t) %>% 
    map(as.data.frame) %>% 
    compress(names_to = 'dataset') %>% 
    as_tibble
  
  tcga_rf$stats <- tcga_rf$stats %>% 
    mutate(plot_cap = paste0('Accuracy: ', 
                             signif(correct_rate, 2), 
                             ', \u03BA: ', 
                             signif(kappa, 2), 
                             ', n = ', nrow(tcga_rf$analysis_tbl)))
  
# Model predictions -----
  
  insert_msg('Model predictions')
  
  tcga_rf$predictions <- tcga_rf$model %>% 
    predict %>% 
    compact
  
# Class-specific accuracy -------
  
  insert_msg('Class-specific accuracy')
  
  tcga_rf$class_accuracy <- tcga_rf$predictions %>% 
    map(~.x$data) %>% 
    map(mutate, correct = .outcome == .fitted) %>% 
    map(blast, .outcome) %>% 
    map(~map_dbl(.x, ~sum(.x$correct)/nrow(.x))) %>% 
    map(compress, 
        names_to = 'class', 
        values = 'accuracy')
  
  tcga_rf$class_labs <- tcga_rf$class_accuracy %>% 
    map(~map2_chr(.x[[1]], .x[[2]], 
                  ~paste(.x, signif(.y, 2), sep = '\ncorrect: ')))
  
# Plotting the confusion matrix -------
  
  insert_msg('Confusion matrices')
  
  tcga_rf$confusion_plots <- 
    list(x = tcga_rf$predictions, 
         plot_title = c('Prediction accuracy, data', 
                        'Prediction accuracy, 50-fold CV'), 
         plot_subtitle = tcga_rf$stats$plot_cap) %>% 
    pmap(plot, 
         type = 'confusion', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Outome', 
         y_lab = 'Predicted') %>% 
    map2(., tcga_rf$class_labs, 
         ~.x + 
           scale_x_discrete(labels = stri_replace(.y, 
                                                  fixed = '_', 
                                                  replacement = ' ')) +
           scale_y_discrete(labels = function(x) stri_replace(x, 
                                                              fixed = '_', 
                                                              replacement = ' ')) + 
           theme(plot.tag = element_blank()) +
           scale_fill_gradient(low = 'white', 
                               high = 'firebrick', 
                               name = '% of observations', 
                               limits = c(0, 40), 
                               oob = scales::squish))
  
# Variable importance -------
  
  insert_msg('Variable importance')
  
  ## Fitting a Ranger model with the optimal set of tuning parameters
  
  set.seed(1234)
  
  tcga_rf$importance$model <- 
    ranger(formula = tcga_rf$formula, 
           data = tcga_rf$analysis_tbl, 
           num.trees = 1000, 
           mtry = tcga_rf$model$bestTune$mtry[1], 
           importance = 'permutation', 
           min.node.size = tcga_rf$model$bestTune$min.node.size[1], 
           splitrule = tcga_rf$model$bestTune$splitrule[1])
  
  ## permutation importance measures
  
  tcga_rf$importance$measures <- 
    ranger::importance(tcga_rf$importance$model) %>% 
    compress(names_to = 'parameter', 
             values_to = 'delta_accuracy') %>% 
    mutate(variable = parameter, 
           level = '', 
           var_label = exchange(variable, 
                                dict = tcga_rf$lexicon)) %>% 
    left_join(tcga_rf$lexicon, by = 'variable')
  
  ## bar plot with the top 20 importance metrics
  ## for the category of explanatory variables
  
  tcga_rf$importance$plot_titles <- 
    c(reactome = 'Reactome pathways', 
      recon = 'Recon metabolic pathways', 
      xcell = 'xCell infiltration', 
      protein = 'Protein expression', 
      expression = 'Transcriptome', 
      mutations = 'Somatic mutations')
  
  tcga_rf$importance$plots <- tcga_rf$importance$measures %>% 
    filter(delta_accuracy > 0) %>% 
    blast(var_type) %>% 
    map(top_n, 20, delta_accuracy)
  
  tcga_rf$importance$plots <- 
    list(importance_data =  tcga_rf$importance$plots , 
         plot_title = tcga_rf$importance$plot_titles[names(tcga_rf$importance$plots)]) %>% 
    pmap(plot_rf_importance) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) tcga_rf$label_wrapper(x, width = 40)))
  
  tcga_rf$importance$plots[c("expression", "mutations")] <- 
    tcga_rf$importance$plots[c("expression", "mutations")] %>% 
    map(~.x + theme(axis.text.y = element_text(face = 'italic')))

# Saving the results -------
  
  insert_msg('Saving the results')
  
  save(tcga_rf, file = './cache/tcga_rf.RData')
  
# END ------
  
  insert_tail()
    