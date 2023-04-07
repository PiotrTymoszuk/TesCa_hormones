# Multi-parameter Cox modeling
# Clinical strata of sex hormone levels and explanatory variables 
# listed below, it was hard to find a good initial set
# with acceptable missingness

# Container -------

  multi_cut <- list()

# Parallel backend -------

  insert_msg('Parallel backnd')
  
  plan('multisession')

# Analysis globals --------

  insert_msg('Analysis globals')

  ## variables
  
  multi_cut$variables <- c('PRL_class',
                           'E2_class', 
                           'T_total_class', 
                           'LH_class', 
                           'FSH_class', 
                           'HCG_class', 
                           'testosterone_replacement', 
                           'LDH_class', 
                           'cs_lugano', 
                           'radiotherapy', 
                           'infiltration_rete_testis', 
                           'seminoma_cancer', 
                           'embryonal_cancer', 
                           'HCG_class', 
                           'chemotherapy', 
                           'teratoma_cancer', 
                           'RLA', 
                           'max_size_cm_sqrt', 
                           'lymphovas_invasion', 
                           'histology', 
                           'pt_stage', 
                           'age_surgery_sqrt')

  ## analysis table
  ## for numeric variables, second order terms are included as well
  ## normalization
  
  multi_cut$analysis_tbl <- surv_globals$data %>% 
    select(ID, relapse, rfs_days, any_of(multi_cut$variables))
  
  multi_cut$numeric_vars <- multi_cut$analysis_tbl %>% 
    map_lgl(is.numeric)
  
  multi_cut$numeric_vars <- 
    names(multi_cut$analysis_tbl)[multi_cut$numeric_vars]
  
  multi_cut$numeric_vars <- 
    multi_cut$numeric_vars[!multi_cut$numeric_vars %in% c('relapse', 'rfs_days')]
  
  for(i in multi_cut$numeric_vars) {
    
    multi_cut$analysis_tbl <- multi_cut$analysis_tbl %>% 
      mutate(!!paste0(i, '_sec') := scale(.data[[i]]^2)[, 1], 
             !!i := scale(.data[[i]])[, 1])
    
  }
  
  multi_cut$analysis_tbl <- multi_cut$analysis_tbl %>%
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')

  ## X and Y matrices

  multi_cut$y <- Surv(multi_cut$analysis_tbl$rfs_days, 
                      multi_cut$analysis_tbl$relapse)
  
  multi_cut$x <- multi_cut$analysis_tbl %>% 
    select(- rfs_days, -relapse) %>% 
    model.matrix(~., data = .)

  ## alpha and number of repeats
  
  multi_cut$alpha <- 1
  
  multi_cut$n_repeats <- 100
  
  ## CV folds
  
  set.seed(1234)
  
  multi_cut$folds <- 1:multi_cut$n_repeats %>% 
    map(function(x) createFolds(y = multi_cut$analysis_tbl$relapse, 
                                k = 10, 
                                list = FALSE, 
                                returnTrain = TRUE)) %>% 
    set_names(paste0('rep_', 1:multi_cut$n_repeats))
  
  ## n numbers
  
  multi_cut$n_numbers <- multi_cut$analysis_tbl %>% 
    count(relapse)
  
  multi_cut$n_numbers <- 
    paste0('total: n = ', sum(multi_cut$n_numbers$n), 
           ', events: n = ', multi_cut$n_numbers$n[2])
  
# Model tuning -------
  
  insert_msg('Model tuning')
  
  multi_cut$tune_models <- multi_cut$folds %>% 
    future_map(~cv.glmnet(x = multi_cut$x, 
                          y = multi_cut$y, 
                          type.measure = 'default', 
                          family = 'cox', 
                          alpha = multi_cut$alpha, 
                          foldid = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## optimal lambda: minimal deviance in CV
  
  multi_cut$lambda_tbl <- multi_cut$tune_models %>% 
    map(~as_tibble(.x[c('lambda', 'cvm', 'cvup', 'cvlo')])) %>% 
    map2_dfr(., multi_cut$tune_models, 
             ~filter(.x, lambda == .y[['lambda.min']]))
  
  multi_cut$opt_lambda <- multi_cut$lambda_tbl %>% 
    filter(cvm == min(cvm))
  
# Fitting the training model -----
  
  insert_msg('Fitting the training Elastic Net model')
  
  multi_cut$glmnet_model <- 
    glmnet(x = multi_cut$x, 
           y = multi_cut$y, 
           family = 'cox', 
           alpha = multi_cut$alpha, 
           lambda = multi_cut$opt_lambda$lambda)
  
# Linear predictor score --------
  
  insert_msg('Linear predictor score')
  
  multi_cut$lp_score <- multi_cut$glmnet_model %>% 
    predict(newx = multi_cut$x) %>% 
    as.data.frame %>% 
    rownames_to_column('ID') %>% 
    set_names(c('ID', 'lp_score')) %>% 
    left_join(multi_cut$analysis_tbl %>% 
                rownames_to_column('ID') %>% 
                select(ID, relapse, rfs_days), 
              by = 'ID')
  
# Univariate Cox model -------
  
  insert_msg('Univariate Cox model')
  
  multi_cut$coxph_model <- 
    coxph(Surv(rfs_days, relapse) ~ lp_score, 
          data = multi_cut$lp_score, 
          x = TRUE, 
          y = TRUE) %>% 
    as_coxex(data = multi_cut$lp_score)
  
# Model assumptions and fit stats --------
  
  insert_msg('Univariable Cox model assumptions and fit stats')
  
  ## assumptions and stats, training cohort
  
  multi_cut[c('assumptions', 
              'stats')] <- c('assumptions', 'fit') %>% 
    map(~summary(multi_cut$coxph_model, type = .x))
  
  ## stats for CV
  
  set.seed(1234)
  
  multi_cut$resample_stats <- multi_cut$coxph_model %>% 
    validate(method = 'crossvalidation', 
             B = 10)
  
# C-index plot ------
  
  insert_msg('C-index plot')
  
  multi_cut$c_index_plot <- multi_cut$resample_stats %>% 
    filter(dataset %in% c('training', 'test')) %>% 
    ggplot(aes(x = c_index, 
               y = dataset, 
               color = dataset)) + 
    geom_vline(xintercept = 0.5, 
               linetype = 'dashed') +
    geom_point(shape = 16, 
               size = 2) +
    scale_color_manual(values = c(test = 'coral3', 
                                  training = 'steelblue'), 
                       labels = c(test = '10-fold CV', 
                                  training = 'data')) +
    scale_x_continuous(limits = c(0.5, 1)) + 
    scale_y_discrete(labels = c(test = '10-fold CV', 
                                training = 'data')) + 
    geom_text_repel(aes(label = signif(c_index, 2)), 
                    size = 2.5, 
                    hjust = 0.5, 
                    vjust = -1.2, 
                    direction = 'x') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'none') + 
    labs(title = 'Model concordance', 
         x = 'C-index')
  
# Model coefficients -------
  
  insert_msg('Model coefficients')
  
  ## non-zero coefficients
  
  multi_cut$coefs <- multi_cut$glmnet_model %>% 
    coef %>% 
    as.matrix %>% 
    as.data.frame %>% 
    rownames_to_column('parameter') %>% 
    set_names(c('parameter', 'estimate')) %>% 
    mutate(hr = exp(estimate)) %>% 
    filter(estimate != 0) %>% 
    as_tibble
  
  multi_cut$coefs <- multi_cut$coefs %>% 
    mutate(order = ifelse(stri_detect(parameter, regex = '_sec$'), 
                          'second', 'first'), 
           variable = stri_extract(parameter, 
                                   regex = paste(sort(multi_cut$variables), 
                                                 collapse = '|')), 
           format = ifelse(variable %in% multi_cut$numeric_vars, 
                           'numeric', 'factor'), 
           level = ifelse(format == 'numeric', 
                          NA, 
                          stri_replace(parameter, 
                                       regex = paste(sort(multi_cut$variables), 
                                                     collapse = '|'), 
                                       replacement = '')), 
           var_label = ifelse(variable != 'PRL_strata', 
                              exchange(variable, 
                                       dict = surv_globals$lexicon), 
                              'PRL strata'), 
           var_label = ifelse(order == 'second', 
                              paste0('(', var_label, ')\u00B2'), 
                              var_label), 
           var_label = ifelse(!is.na(level) & level != 'yes', 
                              paste(var_label, level, sep = ': '), 
                              var_label))
  
# Plotting the coefs as a bubble plot ---------
  
  insert_msg('Plotting the coefficients')
  
  multi_cut$coef_plot <- multi_cut$coefs %>% 
    ggplot(aes(x = hr, 
               y = reorder(var_label, hr), 
               size = abs(log(hr)), 
               fill = log(hr))) + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    geom_point(shape = 21) + 
    geom_text(aes(label = signif(hr, 2)), 
              size = 2.3, 
              vjust = -1.2) + 
    scale_radius(range(0.5, 4.5), 
                 name = 'log HR') + 
    scale_fill_gradient2(low = 'steelblue', 
                         mid = 'white', 
                         high = 'firebrick', 
                         name = 'abs(log HR)') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Elastic Net Cox model coefficients', 
         x = expression('HR'[LASSO]))
  
# Linear predictor score tertile and survival ------
  
  insert_msg('Linear predictor score tertiles and survival')
  
  ## calibrator object, LP score tertiles
  
  multi_cut$calibrator_obj <- multi_cut$coxph_model %>% 
    calibrate(n = 3, 
              labels = c('low', 'int', 'high'))
  
  ## testing for for differences between the score tertiles
  
  multi_cut$tertile_test <- 
    multi_cut$calibrator_obj$surv_fit %>% 
    surv_pvalue %>% 
    adjust_fdr('pval')
  
  ## Kaplan-Meier plot for the LP score tertiles
  
  multi_cut$tertile_km <- multi_cut$calibrator_obj %>% 
    plot(palette = c('steelblue', 
                     'coral2', 
                     'coral4'), 
         cust_theme = globals$common_theme, 
         show_cox = TRUE,  
         title = 'LASSO Score', 
         xlab = 'Relapse-free survival, days') + 
    labs(subtitle = multi_cut$n_numbers) +
    theme(legend.position = 'right', 
          plot.tag = element_blank()) + 
    annotate('text', 
             label = multi_cut$tertile_test$significance, 
             x = 0.1, 
             y = 0.1, 
             size = 2.75, 
             hjust = 0, 
             vjust = 0)
  
# Model calibration with Brier scores ------
  
  insert_msg('Model calibration, Brier scores in time')
  
  multi_cut$brier_obj <- multi_cut$coxph_model %>% 
    surv_brier(splitMethod = 'cv10')
  
  ## IBS
  
  multi_cut$resample_stats <- multi_cut$brier_obj %>% 
    select(training, test) %>% 
    map_dbl(mean, na.rm = TRUE) %>% 
    compress(names_to = 'dataset', 
             values_to = 'IBS') %>% 
    left_join(multi_cut$resample_stats, ., by = 'dataset')
  
  ## plot
  
  multi_cut$brier_plot <- multi_cut$brier_obj %>% 
    plot(show_reference = FALSE, 
         cust_theme = globals$common_theme) + 
    labs(subtitle = paste0('IBS: training: ', 
                           signif(multi_cut$resample_stats$IBS[2], 2), 
                           ', test: ', 
                           signif(multi_cut$resample_stats$IBS[3], 2))) + 
    scale_color_manual(values = c(test = 'coral3', 
                                  training = 'steelblue'), 
                       labels = c(test = '10-fold CV', 
                                  training = 'data'), 
                       name = '')
  
# END ------
  
  plan('sequential')
  
  insert_tail()