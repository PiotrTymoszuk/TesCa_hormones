# Multi-parameter Cox modeling
# Explanatory variables listed below, it was hard to find a good initial set
# with acceptable missingness

  insert_head()
  
# Container -------
  
  multi_cox <- list()
  
# Parallel backend -------
  
  insert_msg('Parallel backnd')
  
  plan('multisession')
  
# Analysis globals --------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  multi_cox$variables <- c('PRL_sqrt', 
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
  
  multi_cox$analysis_tbl <- surv_globals$data %>% 
    select(ID, relapse, rfs_days, all_of(multi_cox$variables)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')
  
  multi_cox$numeric_vars <- multi_cox$analysis_tbl %>% 
    map_lgl(is.numeric)
  
  multi_cox$numeric_vars <- 
    names(multi_cox$analysis_tbl)[multi_cox$numeric_vars]
  
  multi_cox$numeric_vars <- 
    multi_cox$numeric_vars[!multi_cox$numeric_vars %in% c('relapse', 'rfs_days')]
  
  for(i in multi_cox$numeric_vars) {
    
    multi_cox$analysis_tbl <- multi_cox$analysis_tbl %>% 
      mutate(!!paste0(i, '_sec') := scale(.data[[i]]^2)[, 1], 
             !!i := scale(.data[[i]])[, 1])
    
  }
  
  ## X and Y matrices
  
  multi_cox$y <- Surv(multi_cox$analysis_tbl$rfs_days, 
                      multi_cox$analysis_tbl$relapse)

  multi_cox$x <- multi_cox$analysis_tbl %>% 
    select(- rfs_days, -relapse) %>% 
    model.matrix(~., data = .)
  
  ## alpha and number of repeats
  
  multi_cox$alpha <- 1
  
  multi_cox$n_repeats <- 100
  
  ## CV folds
  
  set.seed(1234)
  
  multi_cox$folds <- 1:multi_cox$n_repeats %>% 
    map(function(x) createFolds(y = multi_cox$analysis_tbl$relapse, 
                                k = 10, 
                                list = FALSE, 
                                returnTrain = TRUE)) %>% 
    set_names(paste0('rep_', 1:multi_cox$n_repeats))
  
  ## n numbers
  
  multi_cox$n_numbers <- multi_cox$analysis_tbl %>% 
    count(relapse)
  
  multi_cox$n_numbers <- 
    paste0('total: n = ', sum(multi_cox$n_numbers$n), 
           ', events: n = ', multi_cox$n_numbers$n[2])
    
# Model tuning -------
  
  insert_msg('Model tuning')
  
  multi_cox$tune_models <- multi_cox$folds %>% 
    future_map(~cv.glmnet(x = multi_cox$x, 
                          y = multi_cox$y, 
                          type.measure = 'default', 
                          family = 'cox', 
                          alpha = multi_cox$alpha, 
                          foldid = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## optimal lambda: minimal deviance in CV
  
  multi_cox$lambda_tbl <- multi_cox$tune_models %>% 
    map(~as_tibble(.x[c('lambda', 'cvm', 'cvup', 'cvlo')])) %>% 
    map2_dfr(., multi_cox$tune_models, 
             ~filter(.x, lambda == .y[['lambda.min']]))
  
  multi_cox$opt_lambda <- multi_cox$lambda_tbl %>% 
    filter(cvm == min(cvm))
  
# Fitting the training model -----
  
  insert_msg('Fitting the training Elastic Net model')
  
  multi_cox$glmnet_model <- 
    glmnet(x = multi_cox$x, 
           y = multi_cox$y, 
           family = 'cox', 
           alpha = multi_cox$alpha, 
           lambda = multi_cox$opt_lambda$lambda)
  
# Linear predictor score --------
  
  insert_msg('Linear predictor score')
  
  multi_cox$lp_score <- multi_cox$glmnet_model %>% 
    predict(newx = multi_cox$x) %>% 
    as.data.frame %>% 
    rownames_to_column('ID') %>% 
    set_names(c('ID', 'lp_score')) %>% 
    left_join(multi_cox$analysis_tbl %>% 
                rownames_to_column('ID') %>% 
                select(ID, relapse, rfs_days), 
              by = 'ID')
  
# Univariate Cox model -------
  
  insert_msg('Univariate Cox model')
  
  multi_cox$coxph_model <- 
    coxph(Surv(rfs_days, relapse) ~ lp_score, 
          data = multi_cox$lp_score, 
          x = TRUE, 
          y = TRUE) %>% 
    as_coxex(data = multi_cox$lp_score)

# Model assumptions and fit stats --------
  
  insert_msg('Univariable Cox model assumptions and fit stats')
  
  ## assumptions and stats, training cohort
  
  multi_cox[c('assumptions', 
              'stats')] <- c('assumptions', 'fit') %>% 
    map(~summary(multi_cox$coxph_model, type = .x))
    
  ## stats for CV
  
  set.seed(1234)
  
  multi_cox$resample_stats <- multi_cox$coxph_model %>% 
    validate(method = 'crossvalidation', 
             B = 10)
  
# Model coefficients -------
  
  insert_msg('Model coefficients')
  
  ## non-zero coefficients
  
  multi_cox$coefs <- multi_cox$glmnet_model %>% 
    coef %>% 
    as.matrix %>% 
    as.data.frame %>% 
    rownames_to_column('parameter') %>% 
    set_names(c('parameter', 'estimate')) %>% 
    mutate(hr = exp(estimate)) %>% 
    filter(estimate != 0) %>% 
    as_tibble
  
  multi_cox$coefs <- multi_cox$coefs %>% 
    mutate(order = ifelse(stri_detect(parameter, regex = '_sec$'), 
                          'second', 'first'), 
           variable = stri_extract(parameter, 
                                   regex = paste(sort(multi_cox$variables), 
                                                 collapse = '|')), 
           format = ifelse(variable %in% multi_cox$numeric_vars, 
                           'numeric', 'factor'), 
           level = ifelse(format == 'numeric', 
                           NA, 
                           stri_replace(parameter, 
                                        regex = paste(sort(multi_cox$variables), 
                                                      collapse = '|'), 
                                        replacement = '')), 
           var_label = exchange(variable, 
                                dict = surv_globals$lexicon), 
           var_label = ifelse(order == 'second', 
                              paste0('(', var_label, ')\u00B2'), 
                              var_label), 
           var_label = ifelse(!is.na(level) & level != 'yes', 
                              paste(var_label, level, sep = ': '), 
                              var_label))
  
# Plotting the coefs as a bubble plot ---------
  
  insert_msg('Plotting the coefficients')
  
  multi_cox$coef_plot <- multi_cox$coefs %>% 
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
    labs(title = 'LASSO Cox model coefficients', 
         x = expression('HR'[LASSO]))
  
# Linear predictor score tertile and survival ------
  
  insert_msg('Linear predictor score tertiles and survival')
  
  ## calibrator object, LP score tertiles
  
  multi_cox$calibrator_obj <- multi_cox$coxph_model %>% 
    calibrate(n = 3, 
              labels = c('low', 'int', 'high'))
  
  ## testing for for differences between the score tertiles
  
  multi_cox$tertile_test <- 
    multi_cox$calibrator_obj$surv_fit %>% 
    surv_pvalue %>% 
    adjust_fdr('pval')
  
  ## Kaplan-Meier plot for the LP score tertiles
  
  multi_cox$tertile_km <- multi_cox$calibrator_obj %>% 
    plot(palette = c('steelblue', 
                     'coral2', 
                     'coral4'), 
         cust_theme = globals$common_theme, 
         show_cox = FALSE,  
         title = 'LASSO Score', 
         xlab = 'Relapse-free survival, days') + 
    labs(subtitle = multi_cox$n_numbers) +
    theme(legend.position = 'right', 
          plot.tag = element_blank()) + 
    annotate('text', 
             label = multi_cox$tertile_test$significance, 
             x = 0.1, 
             y = 0.1, 
             size = 2.75, 
             hjust = 0, 
             vjust = 0)
  
# Model calibration with Brier scores ------
  
  insert_msg('Model calibration, Brier scores in time')
  
  multi_cox$brier_obj <- multi_cox$coxph_model %>% 
    surv_brier(splitMethod = 'cv10')
  
  multi_cox$brier_plot <- multi_cox$brier_obj %>% 
    plot(show_reference = FALSE, 
         cust_theme = globals$common_theme)
  
# END ------
  
  plan('sequential')
  
  insert_tail()