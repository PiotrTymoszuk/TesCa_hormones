# Elastic Net Cox modeling of relapse-free survival

  insert_head()
  
# container ------
  
  tcga_cox <- list()
  
# parallel backend -----
  
  insert_msg('Parallel backend')

  plan('multisession')
    
# analysis globals ------
  
  insert_msg('Analysis globals')

  ## analysis tables
  
  tcga_cox$conf_variables <- c('histology', 'age_surgery', 'pt_stage', 
                               'radiotherapy', 'marker_status')
  
  tcga_cox$analysis_tbl[c('clinical', 
                          'genes', 
                          'class')] <- 
    list(tcga$clinical %>% 
           select(ID, rfs_days, relapse, tcga_cox$conf_variables), 
         tcga$clinical %>% 
           select(ID, rfs_days, relapse, tcga_cox$conf_variables) %>% 
           left_join(tcga_relapse$cutpoints$strata_tbl %>% 
                       unclass %>% 
                       as.data.frame %>% 
                       select(-relapse, -rfs_days) %>% 
                       map_dfc(factor, c('low', 'high')) %>% 
                       mutate(ID = tcga_relapse$analysis_tbl$ID),  
                     by = 'ID'), 
         tcga$clinical %>% 
           select(ID, rfs_days, relapse, tcga_cox$conf_variables) %>% 
           left_join(tcga_mix$assignment, 
                     by = 'ID')) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(column_to_rownames, 'ID')
  
  ## variable regex
  
  tcga_cox$all_vars <- tcga_cox$analysis_tbl %>% 
    map(names) %>% 
    map(~.x[!.x %in% c('relapse', 'rfs_days')]) %>% 
    reduce(union) %>% 
    sort(decreasing = TRUE)

  tcga_cox$var_regex <- paste(tcga_cox$all_vars, collapse = '|')  
  
  ## Y and X matrices
  
  tcga_cox$y <- tcga_cox$analysis_tbl %>% 
    map(~Surv(.x$rfs_days, .x$relapse))
  
  tcga_cox$x <- tcga_cox$analysis_tbl %>% 
    map(select, -rfs_days, -relapse) %>% 
    map(~model.matrix(~., data = .))
  
  ## numbers of events
  
  tcga_cox$event_n <- tcga_cox$analysis_tbl %>% 
    map(count, relapse)
  
  tcga_cox$event_lab <- tcga_cox$event_n %>% 
    map(~paste0('total: n = ', sum(.x$n), 
                ', events: n = ', .x$n[2]))
  
  
  ## alpha and number of repeats
  
  tcga_cox$alpha <- 0.5
  
  tcga_cox$n_repeats <- 100
  
  ## CV folds
  
  set.seed(1234)
  
  tcga_cox$folds <- 1:tcga_cox$n_repeats %>% 
    map(function(x) createFolds(y = tcga_cox$analysis_tbl[[1]]$relapse, 
                                k = 10, 
                                list = FALSE, 
                                returnTrain = TRUE)) %>% 
    set_names(paste0('rep_', 1:multi_cox$n_repeats))
  
# Model tuning -------
  
  insert_msg('Model tuning')
  
  tcga_cox$tune_models <- 
    list(exp = tcga_cox$x, 
         surv = tcga_cox$y) %>% 
    pmap(function(expl, surv) tcga_cox$folds %>% 
           future_map(~cv.glmnet(x = expl, 
                                 y = surv, 
                                 type.measure = 'default', 
                                 family = 'cox', 
                                 alpha = tcga_cox$alpha, 
                                 foldid = .x), 
                      .options = furrr_options(seed = TRUE)))
  
  ## optimal lambda: minimal deviance in CV
  
  tcga_cox$lambda_tbl <- tcga_cox$tune_models %>% 
    map(function(model) model %>% 
          map(~as_tibble(.x[c('lambda', 'cvm', 'cvup', 'cvlo')])) %>% 
          map2_dfr(., model, 
                   ~filter(.x, lambda == .y[['lambda.min']])))

  tcga_cox$opt_lambda <- tcga_cox$lambda_tbl %>% 
    map(filter, cvm == min(cvm)) %>% 
    compress(names_to = 'model')
  
# Fitting the training models -----
  
  insert_msg('Fitting the training Elastic Net models')
  
  tcga_cox$glmnet_models <- 
    list(x = tcga_cox$x, 
         y = tcga_cox$y, 
         lambda = tcga_cox$opt_lambda$lambda) %>% 
    pmap(glmnet, 
         family = 'cox', 
         alpha = tcga_cox$alpha)
  
# Linear predictor scores ------
  
  insert_msg('Linrear predictor scores')
  
  tcga_cox$lp_scores <- 
    list(object = tcga_cox$glmnet_models , 
         newx = tcga_cox$x) %>% 
    pmap(predict) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'ID') %>% 
    map(set_names, c('ID', 'lp_score')) %>% 
    map(left_join, 
        tcga$clinical[c('ID', 'relapse', 'rfs_days')], 
        by = 'ID') %>% 
    map(as_tibble)

# Univariate Cox models -----
  
  insert_msg('Univariate models')

  tcga_cox$coxph_models <- tcga_cox$lp_scores %>% 
    map(~call2('coxph', 
               formula = Surv(rfs_days, relapse) ~ lp_score, 
               data = .x, 
               x = TRUE, 
               y = TRUE)) %>% 
    map(eval) %>% 
    map2(., tcga_cox$lp_scores, 
         as_coxex)
  
# Model assumptions ----
  
  insert_msg('Model assumptions')
  
  tcga_cox$assumptions <- tcga_cox$coxph_models %>% 
    map(summary, 'assumptions')
  
# Training and CV stats stats -----
  
  insert_msg('Training and CV stats stats')
  
  tcga_cox$stats <- tcga_cox$coxph_models %>% 
    map(summary, 'fit') %>% 
    compress(names_to = 'model')
  
  tcga_cox$resample_stats <- tcga_cox$coxph_models %>% 
    map(validate, 
        method = 'crossvalidation', 
        B = 10)
  
# Resample Brier scores ------
  
  insert_msg('Brier scores')
  
  tcga_cox$brier_obj <- tcga_cox$coxph_models %>% 
    map(surv_brier, splitMethod = 'cv10')
   
  tcga_cox$ibs <- tcga_cox$brier_obj %>% 
    map(select, reference, training, test) %>% 
    map(colMeans, na.rm = TRUE) %>% 
    map(compress, 
        names_to = 'dataset', 
        values_to = 'ibs')
  
  tcga_cox$resample_stats <- 
    map2(tcga_cox$resample_stats, 
         tcga_cox$ibs, 
         full_join, by = 'dataset')
  
  tcga_cox$ibs <- NULL
  tcga_cox <- compact(tcga_cox)
  
# Plotting the resample stats -----
  
  insert_msg('Plotting the resample stats')
  
  tcga_cox$stat_plots <- tcga_cox$resample_stats %>% 
    compress(names_to = 'model') %>% 
    filter(dataset %in% c('training', 'test')) %>% 
    mutate(dataset = factor(dataset, c('training', 'test'))) %>% 
    blast(dataset)
  
  tcga_cox$stat_plots <- 
    list(x = tcga_cox$stat_plots, 
         y = c('Training data', '10-fold cross-validation')) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = c_index, 
                      y = 1 - ibs, 
                      color = model)) + 
           geom_hline(yintercept = 0.75, 
                      linetype = 'dashed') + 
           geom_vline(xintercept = 0.1, 
                      linetype = 'dashed') + 
           geom_point(shape = 16, 
                      size = 2) + 
           geom_text_repel(aes(label = model), 
                           size = 2.75) +
           scale_color_manual(values = c(clinical = 'steelblue', 
                                         genes = 'orangered3', 
                                         class = 'firebrick4'), 
                              name = '') +
           expand_limits(x = 1, y = 1) +
           guides(color = 'none') + 
           globals$common_theme + 
           labs(title = y, 
                subtitle = tcga_cox$event_lab[[1]], 
                x = 'C-index', 
                y = '1 - IBS'))
  
# Model calibration: plotting the Brier scores ------
  
  insert_msg('Model calibration: plotting the Brier scores')
  
  tcga_cox$brier_plots <- tcga_cox$brier_obj %>% 
    map(plot, 
        show_reference = TRUE, 
        cust_theme = globals$common_theme) %>% 
    map2(., c('Clinical model', 
              'Clinical + genes', 
              'Clinical + hormonal subsets'), 
         ~.x + 
           labs(title = .y) + 
           scale_y_continuous(limits = c(0, 0.25)))
  
# Model calibration: LP tertiles -----
  
  insert_msg('Model calibration: score tertiles')
  
  ## calibrator objects
  
  tcga_cox$calibration_obj <- tcga_cox$coxph_models %>% 
    map(calibrate, 
        n = 3, 
        labels = c('low', 'int', 'high'))
  
  ## differences between the tertiles
  ## Peto-Peto test
  
  tcga_cox$tertiles_test <- tcga_cox$calibration_obj %>% 
    map(~.x$surv_fit) %>% 
    map(surv_pvalue, method = 'S1') %>% 
    compress(names_to = 'model') %>% 
    adjust_fdr(variable = 'pval')
  
  ## Kaplan-Meier plots
  
  tcga_cox$tertiles_plots <- 
    list(x = tcga_cox$calibration_obj, 
         title = c('Clinical model', 
                   'Clinical + genes', 
                   'Clinical + hormonal subsets')) %>% 
    pmap(plot, 
         palette = c('steelblue', 'gray40', 'firebrick'), 
         cust_theme = globals$common_theme, 
         show_cox = FALSE, 
         legend.title = 'ElasticNet Score', 
         xlab = 'Relapse-free survival, days') %>% 
    map(~.x + 
          theme(legend.position = 'right')) %>% 
    map2(., tcga_cox$tertiles_test$significance, 
         ~.x + 
           annotate('text', 
                    label = .y, 
                    x = 100, 
                    y = 0.1, 
                    hjust = 0, 
                    vjust = 0, 
                    size = 2.75)) %>% 
    map2(., tcga_cox$event_lab,
         ~.x + labs(subtitle = .y))
  
# Elastic Net model coefficients ------
  
  insert_msg('Elastic Net model coefficients')
  
  tcga_cox$coefs <- tcga_cox$glmnet_models %>% 
    map(coef) %>% 
    map(as.matrix) %>% 
    map(set_colnames, c('estimate')) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'parameter') %>% 
    map(filter, estimate != 0) %>% 
    map(mutate, hr = exp(estimate)) %>% 
    map(as_tibble)
  
  ## extraction of variables and their levels
  
  tcga_cox$coefs <- tcga_cox$coefs %>% 
    map(mutate, 
        variable = stri_extract(parameter, 
                                regex = tcga_cox$var_regex), 
        level = stri_replace(parameter, 
                             regex = tcga_cox$var_regex, 
                             replacement = ''), 
        level = ifelse(level == '', NA, level), 
        var_label = ifelse(variable %in% tcga_cox$conf_variables, 
                           exchange(variable, 
                                    dict = tcga$lexicon), 
                           ifelse(variable == 'class', 
                                  'Hormonal subset', 
                                  variable)), 
        ax_label = ifelse(is.na(level), 
                          var_label, 
                          paste(var_label, level, sep = ': ')))
  
# Plotting the model estimates ------
  
  insert_msg('Model estimate plots')
  
  tcga_cox$coef_plots <- 
    list(x = tcga_cox$coefs, 
         y = c('Clinical model', 
               'Clinical + genes', 
               'Clinical + hormonal subsets'), 
         z = tcga_cox$event_lab) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = estimate, 
                      y = reorder(ax_label, estimate), 
                      fill = estimate, 
                      size = abs(estimate))) + 
           geom_vline(xintercept = 0, 
                      linetype = 'dashed') + 
           geom_point(shape = 21) + 
           geom_text(aes(label = signif(hr, 2)), 
                     size = 2.5, 
                     hjust = 0.5, 
                     vjust = -1.4) + 
           scale_size_area(max_size = 4.5, 
                           limits = c(0, 6.5), 
                           name = 'abs(log HR)') + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 0, 
                                name = 'log HR', 
                                limits = c(-6.5, 1.4)) + 
           scale_x_continuous(limits = c(-6.5, 1.4)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                subtitle = z, 
                x = expression('log HR'[ElasticNet])))
  
# END ------
  
  plan('sequential')
  
  insert_tail()