# Univariate Cox modeling

  insert_head()
  
# container -------
  
  uni_cox <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## model formulas: factors
  
  uni_cox$formulas$factors <- surv_globals$variables$factor %>% 
    paste('Surv(rfs_days, relapse) ~', .) %>% 
    map(as.formula) %>% 
    set_names(surv_globals$variables$factor)
  
  ## model formulas: numeric, first order
  
  uni_cox$formulas$first_order <- surv_globals$variables$numeric %>% 
    paste('Surv(rfs_days, relapse) ~', .) %>% 
    map(as.formula) %>% 
    set_names(surv_globals$variables$numeric)
  
  ## model formulas: numeric first- and second-order terms
  
  uni_cox$formulas$second_order <- surv_globals$variables$numeric %>% 
    paste0('Surv(rfs_days, relapse) ~ ', ., ' + ', ., '_sec') %>% 
    map(as.formula) %>% 
    set_names(surv_globals$variables$numeric)
  
  ## analysis tables: second-order terms
  
  uni_cox$analysis_tbl <- surv_globals$data
  
  for(i in surv_globals$variables$numeric) {
    
    uni_cox$analysis_tbl <- uni_cox$analysis_tbl %>% 
      mutate(!!paste0(i, '_sec') := .data[[i]]^2)
    
  }
  
# cox models --------
  
  insert_msg('Building the Cox models')
  
  uni_cox$models <- list()
  
  for(i in names(uni_cox$formulas)) {
    
    uni_cox$models[[i]] <- 
      uni_cox$formulas[[i]] %>% 
      map(~call2(.fn = 'coxph', 
                 formula = .x, 
                 data = uni_cox$analysis_tbl, 
                 x = TRUE)) %>% 
      map(eval) %>% 
      map(~as_coxex(.x, data = uni_cox$analysis_tbl ))
    
  }
  
# Assumptions ------
  
  insert_msg('Model assumptions')

  ## the proportional hazard assumption is the key
  
  uni_cox$assumptions <- uni_cox$models %>% 
    map(~map(.x, summary, type = 'assumptions')) %>% 
    map(compress, names_to = 'variable')
  
# Fit statistics, training data ------
  
  insert_msg('Fit statistics, training data')
  
  uni_cox$stats <- uni_cox$models %>% 
    map(~map(.x, summary, type = 'fit')) %>% 
    map(compress, names_to = 'variable') %>% 
    map(mutate, 
        concordant = ifelse(lower_ci > 0.5, 'yes', 'no'), 
        concordant = factor(concordant, c('no', 'yes')),
        plot_lab = paste0(signif(c_index, 2), 
                          ' [', signif(lower_ci, 2), 
                          ' - ', signif(upper_ci, 2), ']'))
  
  ## plotting the fit stats: C-index with its confidence intervals
  
  uni_cox$c_index_plots <- 
    list(x = uni_cox$stats, 
         y = c('Categorical variables', 
               'Numeric variables, first order', 
               'Numeric variables, first and second order')) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = c_index, 
                      y = reorder(variable, c_index), 
                      color = concordant)) + 
           geom_vline(xintercept = 0.5, 
                      linetype = 'dashed') + 
           geom_errorbarh(aes(xmin = lower_ci,
                              xmax = upper_ci),
                          height = 0) + 
           geom_point(size = 2, 
                      shape = 16) + 
           geom_text(aes(label = plot_lab), 
                     size = 2.5, 
                     vjust = -1.2) + 
           scale_color_manual(values = c(no = 'gray69', 
                                         yes = 'coral3')) + 
           scale_y_discrete(labels = exchange(surv_globals$lexicon$variable, 
                                              dict = surv_globals$lexicon, 
                                              value = 'label')) + 
           scale_x_continuous(limits = c(0.3, 0.75), 
                              breaks = seq(0.3, 0.7, by = 0.1)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'C-index, 95% CI'))
  
  ## fit stats: R-squared
  
  uni_cox$rsq_plots <- 
    list(x = uni_cox$stats, 
         y = c('Categorical variables', 
               'Numeric variables, first order', 
               'Numeric variables, first and second order')) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = raw_rsq, 
                      y = reorder(variable, raw_rsq), 
                      color = concordant, 
                      fill = concordant)) + 
           geom_bar(stat = 'identity', 
                    color = 'black') + 
           geom_text(aes(label = signif(raw_rsq, 2)), 
                     size = 2.5, 
                     hjust = -0.4, 
                     show.legend = FALSE) + 
           scale_color_manual(values = c(no = 'gray69', 
                                         yes = 'coral3')) + 
           scale_fill_manual(values = c(no = 'gray69', 
                                        yes = 'coral3')) +
           scale_y_discrete(labels = exchange(surv_globals$lexicon$variable, 
                                              dict = surv_globals$lexicon, 
                                              value = 'label')) + 
           scale_x_continuous(limits = c(0, 0.4)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = expression('R'^2)))
  
# Fit stats, cross-validation ------
  
  insert_msg('Cross-validation stats')
  
  uni_cox$resample_stats <- uni_cox$models %>% 
    map(~map(.x, 
             validate, 
             method = 'crossvalidation', 
             B = 10)) %>% 
    map(compress, names_to = 'variable')
  
  ## C-index plots
  
  uni_cox$resample_c_plots <- 
    list(x = uni_cox$resample_stats %>% 
           map(filter, dataset %in% c('training', 'test')), 
         y = c('Categorical variables', 
               'Numeric variables, first order', 
               'Numeric variables, first and second order')) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = c_index, 
                      y = reorder(variable, c_index), 
                      color = dataset)) + 
           geom_vline(xintercept = 0.5, 
                      linetype = 'dashed') + 
           geom_point(size = 2, 
                      shape = 16) + 
           geom_text_repel(aes(label = signif(c_index, 2)), 
                           size = 2.5, 
                           vjust = -1.2, 
                           direction = 'x', 
                           show.legend = FALSE) + 
           scale_color_manual(values = c(training = 'steelblue', 
                                         test = 'coral3'), 
                              labels = c(training = 'data', 
                                         test = '10-fold CV'), 
                              name = '') + 
           scale_y_discrete(labels = exchange(surv_globals$lexicon$variable, 
                                              dict = surv_globals$lexicon, 
                                              value = 'label')) + 
           scale_x_continuous(limits = c(0.48, 0.7), 
                              breaks = seq(0.5, 0.7, by = 0.05)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'C-index'))
  
  ## R-squared plots
  
  uni_cox$resample_rsq_plots <- 
    list(x = uni_cox$resample_stats %>% 
           map(filter, dataset %in% c('training', 'test')), 
         y = c('Categorical variables', 
               'Numeric variables, first order', 
               'Numeric variables, first and second order')) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = R2, 
                      y = reorder(variable, R2), 
                      color = dataset)) + 
           geom_point(size = 2, 
                      shape = 16) + 
           geom_text_repel(aes(label = signif(R2, 2)), 
                           size = 2.5, 
                           vjust = -1.2, 
                           direction = 'x', 
                           show.legend = FALSE) + 
           scale_color_manual(values = c(training = 'steelblue', 
                                         test = 'coral3'), 
                              labels = c(training = 'data', 
                                         test = '10-fold CV'), 
                              name = '') + 
           scale_y_discrete(labels = exchange(surv_globals$lexicon$variable, 
                                              dict = surv_globals$lexicon, 
                                              value = 'label')) + 
           scale_x_continuous(limits = c(0, 0.1), 
                              breaks = seq(0, 0.1, by = 0.025)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = expression('Nagelkirke R'^2)))
  
# LRT testing, second-order terms ------
  
  insert_msg('LRT testing, second order terms')
  
  uni_cox$lrt <- 
    map2(map(uni_cox$models$first_order, as_coxph), 
         map(uni_cox$models$second_order, as_coxph), 
         anova)
  
  uni_cox$lrt_summary <- uni_cox$lrt %>% 
    map(as.data.frame) %>% 
    map(~.x[2, ]) %>%
    map(set_names, c('loglik', 'chisq', 'df', 'p_value')) %>% 
    compress(names_to = 'variable') %>% 
    as_tibble
  
# Inference ------
  
  insert_msg('Inference')
  
  uni_cox$inference <- uni_cox$models %>% 
    map(~map_dfr(.x, summary, type = 'inference')) %>% 
    map(mutate, 
        order = ifelse(stri_detect(parameter, fixed = '_sec'), 
                       'second', 'first'), 
        level = ifelse(order == 'second', '', level), 
        label = exchange(variable, 
                         dict = surv_globals$lexicon, 
                         value = 'label'), 
        label = ifelse(order == 'second', 
                       paste0('(', label, ')\u00B2'), 
                       label),
        label = ifelse(!is.na(n), 
                       paste(label, level, sep = ': '), 
                       label),
        label = ifelse(!is.na(n), 
                       paste0(label, '\nn = ', n, ', total: n = ', n_complete), 
                       paste0(label, '\ntotal: n = ', n_complete)), 
        estimate = exp(estimate), 
        lower_ci = exp(lower_ci), 
        upper_ci = exp(upper_ci), 
        hr_lab = paste0(signif(estimate, 2), 
                        ' [', signif(lower_ci, 2), 
                        ' - ', signif(upper_ci, 2), ']'), 
        significance = ifelse(p_value < 0.05, 
                              ifelse(estimate > 1, 
                                     'unfavorable', 
                                     'favorable'), 
                              'ns'), 
        significance = factor(significance, 
                              c('unfavorable', 'favorable', 'ns')))
  
# Forest plots for HR estimates --------
  
  insert_msg('Forest plots for HR estimates')
  
  uni_cox$forest_plots <- 
    list(x = uni_cox$inference, 
         y = c('Categorical variables', 
               'Numeric variables, first order', 
               'Numeric variables, first and second order'), 
         z = c('HR, 95% CI', 
               'HR, 95% CI, Z-score', 
               'HR, 95% CI, Z-score')) %>% 
    pmap(function(x, y, z) x %>%
           ggplot(aes(x = estimate, 
                      y = reorder(label, estimate), 
                      color = significance, 
                      shape = order)) + 
           geom_vline(xintercept = 1, 
                      linetype = 'dashed') + 
           geom_errorbarh(aes(xmin = lower_ci, 
                              xmax = upper_ci), 
                          height = 0, 
                          position = position_dodge(width = 0.75)) + 
           geom_point(size = 2, 
                      position = position_dodge(width = 0.75)) + 
           geom_text(aes(label = hr_lab), 
                     size = 2.5, 
                     vjust = -1.1, 
                     hjust = 0, 
                     show.legend = FALSE, 
                     position = position_dodge(width = 0.75)) + 
           scale_color_manual(values = c(unfavorable = 'firebrick', 
                                         favorable = 'steelblue', 
                                         ns = 'gray60'), 
                              name = '') + 
           scale_shape_manual(values = c(first = 16, 
                                         second = 18), 
                              name = 'Term order') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = z))
  
  ## facets for the second order model Forests
  
  uni_cox$forest_plots$second_order <- 
    uni_cox$forest_plots$second_order + 
    facet_grid(variable ~ ., 
               scales = 'free', 
               space = 'free') + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())

# END ------
  
  rm(i)
  
  insert_tail()