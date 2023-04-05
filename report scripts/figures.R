# Figures for the analysis report

  insert_head()
  
# container -------
  
  figures <- list()
  
# Figure 1: participant and variable missingness -------
  
  insert_msg('Figure 1: missingness')
  
  figures$missing <- 
    plot_grid(missing$patient$histogram, 
              nrow = 2, 
              rel_heights = c(0.4, 0.6)) %>% 
    plot_grid(missing$variable$plot, 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_1_missingness', 
              ref_name = 'missing', 
              caption = paste('Missing data per participant and variable.'), 
              w = 180, 
              h = 180)
  
# Figure 2: information content of the variables --------
  
  insert_msg('Figure 2: Gini coefficient')
  
  figures$gini <- plot_grid(gini$plot) %>% 
    as_figure(label = 'figure_2_information_content_gini', 
              ref_name = 'gini', 
              caption = paste('Informaiton content of the variables measured', 
                              'by Gini coefficient.'),
              w = 100, 
              h = 160)
  
# Figure 3: normality --------
  
  insert_msg('Figure 3: Normality')
  
  figures$normailty <- plot_grid(distr$normality$plot) %>% 
    as_figure(label = 'figure_3_normality', 
              ref_name = 'normality', 
              caption = paste('Normality of numeric study variables assessed', 
                              'by Shapiro-Wilk test.'), 
              w = 130, 
              h = 180)
  
# Figure 4: univariable Cox modeling -------
  
  insert_msg('Figure 4: Univariable Cox modeling: model stats')
  
  ## upper panel: fit stats
  
  figures$uni_stat_factors <- 
    uni_cox[c('resample_c_plots', 
              'resample_rsq_plots')] %>% 
    map(~.$factors) %>% 
    map2(., c('Concordance', 'Explained variance'), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(uni_cox$resample_c_plots$factors + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_4_univariable_stat_categorical_variables',
              ref_name = 'uni_cox_factors', 
              caption = paste('Fit statistisc of univariable Cox models', 
                              'of relapse-free survival with categorical', 
                              'explanatory variables.'), 
              w = 180, 
              h = 160)
  
# Figure 5: univariable Cox modeling stats, numeric explanatory variables -----
  
  insert_msg('Figure 5: univariable Cox modeling stats, numeric features')
  
  figures$uni_stat_numeric <- 
    uni_cox[c('resample_c_plots', 
              'resample_rsq_plots')] %>% 
    map(~.[c('first_order', 'second_order')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map2(., rep(c('Concordance', 'Explained variance'), 2), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(uni_cox$resample_c_plots$factors + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.93, 0.07)) %>% 
    as_figure(label = 'figure_5_univariable_stat_numeric_variables',
              ref_name = 'uni_cox_factors', 
              caption = paste('Fit statistisc of univariable Cox models', 
                              'of relapse-free survival with categorical', 
                              'explanatory variables.'), 
              w = 180, 
              h = 220)

  
    
# Figure 6: inference for univariable models, factors ------
  
  insert_msg('Figure 6: inference for univariable models, factors')
  
  figures$uni_cox_inference_factors <- 
    plot_grid(uni_cox$forest_plots$factors + 
                guides(shape = 'none')) %>% 
    as_figure(label = 'figure_6_uni_cox_inference_factors', 
              ref_name = 'uni_cox_inference_factors', 
              caption = paste('Inference for univariable Cox models of', 
                              'relapse-free survival with categorical', 
                              'explanatory variables.'), 
              w = 180, 
              h = 160)
  
# Figure 7: inference for univariate models, numeric explanatory variables ----
  
  insert_msg('Figure 7: univariable model inference, nmeric variables')
  
  figures$uni_cox_inference_numeric <- 
    uni_cox$forest_plots[c("first_order", "second_order")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(uni_cox$forest_plots[[1]] + 
                           guides(shape = 'none') +
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.93, 0.07)) %>% 
    as_figure(label = 'figure_7_uni_cox_inference_numeric', 
              ref_name = 'uni_cox_inference_numeric', 
              caption = paste('Inference for univariable Cox models of', 
                              'relapse-free survival with numeric', 
                              'explanatory variables.'), 
              w = 180, 
              h = 230)
  
# Figure 8: PRL and survival -------
  
  insert_msg('Figure 8: prolactin and survival')
  
  ## top panel: significant factors in univariable Cox modeling
  
  figures$prl_survival$upper <- 
    c(uni_km$factors$plots[c("testosterone_replacement", "chemotherapy")], 
      uni_km$numeric$plots$first_order["PRL_sqrt"], 
      uni_km$numeric$plots$second_order["PRL_sqrt"])
  
  ## bottom panel cutoff for PRL
  
  figures$prl_survival$bottom <- 
    list(uni_cut$diagnostic_plots$PRL$significance + 
                labs(title = 'Screening for PRL cutoff', 
                     subtitle = 'Log-rank test', 
                     x = 'PRL, µU/mL', 
                     y = expression('-log'[10] * ' p')), 
              uni_cut$km_plots$PRL)
  
  ## the entire figure
  
  figures$prl_survival <- figures$prl_survival %>% 
    unlist(recursive = FALSE) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         '', '', 
                         'B', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_8_significant_prognostic_factors', 
              ref_name = 'prl_survival', 
              caption = paste('Significant prognostic factors', 
                              'identified by univariable Cox modeling', 
                              'of relapse-free survival.'), 
              w = 180, 
              h = 190)
  
# Figure 9: LASSO modeling -----  
  
  insert_msg('Figure 9: LASSO modeling')
  
  ## upper panel: coefficients 
  
  figures$cox_lasso$upper <- list(numeric = multi_cox, 
                                  strata = multi_cut) %>% 
    map(~.x$coef_plot) %>% 
    map(~.x + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'white', 
                               high = 'firebrick', 
                               limits = log(c(0.18, 9.5))) + 
          scale_radius(limits = abs(log(c(1, 9.5))), 
                       range = c(0.5, 4.5))) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    map2(., c('LASSO Cox, PRL', 'LASSO Cox, PRL strata'), 
         ~.x + labs(title = .y)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  ## middle panel: Brier scores
  
  figures$cox_lasso$middle <- list(numeric = multi_cox, 
                                   strata = multi_cut) %>% 
    map(~.x$brier_plot) %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 0.105))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(multi_cox$brier_plot + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  
  ## bottom panel: Kaplan-Meier plots
  
  figures$cox_lasso$bottom <- list(numeric = multi_cox, 
                                   strata = multi_cut) %>% 
    map(~.x$tertile_km) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  ## the entire figures
  
  figures$cox_lasso <- 
    plot_grid(figures$cox_lasso$upper, 
              figures$cox_lasso$middle, 
              figures$cox_lasso$bottom, 
              nrow = 3, 
              rel_heights = c(1.4, 1, 1),
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_9_multi_parameter_cox_modeling', 
              ref_name = 'cox_lasso', 
              caption = paste('Multi-paramater LASSO', 
                              'Cox modeling of effects of PRL concentration', 
                              'and PRL strata on relapse-free survival.'), 
              w = 180, 
              h = 240)
  
# Figure 10: correlation of sex hormone levels ------
  
  insert_msg('Figure 10: correlation of sex hormone levels')
  
  figures$corr <- corr$bubble_plot %>% 
    as_figure(label = 'figure_10_correlation_hormone_levels', 
              ref_name = 'corr', 
              caption = paste('Correlation of pre-surgery', 
                              'sex hormone levels.'), 
              w = 130, 
              h = 110)
  
# Figure 11: development of the clusters ------
  
  insert_msg('Figure 11: development of patient custers')
  
  
  
# Saving figures in the disc -------
  
  insert_msg('Disc save')
  
  figures %>% 
    walk(pickle, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './report/figures')
  
# END -----
  
  insert_tail()