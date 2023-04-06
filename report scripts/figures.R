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
  
# Figure 4: differences between histology types, cances features -------
    
  insert_msg('Figure 4: differences between histologies, cancer')
  
  figures$histology_cancer <- histology$plots
  
  figures$histology_cancer$IGCCCG_risk_group <- 
    figures$histology_cancer$IGCCCG_risk_group + 
    scale_fill_brewer(labels = c('good', 'int', 'poor'))
  
  figures$histology_cancer <- 
    c(list(figures$histology_cancer$age_surgery + 
             theme(legend.position = 'none')), 
      figures$histology_cancer[c("pt_stage", 
                                 "cs_lugano", 
                                 "IGCCCG_risk_group", 
                                 "lymphovas_invasion")] %>% 
        map(~.x + theme(legend.position = 'bottom')), 
      list(figures$histology_cancer$AFP + 
             scale_y_continuous(trans = 'log10') + 
             theme(legend.position = 'none')), 
      list(figures$histology_cancer$AFP_class + 
             theme(legend.position = 'bottom'))) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_4_histology_demogrpahy_cancer_features', 
              ref_name = 'histology_cancer', 
              caption = paste('Significant differences in demographic and', 
                              'cancer features between seminoma and', 
                              'mixed-histology tumors.'), 
              w = 180, 
              h = 220)
  
# Figure 5: differences in therapy between the histologies -----
  
  insert_msg('Figure 5: differences in therapy between histologies')
  
  figures$histology_therapy <- 
    histology$plots[c("RLA", "chemotherapy", "radiotherapy")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.title.x = element_blank())) %>%
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(histology$plots$chemotherapy + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15)) %>% 
    as_figure(label = 'figure_5_histology_therapy', 
              ref_name = 'histology_therapy',
              caption = paste('Differences in frequency of', 
                              'retroperitoneal lymphadenectomy', 
                              'chemo- and radiotherapy', 
                              'between seminoma and', 
                              'mixed-histology tumors.'), 
              w = 180, 
              h = 85)
  
# Figure 6 - 7: histology and sex hormones -------
  
  insert_msg('Figure 6 - 7: histology and hormones')
  
  ## figure 6: main sex hormones and HCG
  
  figures$histology_estro_testo_hcg <- 
    list(histology$plots$T_total + 
           theme(legend.position = 'none'), 
         histology$plots$T_total_class, 
         histology$plots$E2 + 
           theme(legend.position = 'none'), 
         histology$plots$E2_class, 
         histology$plots$HCG + 
           theme(legend.position = 'none') + 
           scale_y_continuous(trans = 'log10'), 
         histology$plots$HCG_class)
  
  ## Figure 7: FSH, LH and PRL
  
  figures$histology_fsh_ld_prl <- 
    list(histology$plots$FSH + 
           theme(legend.position = 'none') + 
           scale_y_continuous(trans = 'pseudo_log'), 
         histology$plots$FSH_class, 
         histology$plots$LH + 
           theme(legend.position = 'none') + 
           scale_y_continuous(trans = 'pseudo_log'), 
         histology$plots$LH_class, 
         histology$plots$PRL + 
           theme(legend.position = 'none') + 
           scale_y_continuous(trans = 'pseudo_log'), 
         histology$plots$PRL_class) 

  ## figures 
  
  figures[c('histology_estro_testo_hcg', 
            'histology_fsh_ld_prl')] <- 
    figures[c('histology_estro_testo_hcg', 
              'histology_fsh_ld_prl')] %>% 
    map(~map(.x, ~.x + theme(axis.title.x = element_blank()))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr')) %>% 
    list(x = ., 
         label = c('figure_6_histology_major_sex_hormones', 
                   'figure_7_histology_pituitary_sex_hormones'), 
         ref_names = c('histology_estro_testo_hcg', 
                       'histology_fsh_ld_prl'), 
         caption = c(paste('Differences in pre-surgery levels of testosterone,', 
                           'estradiol and human chorionic gonadotropin', 
                           'between seminoma and mixed-histology cancers.'), 
                     paste('Differences in pre-surgery levels of', 
                           'follicle-stimulating', 
                           'hormone, luteinizing hormone and prolactin', 
                           'between seminoma and mixed-histology cancers.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 220)
  
# Figure 8: signature of mixed-histology tumors --------
  
  insert_msg('Figure 8: signature of mixed-histology tumors')
  
  ## right panel: model performance
  
  figures$histology_classifier <- 
    plot_grid(histo_class$roc_plots$train + 
                theme(plot.tag = element_blank()), 
              histo_class$roc_plots$cv + 
                theme(plot.tag = element_blank()), 
              histo_class$brier_plots$sorted_score + 
                theme(legend.position = 'bottom'), 
              nrow = 3, 
              align = 'v', 
              axis = 'tblr') %>% 
    plot_grid(histo_class$importance_plot, ., 
              ncol = 2, 
              rel_widths = c(0.55, 0.45), 
              labels = LETTERS, 
              label_size = 10) %>%
    as_figure(label = 'figure_8_histology_random_forest_classifier', 
              ref_name = 'histology_class', 
              caption = paste('Multi-paramater clinical signature of', 
                              'mixed-histology cancers established by', 
                              'conditional Random Forest modeling.'), 
              w = 180, 
              h = 220)

# Figure 9: histology and survival -------
  
  insert_msg('Figure 9: histology and survival')
  
  figures$histology_survival <- histo_surv$plot %>% 
    as_figure(label = 'figure_9_histology_survival', 
              ref_name = 'histology_survial', 
              caption = paste('Relapse-free surcical in patients', 
                              'with seminoma and mixed-histology tumors.'),
              w = 120, 
              h = 75)
  
# Figure 10: pre-surgery hormone levels, PCA and MCA -------
  
  insert_msg('Figure 10: PCA and MCA')
  
  figures$hormone_components <- 
    list(pca$pca_plots$loadings, 
         hor_corresp$plots$column) %>% 
    map2(., c('PCA', 'MCA'), 
         ~.x + 
           geom_vline(xintercept = 0, 
                      linetype = 'dashed') +
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           labs(title = .y) + 
           theme(plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_10_components_hormones', 
              ref_name = 'hormone_components', 
              caption = paste('Component analysis of pre-surgery', 
                              'sex hormone levels.'), 
              w = 120,  
              h = 200)
  
# Figure 11: cancer subsets ------
  
  insert_msg('Figure 11: latent classes')
  
  figures$latent_classes <- 
    class_hormo$plots[c("T_total_class", "E2_class", "HCG_class", 
                        "FSH_class", "LH_class", "PRL_class")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_11_patient_subsets', 
              ref_name = 'latent_classes', 
              caption = paste('Subsets of testis cancer patients', 
                              'defined by latent class analysis', 
                              'in respect to pre-surgery levels', 
                              'of sex hormones.'), 
              w = 180, 
              h = 220)
  
# Figure 12: hormones in cancer subsets -------
  
  insert_msg('Figure 12: hormones of cancer subsets')
  
  figures$hormone_classes <- 
    plot_grid(class_hormo$ribbon_plot + 
                theme(legend.position = 'none', 
                      plot.margin = ggplot2::margin(r = 7, 
                                                    l = 7, 
                                                    unit = 'mm')), 
              get_legend(class_hormo$ribbon_plot), 
              ncol = 2, 
              rel_widths = c(1, 0.3)) %>% 
    as_figure(label = 'figure_12_patient_subsets_hormones', 
              ref_name = 'hormone_classes', 
              caption = paste('Pre-surgery blood concentrations of', 
                              'sex hormones in the subsets of testis cancer.'), 
              w = 180, 
              h = 150)
  
# Figure 13: subsets and demographic features ------
  
  insert_msg('Figure 13: subsets and demographics')
  
  figures$classes_demography <- 
    plot_grid(class_bcg$plots$age_surgery, 
              class_bcg$plots$bmi, 
              class_bcg$plots$body_mass_class, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_13_patient_subsets_demography', 
              ref_name = 'demography_classes', 
              caption = paste('Differences in age and body mass index', 
                              'between the subsets of testis cancer.'), 
              w = 180,
              h = 140)
  
# Figure 14: subsets, tumor size and pathology ------
  
  insert_msg('Figure 14: subsets and cancer pathology')
  
  figures$classes_pathology <-
    plot_grid(class_bcg$plots$max_size_cm, 
              ggdraw(), 
              class_bcg$plots$pt_stage, 
              class_bcg$plots$cs_lugano, 
              class_bcg$plots$lymphovas_invasion, 
              class_bcg$plots$histology, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_14_patient_subsets_pathology', 
              ref_name = 'classes_pathology', 
              caption = paste('Pathological cancer characteristic', 
                              'in the subsets of testis cancer.'), 
              w = 180, 
              h = 220)

# Figure 15: subsets, cancer markers -------
  
  insert_msg('Figure 15: subsets and cancer markers')
  
  figures$classes_markers <- 
    plot_grid(class_bcg$plots$AFP + 
                scale_y_continuous(trans = 'log10'), 
              class_bcg$plots$AFP_class, 
              class_bcg$plots$LDH + 
                scale_y_continuous(trans = 'log10'), 
              class_bcg$plots$LDH_class, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_15_patient_subsets_pathology', 
              ref_name = 'classes_markers', 
              caption = paste('Blood levels of biochemical cancer markers', 
                              'in the subsets of testis cancer.'), 
              w = 180, 
              h = 140)

# Figure 16: subsets and therapy -------
  
  insert_msg('Figure 16: subsets and cancer therapy')
  
  figures$classes_therapy <- 
    class_bcg$plots[c("RLA", "chemotherapy", "radiotherapy")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_16_patient_subsets_therapy', 
              ref_name = 'classes_therapy', 
              caption = paste('Therapy in the subsets of testis cancer.'), 
              w = 180, 
              h = 140)
  
# Figure 17: multi-parameter classifier ------
  
  insert_msg('Figure 17: subsets, Random Forest classifier')
  
  figures$classes_random_forest <- 
    class_rf$confusion_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>%
    c(list(get_legend(class_rf$confusion_plots[[1]] + 
                        theme(legend.position = 'bottom')))) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              rel_heights = c(1, 1, 0.5)) %>%
      plot_grid(class_rf$importance_plot, ., 
                ncol = 2, 
                rel_widths = c(0.6, 0.4), 
                labels = LETTERS, 
                label_size = 10) %>% 
    as_figure(label = 'figure_17_subsets_classifier', 
              ref_name = 'classes_random_forest', 
              caption = paste('Demographic and clinical signature', 
                              'of the subsets of testis cancer', 
                              'developed by conditional', 
                              'Random Forest modeling.'), 
              w = 180, 
              h = 180)
  
# Figure 18: subsets and survival ------
  
  insert_msg('Figure 18: subsets and survival')
  
  figures$classes_survival <- class_surv$plot %>% 
    as_figure(label = 'figure_18_subsets_survival', 
              ref_name = 'classes_survival', 
              caption = paste('Relapse-free surcical in the subsets', 
                              'of testis cancer.'),
              w = 120, 
              h = 75)
  
# Figure 19: sex hormones and survival --------
  
  insert_msg('Figure 19: sex hormones and survival')
  
  figures$uni_cox <- uni_cox$forest_plots %>% 
    map2(., c('Hormones, clinical strata', 
              'Hormones, first-order', 
              'Hormones, first/second-order'), 
         ~.x + 
           labs(title = .y) + 
           theme(legend.position = 'none'))
  
  for(i in names(figures$uni_cox)) {
    
    figures$uni_cox[[i]]$data <- 
      figures$uni_cox[[i]]$data %>% 
      filter(stri_detect(variable, 
                         regex = 'LH|FSH|E2|T_total|HCG|PRL'))
      
    
  }
  
  figures$uni_cox <- 
    list(figures$uni_cox$factors, 
         get_legend(uni_cox$forest_plots$factors + 
                      guides(shape = 'none')), 
         figures$uni_cox$first_order, 
         figures$uni_cox$second_order) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_19_univariate_cox', 
              ref_name = 'uni_cox', 
              caption = paste('Modeling of relapse-free survival as a function', 
                              'of pre-surgery sex hormone levels', 
                              'by uni-variable Cox regression.'), 
              w = 180, 
              h = 220)
    
# Figure 20: LASSO modeling -----  
  
  insert_msg('Figure 20: LASSO modeling')
  
  figures$multi_cox <- 
    plot_grid(multi_cox$brier_plot, 
              multi_cox$tertile_km, 
              nrow = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(multi_cox$coef_plot + 
                scale_fill_gradient2(low = 'steelblue', 
                                     mid = 'white', 
                                     high = 'firebrick', 
                                     limits = log(c(0.18, 9.5))) + 
                scale_radius(limits = abs(log(c(1, 9.5))), 
                             range = c(0.5, 4.5)) + 
                theme(legend.position = 'none'),
              ., 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_20_multi_parameter_cox_modeling', 
              ref_name = 'multi_cox', 
              caption = paste('Multi-paramater Elastic Net', 
                              'Cox modeling of effects of pre-surgery sex', 
                              'hormone levels on relapse-free survival.'), 
              w = 180, 
              h = 140)
  
# Saving figures in the disc -------
  
  insert_msg('Disc save')
  
  figures %>% 
    walk(pickle, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './report/figures')
  
# END -----
  
  rm(i)
  
  insert_tail()