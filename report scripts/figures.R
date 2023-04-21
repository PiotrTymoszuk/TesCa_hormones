# Figures for the analysis report

  insert_head()
  
# container -------
  
  figures <- list()
  
# Figure 1: inclusion scheme -------
  
  insert_msg('Figure 1: inclusion scheme')

  figures$inclusion <- 
    plot_grid(ggdraw() + 
                draw_image('./schemes/inclusion.png')) %>% 
    as_figure(label = 'figure_1_analysis_scheme', 
              ref_name = 'inclusion_scheme', 
              caption = paste('Analysis inclusion scheme', 
                              'and data analysis strategy.'), 
              w = 180, 
              h = 180 * 4272/3906)
    
# Figure 2: participant and variable missingness -------
  
  insert_msg('Figure 2: missingness')
  
  figures$missing <- 
    plot_grid(missing$patient$histogram, 
              nrow = 2, 
              rel_heights = c(0.4, 0.6)) %>% 
    plot_grid(missing$variable$plot, 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_2_missingness', 
              ref_name = 'missing', 
              caption = paste('Missing data per participant and variable.'), 
              w = 180, 
              h = 180)
  
# Figure 3: information content of the variables --------
  
  insert_msg('Figure 3: Gini coefficient')
  
  figures$gini <- plot_grid(gini$plot) %>% 
    as_figure(label = 'figure_3_information_content_gini', 
              ref_name = 'gini', 
              caption = paste('Informaiton content of the variables measured', 
                              'by Gini coefficient.'),
              w = 100, 
              h = 160)
  
# Figure 4: normality --------
  
  insert_msg('Figure 4: Normality')
  
  figures$normailty <- plot_grid(distr$normality$plot) %>% 
    as_figure(label = 'figure_4_normality', 
              ref_name = 'normality', 
              caption = paste('Normality of numeric study variables assessed', 
                              'by Shapiro-Wilk test.'), 
              w = 130, 
              h = 180)
  
# Figure 5: differences between histology types, cancer features -------
    
  insert_msg('Figure 5: differences between histologies, cancer')
  
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
        map(~.x + theme(legend.position = 'bottom'))) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_5_histology_demogrpahy_cancer_features', 
              ref_name = 'histology_cancer', 
              caption = paste('Significant differences in demographic and', 
                              'cancer features between seminoma and', 
                              'non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 155)
  
# Figure 6: histology and cancer markers --------
  
  insert_msg('Figure 6: histology and cancer markers')
  
  figures$histology_markers <- 
    c(list(histology$plots$AFP + 
             scale_y_continuous(trans = 'log10') + 
             theme(legend.position = 'none')), 
      list(histology$plots$AFP_class), 
      list(histology$plots$HCG + 
             theme(legend.position = 'none') + 
             scale_y_continuous(trans = 'log10')), 
      list(histology$plots$HCG_class), 
      list(histology$plots$LDH + 
             theme(legend.position = 'none') + 
             scale_y_continuous(trans = 'log10', 
                                limits = c(90, 3300))), 
      list(histology$plots$LDH_class)) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_6_histology_cancer markers', 
              ref_name = 'histology_markers', 
              caption = paste('Significant differences in blood', 
                              'concentrations of cancer markers', 
                              'between seminoma and', 
                              'non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 220)
  
# Figure 7: differences in therapy between the histologies -----
  
  insert_msg('Figure 7: differences in therapy between histologies')
  
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
    as_figure(label = 'figure_7_histology_therapy', 
              ref_name = 'histology_therapy',
              caption = paste('Differences in frequency of', 
                              'retroperitoneal lymphadenectomy', 
                              'chemo- and radiotherapy', 
                              'between seminoma and', 
                              'non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 85)
  
# Figure 8 - 9: histology and sex hormones -------
  
  insert_msg('Figure 8 - 9: histology and hormones')
  
  ## figure 8: main sex hormones
  
  figures$histology_estro_testo_hcg <- 
    list(histology$plots$T_total + 
           theme(legend.position = 'none'), 
         histology$plots$T_total_class, 
         histology$plots$E2 + 
           theme(legend.position = 'none'), 
         histology$plots$E2_class)
  
  ## Figure 9: FSH, LH and PRL
  
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
         label = c('figure_8_histology_major_sex_hormones', 
                   'figure_9_histology_pituitary_sex_hormones'), 
         ref_name = c('histology_estro_testo_hcg', 
                      'histology_fsh_ld_prl'), 
         caption = c(paste('Differences in pre-surgery levels of testosterone,', 
                           'and estradiol', 
                           'between seminoma and non-seminomatous', 
                           'germ cell tumors.'), 
                     paste('Differences in pre-surgery levels of', 
                           'follicle-stimulating', 
                           'hormone, luteinizing hormone and prolactin', 
                           'between seminoma and', 
                           'non-seminomatous germ cell tumors.')), 
         h = c(145, 220)) %>% 
    pmap(as_figure, 
         w = 180)
  
# Figure 10: signature of NSGCT tumors --------
  
  insert_msg('Figure 10: signature of NSGCT tumors')
  
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
    plot_grid(histo_class$importance_plot + 
                theme(plot.title.position = 'plot'), ., 
              ncol = 2, 
              rel_widths = c(0.55, 0.45), 
              labels = LETTERS, 
              label_size = 10) %>%
    as_figure(label = 'figure_10_histology_random_forest_classifier', 
              ref_name = 'histology_class', 
              caption = paste('Multi-paramater clinical signature of', 
                              'non-seminomatous germ cell tumors', 
                              'established by', 
                              'conditional Random Forest modeling.'), 
              w = 180, 
              h = 220)

# Figure 11: histology and survival -------
  
  insert_msg('Figure 11: histology and survival')
  
  figures$histology_survival <- histo_surv$plot %>% 
    as_figure(label = 'figure_11_histology_survival', 
              ref_name = 'histology_survival', 
              caption = paste('Relapse-free surcical in patients', 
                              'with seminoma and non-seminomatous', 
                              'germ cell tumors.'),
              w = 120, 
              h = 75)
  
# Figure 12: results of histology comparison -----
  
  insert_msg('Figure 12: results of histology comparison, summary')
  
  figures$histology_summary <- 
    plot_grid(ggdraw() + 
                draw_image('./schemes/results_histology.png')) %>% 
    as_figure(label = 'figure_12_histology_result_summary', 
              ref_name = 'histology_summary', 
              caption = paste('Key differences between seminoma and', 
                              'non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 180 * 1950/3858)
  
# Figure 13: pre-surgery hormone levels, PCA and MCA -------
  
  insert_msg('Figure 13: PCA and MCA')
  
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
              label_size = 12) %>% 
    as_figure(label = 'figure_13_components_hormones', 
              ref_name = 'hormone_components', 
              caption = paste('Component analysis of pre-surgery', 
                              'sex hormone levels.'), 
              w = 140,  
              h = 200)

# Figure 14: tuning of LC model ----
  
  insert_msg('Figure 14: tuning of the LC model')
  
  figures$lca_tuning <- lca$tuning$bic_plot %>% 
    as_figure(label = 'figure_14_lca_class_number', 
              ref_name = 'lca_tuning',
              caption = paste('Determination of the optimal', 
                              'number of hormonal subsets of testicle cancer', 
                              'with latent class analysis.'), 
              w = 110, 
              h = 80)
  
# Figure 15: cancer subsets ------
  
  insert_msg('Figure 15: latent classes')
  
  figures$latent_classes <- 
    class_hormo$plots[c("T_total_class", "E2_class", 
                        "FSH_class", "LH_class", "PRL_class")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_15_patient_subsets', 
              ref_name = 'latent_classes', 
              caption = paste('Hormonal subsets of testis cancer patients', 
                              'defined by latent class analysis', 
                              'in respect to pre-surgery strata', 
                              'of sex hormones.'), 
              w = 180, 
              h = 220)
  
# Figure 16: hormones in cancer subsets -------
  
  insert_msg('Figure 16: hormones of cancer subsets')
  
  figures$hormone_classes <- 
    plot_grid(class_hormo$ribbon_plot + 
                theme(legend.position = 'none', 
                      plot.margin = ggplot2::margin(r = 7, 
                                                    l = 7, 
                                                    unit = 'mm')), 
              get_legend(class_hormo$ribbon_plot), 
              ncol = 2, 
              rel_widths = c(1, 0.3)) %>% 
    as_figure(label = 'figure_16_patient_subsets_hormones', 
              ref_name = 'hormone_classes', 
              caption = paste('Pre-surgery blood concentrations of', 
                              'sex hormones in the hormonal subsets', 
                              'of testis cancer.'), 
              w = 180, 
              h = 150)
  
# Figure 17: subsets and demographic features ------
  
  insert_msg('Figure 17: subsets and demographics')
  
  figures$classes_demography <- 
    plot_grid(class_bcg$plots$age_surgery, 
              class_bcg$plots$bmi, 
              class_bcg$plots$body_mass_class, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_17_patient_subsets_demography', 
              ref_name = 'demography_classes', 
              caption = paste('Differences in age and body mass index', 
                              'between the hormonal subsets of testis cancer.'), 
              w = 180,
              h = 140)
  
# Figure 18: subsets, tumor size and pathology ------
  
  insert_msg('Figure 18: subsets and cancer pathology')
  
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
    as_figure(label = 'figure_18_patient_subsets_pathology', 
              ref_name = 'classes_pathology', 
              caption = paste('Pathological cancer characteristic', 
                              'in the hormonal subsets of testis cancer.'), 
              w = 180, 
              h = 220)

# Figure 19: subsets and histology -------
  
  insert_msg('Figure 19: subsets and histology')
  
  figures$classes_histology <- 
    class_bcg$plots[c("seminoma_percent", 
                      "teratoma_percent", 
                      "embryonal_percent", 
                      "yolk_sac_ca_percent")] %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    as_figure(label = 'figure_19_subsets_histology', 
              ref_name = 'classes_histology', 
              caption = paste('Percentages of seminoma, teratoma, embryonal', 
                              'and yolk sac cancer histologies in the', 
                              'hormonal subsets.'), 
              w = 180, 
              h = 140)
  
# Figure 20: subsets, cancer markers -------
  
  insert_msg('Figure 20: subsets and cancer markers')
  
  figures$classes_markers <- 
    plot_grid(class_bcg$plots$AFP + 
                scale_y_continuous(trans = 'log10'), 
              class_bcg$plots$AFP_class, 
              class_bcg$plots$HCG + 
                scale_y_continuous(trans = 'log10'), 
              class_bcg$plots$HCG_class, 
              class_bcg$plots$LDH + 
                scale_y_continuous(trans = 'log10', 
                                   limits = c(90, 3000)), 
              class_bcg$plots$LDH_class, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_20_patient_subsets_markers', 
              ref_name = 'classes_markers', 
              caption = paste('Blood levels of biochemical cancer markers', 
                              'in the hormonal subsets of testis cancer.'), 
              w = 180, 
              h = 220)
  
# Figure 21: subsets, free testosterone and SHBG -------
  
  insert_msg('Figure 21: free testosterone and SHBG')
  
  figures$classes_tfree_shbg <- class_bcg$plots[c("T_free", "SHBG")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_21_subsets_tfree_shbg', 
              ref_name = 'classes_tfree_shbg', 
              caption = paste('Differences in blood concentrations', 
                              'of free testosterone and sex hormone-binding', 
                              'globulin in the hormonal subsets.'),
              w = 140, 
              h = 75)

# Figure 22: subsets and therapy -------
  
  insert_msg('Figure 22: subsets and cancer therapy')
  
  figures$classes_therapy <- 
    class_bcg$plots[c("RLA", 
                      "chemotherapy", 
                      "radiotherapy", 
                      "testosterone_replacement")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(class_bcg$plots$chemotherapy))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_22_patient_subsets_therapy', 
              ref_name = 'classes_therapy', 
              caption = paste('Therapy in the hormonal subsets', 
                              'of testis cancer.'), 
              w = 180, 
              h = 155)
  
# Figure 23: multi-parameter classifier ------
  
  insert_msg('Figure 23: subsets, Random Forest classifier')
  
  figures$classes_random_forest <- 
    class_rf$confusion_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>%
    c(list(get_legend(class_rf$confusion_plots[[1]] + 
                        theme(legend.position = 'bottom')))) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              rel_heights = c(1, 1, 0.5)) %>%
      plot_grid(class_rf$importance_plot + 
                  theme(plot.title.position = 'plot'), ., 
                ncol = 2, 
                rel_widths = c(0.52, 0.48), 
                labels = LETTERS, 
                label_size = 10) %>% 
    as_figure(label = 'figure_23_subsets_classifier', 
              ref_name = 'classes_random_forest', 
              caption = paste('Demographic and clinical signature', 
                              'of the hormonal subsets of testis cancer', 
                              'developed by conditional', 
                              'Random Forest modeling.'), 
              w = 180, 
              h = 180)
  
# Figure 24: subsets and survival ------
  
  insert_msg('Figure 24: subsets and survival')
  
  figures$classes_survival <- class_surv$plot %>% 
    as_figure(label = 'figure_24_subsets_survival', 
              ref_name = 'classes_survival', 
              caption = paste('Relapse-free surcical in the hormonal subsets', 
                              'of testis cancer.'),
              w = 120, 
              h = 75)
  
# Figure 25: subsets and marker status ------
  
  insert_msg('Figure 25: subsets and marker status')
  
  figures$classes_marker_status <- 
    plot_grid(class_bcg$plots$marker_status) %>% 
    as_figure(label = 'figure_25_subsets_marker_status', 
              ref_name = 'classes_marker',
              caption = paste('Alpha fetoprotein and human chorionic', 
                              'gonadotropin marker status', 
                              'in the hormonal subsets.'), 
              w = 90, 
              h = 80)
  
# Figure 26: markers: tumor size, LDH concentrations, staging and histology -----
  
  insert_msg('Figure 26: marker status and cancer features')
  
  ## tumor size
  
  figures$marker_cancer$size <- 
    class_mark$plots %>% 
    map(~.x$max_size_cm) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(0, 16)) + 
          theme(legend.position = 'none', 
                axis.text = element_text(size = 7))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(ggdraw(), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## LDH
  
  figures$marker_cancer$ldh <- 
    class_mark$plots %>% 
    map(~.x$LDH) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(90, 3000), 
                             trans = 'log10') + 
          theme(legend.position = 'none', 
                axis.text.x = element_text(size = 7))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(ggdraw(), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## tumor stage, Lugano, lymphovascular invasion and histology
  
  figures$marker_cancer[c('stage', 'lugano', 'lvi', 'histology')] <- 
    map2(class_mark$plots$neutral[c("pt_stage", 
                                    "cs_lugano", 
                                    "lymphovas_invasion", 
                                    "histology")], 
         class_mark$plots$pituitary[c("pt_stage", 
                                    "cs_lugano", 
                                    "lymphovas_invasion", 
                                    "histology")], 
         factor_plot_pair)
  
  ## the entire figure
  
  figures$marker_cancer <- 
    plot_grid(figures$marker_cancer$size, 
              figures$marker_cancer$ldh, 
              figures$marker_cancer$stage, 
              figures$marker_cancer$lugano,
              figures$marker_cancer$lvi, 
              figures$marker_cancer$histology,
              ncol = 2,
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_26_marker_status_subsets_pathology', 
              ref_name = 'marker_cancer', 
              caption = paste('Significant differences in tumor pathology', 
                              'between neutral and pituitary hormonal subset', 
                              'participants split by the alpha fetoprotein', 
                              'and human chorionic gonadotropin status.'), 
              w = 190, 
              h = 240)
  
# Figure 27: markers and therapy -------
  
  insert_msg('Figure 27: markers and therapy')
  
  figures$marker_therapy <- 
    class_mark$plots %>% 
    map(~.x[c('RLA', 'chemotherapy', 
              'radiotherapy', 'testosterone_replacement')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', '', 
                         'C', '', 'D', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(class_mark$plots$neutral$chemotherapy), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_27_subsets_marker_therapy', 
              ref_name = 'marker_therapy', 
              caption = paste('Differences in cancer therapy', 
                              'between neutral and pituitary hormonal subset', 
                              'participants split by the alpha fetoprotein', 
                              'and human chorionic gonadotropin status.'), 
              w = 200, 
              h = 155)
  
# Figure 28: hormonal subsets, summary -----
  
  insert_msg('Figure 28: hormonal subsets, summary')
  
  figures$classes_summary <- 
    plot_grid(ggdraw() + 
                draw_image('./schemes/results_hormones.png')) %>% 
    as_figure(label = 'figure_28_subsets_summary', 
              ref_name = 'classes_summary',
              caption = paste('Key characteristic of the hormonal subsets', 
                              'of testicle cancer.'), 
              w = 180, 
              h = 180 * 2730/3936)
  
# Figure 29: sex hormones and survival --------
  
  insert_msg('Figure 29: sex hormones and survival')
  
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
    as_figure(label = 'figure_29_univariate_cox', 
              ref_name = 'uni_cox', 
              caption = paste('Modeling of relapse-free survival as a function', 
                              'of pre-surgery sex hormone levels', 
                              'by uni-variable Cox regression.'), 
              w = 180, 
              h = 220)
    
# Figure 30: Elastic Net survival modeling -----  
  
  insert_msg('Figure 30: Elastic Net survival modeling')
  
  figures$multi_cox <- 
    plot_grid(multi_cox$c_index_plot, 
              multi_cox$brier_plot, 
              multi_cox$tertile_km, 
              nrow = 3, 
              align = 'hv', 
              axis = 'tblr', 
              rel_heights = c(0.7, 1, 1)) %>% 
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
    as_figure(label = 'figure_30_multi_parameter_cox_modeling', 
              ref_name = 'multi_cox', 
              caption = paste('Multi-paramater Elastic Net', 
                              'Cox modeling of effects of pre-surgery sex', 
                              'hormone levels on relapse-free survival.'), 
              w = 180, 
              h = 150)
  
# Figure 31: Hormone-related genes in the TCGA cohort ------
  
  insert_msg('Figure 31: hormone-related genes in the TCGA cohort')
  
  figures$tcga_expression <- 
    tcga_exp$entire_plots[c("pituitary", "testicle")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_31_tcga_hormone_gene_expression', 
              ref_name = 'tcga_expression', 
              caption = paste('Expression of sex hormone-related genes', 
                              'in the TCGA testicle cancer cohort.'), 
              w = 180, 
              h = 110)
  
# Figure 32: gene expression and histology ------
  
  insert_msg('Figure 32: gene expression and histology, TCGA')
  
  figures$tcga_histology <- tcga_exp$plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr',
              labels = c('A', '', '', '', 
                         'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_32_tcga_histology', 
              ref_name = 'tcga_histology', 
              caption = paste('Expression of sex hormone-related', 
                              'genes in seminoma and NSGCT samples', 
                              'from the TCGA cohort.'), 
              w = 180, 
              h = 210)
  
# Figure 33: gene expression PCA -----
  
  insert_msg('Figure 33: TCGA, PCA')
  
  figures$tcga_pca <- 
    list(tcga_pca$plots$tcga$scree, 
         tcga_pca$plots$tcga$loadings + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_vline(xintercept = 0, 
                      linetype = 'dashed')) %>% 
    map(~.x + 
          labs(subtitle = paste('n =', nrow(tcga_pca$analysis_tbl$tcga))) + 
          theme(plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', 'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_33_tcga_pca', 
              ref_name = 'tcga_pca', 
              caption = paste('Principal component analysis of the', 
                              'sex hormone-related gene dataset in the', 
                              'TCGA cohort.'), 
              w = 180, 
              h = 90)
  
# Figure 34: mixture model ------
  
  insert_msg('Figure 34: mixture model, BIC and parameters')

  figures$tcga_gmm <- 
    plot_grid(tcga_mix$gmm_tuning$plots$bic_plot, 
              tcga_mix$mean_hm$plot + 
                theme(legend.position = 'bottom'), 
              ncol = 2, 
              rel_widths = c(0.45, 0.55), 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_34_tcga_mixture_model', 
              ref_name = 'tcga_gmm', 
              caption = paste('Development of the hormonal subsets of TCGA', 
                              'testis cancers by Gaussian mixture modeling.'), 
              w = 180, 
              h = 120)
    
# Figure 35: gene expression, class defining features ------
  
  insert_msg('Figure 35: expression of the class-defining features')

  figures$tcga_class_features <- 
    tcga_class$plots[c( "PRL", "FSHB", "LHB", "CGA", 
                       "HSD17B1", "CYP19A1", "HSD3B1", 
                       "HSD3B2", "CYP17A1", "HSD17B3")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.text.x = element_text(size = 7))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    as_figure(label = 'figure_35_class_defning_features',
              ref_name = 'tcga_class_features', 
              caption = paste('Expression of the sex hormone-related', 
                              'genes of interest in the hormonal', 
                              'subsets of the TCGA cohort.'), 
              w = 180, 
              h = 260)
  
# Figure 36: clinical features of the subsets ------
  
  insert_msg('Figure 36: clinical feature of the hormonal subsets')
  
  figures$tcga_clinic <- 
    tcga_bcg$plots[c("histology", "marker_status", "radiotherapy")] %>% 
    map(~.x + theme(legend.position = 'right')) %>% 
    c(list(tcga_bcg$plots$age_surgery  +
             theme(legend.position = 'none')), .) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_38_tcga_subsets_clinic', 
              ref_name = 'tcga_clinic', 
              caption = paste('Significant differences in demographic', 
                              'and clinical features between the hormonal', 
                              'subsets of the TCGA cohort.'), 
              w = 180, 
              h = 140)
  
# Figure 37: reactome pathways, heat map -------
  
  insert_msg('Figure 37: reactome pathways, heat map')
  
  figures$tcga_reactome_hm <- 
    tcga_biology$heat_map$plot %>% 
    as_figure(label = 'figure_37_tcga_subsets_reactome_pathways', 
              ref_name = 'tcga_reactome_hm', 
              caption = paste('Differences in Reactome pathways', 
                              'signatures between the hormonal subsets', 
                              'of the TCGA cohort.'), 
              w = 180, 
              h = 180)
  
# Figure 38: top Reactome pathways  -----
  
  insert_msg('Figure 38: top Reactome pathways')
  
  figures$tcga_top_reactome <- 
    tcga_biology$top_forests %>% 
    map(~.x + 
          theme(axis.text.y = element_text(size = 6), 
                legend.position = 'none', 
                plot.subtitle = element_blank(), 
                plot.title.position = 'plot', 
                plot.title = element_text(hjust = 0.3))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_38_top_reactome_pathways', 
              ref_name = 'tcga_top_reactome', 
              caption = paste('Hallmark Reactome pathways of the', 
                              'hormonal subsets of the TCGA cohort.'), 
              w = 180, 
              h = 280)
  
# Figure 39: signatures of stroma and immunogenicity -------
  
  insert_msg('Figure 39: scores of stroma and immunogenicity')
  
  figures$tcga_imm_scores <- 
    tcga_xcell$plots[c("immune.score", 
                       "stroma.score", 
                       "microenvironment.score")] %>% 
    c(tcga_quantiseq$plots["uncharacterized.cell"] %>% 
        map(~.x + labs(title = 'Non-immune cells')), .) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_39_stroma_signatures', 
              ref_name = 'tcga_imm_score', 
              caption = paste('Differences in predicted tumor cell', 
                              'content and signatures of immunity and stroma', 
                              'between the hormonal subsets of the TCGA', 
                              'cohort.'), 
              w = 180, 
              h = 140)
  
# Figure 40: B and T cell infiltration ------
  
  insert_msg('Figure 40: B and T cell infiltration')
  
  figures$tcga_lympho <- 
    list(tcga_quantiseq$plots$B.cell, 
         tcga_xcell$plots$B.cell, 
         tcga_quantiseq$plots$T.cell.regulatory..Tregs., 
         tcga_xcell$plots$T.cell.regulatory..Tregs., 
         tcga_quantiseq$plots$T.cell.CD8., 
         tcga_quantiseq$plots$T.cell.CD8., 
         tcga_xcell$plots$T.cell.CD8..effector.memory, 
         tcga_xcell$plots$T.cell.CD8..central.memory) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         'B', '', 
                         'C', '', 
                         'D', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_40_tcga_subset_lymphocytes', 
              ref_name = 'tcga_lympho',
              caption = paste('Differences in predicted B and T lymphocyte', 
                              'infiltration between the hormonal subsets of', 
                              'the TCGA cohort.'), 
              w = 180, 
              h = 280)
  
# Figure 41: TCGA subsets, mast cells, endothelial cells and fibroblasts ------
  
  insert_msg('Figure 41: mast cells, EC and fibroblasts')
  
  figures$tcga_mc_ec_fibro <- 
    tcga_xcell$plots[c("Mast.cell", 
                       "Endothelial.cell", 
                       "Cancer.associated.fibroblast")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_41_tcga_mast_ec_fibroblast', 
              ref_name = 'tcga_mc_ec_fibro', 
              caption = paste('Differences in predicted infiltration', 
                              'of mast cells, endothelial cells', 
                              'and fibroblasts between the hormonal subsets', 
                              'of the TCGA cohort.'), 
              w = 180, 
              h = 140)
  
# Figure 42: TCGA subsets, differential gene expression, gene numbers ----- 
  
  insert_msg('Figure 42: differential gene expression, gene numbers')

  figures$tcga_n_dge <- tcga_dplots$gene_n$plot %>% 
    as_figure(label = 'figure_42_tcga_number_differential_expressed', 
              ref_name = 'tcga_n_dge', 
              caption = paste('Percentages of the transcriptome differentially', 
                              'regulated between the hormonal subsets of the', 
                              'TCGA cohort.'), 
              w = 140, 
              h = 90)
  
# Figure 43: TCGA subsets, differential gene expression ------
  
  insert_msg('Figure 43: differential gene expression, Volcano')
  
  figures$tcga_volcano_dge <- tcga_dplots$volcano %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(tcga_dplots$volcano[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_43_tcga_dge_volcano', 
              ref_name = 'tcga_volcano_dge', 
              caption = paste('Significance and regulation of gene expression', 
                              'between the hormonal subsets of the TCGA cohort.'), 
              w = 180, 
              h = 200)
  
# Figure 44: TCGA subsets, top regulated genes -------
  
  insert_msg('Figure 44: top regulated genes')
  
  figures$tcga_top_dge <- tcga_dplots$top_genes %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.text.y = element_text(size = 7), 
                plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_44_tcga_top_regulated_genes', 
              ref_name = 'tcga_top_dge', 
              caption = paste('The genes stongest differentially', 
                              'regulated between the hormonal subsets of the', 
                              'TCGA cohort.'), 
              w = 180, 
              h = 230)
  
# Figure 45: TCGA subsets, signaling ------
  
  insert_msg('Figure 45: TCGA subsets, signaling')
  
  figures$tcga_signaling <- 
    plot_grid(tcga_splots$bubble$plot + 
                guides(size = 'none') + 
                theme(legend.position = 'bottom')) %>% 
    as_figure(label = 'figure_45_tcga_signaling', 
              ref_name = 'tcga_signaling', 
              caption = paste('Predicted differential modulation', 
                              'of signaling pathway activity in', 
                              'the hormonal subsets of the TCGA cohort.'), 
              w = 180, 
              h = 210)
  
# Figure 46: TCGA subsets and protein expression -------
  
  insert_msg('Figure 46: TCGA subsets and protein expression')
  
  figures$tcga_protein_hm <- 
    plot_grid(tcga_protein$heat_map + 
                theme(axis.text.y = element_text(size = 6))) %>% 
    as_figure(label = 'figure_46_tcga_protein_heat_map', 
              ref_name = 'tcga_protein_hm', 
              caption = paste0('Differential protein expression in the', 
                               'hormonal subsets of the TCGA cohort.'), 
              w = 180, 
              h = 230)
  
# Figure 47: TCGA subsets, top proteins ------
  
  insert_msg('Figure 47: TCGA subsets, top proteins')
  
  figures$tcga_top_proteins <- 
    tcga_protein$plots[c("CHEK2|Chk2", "KIT|c-Kit", 
                         "FN1|Fibronectin", "ERBB2|HER2", 
                         "XRCC5|Ku80", "MTOR|mTOR")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_47_tcga_top_proteins', 
              ref_name = 'tcga_top_proteins', 
              caption = paste('The proteins strongest regulated between', 
                              'the hormonal subsets of the TCGA cohort.'), 
              w = 180, 
              h = 210)
  
# Figure 48: TCGA, hormone genes and relapse ------
  
  insert_msg('Figure 48: hormone genes and relapse')
  
  figures$tcga_gene_survival <- 
    tcga_relapse$cutpoints$plots[c("PRL", "CGA", 
                                   "HSD3B1", "HSD3B2", 
                                   "HSD17B1")] %>% 
    map(~.x + theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_48_tcga_hormone_genes_relapse', 
              ref_name = 'tcga_gene_survival', 
              caption = paste('Significant and near significant', 
                              'effects of sex hormone-related gene expression', 
                              'on relapse-free survival in the TCGA', 
                              'testicle cancer cohort.'), 
              w = 180, 
              h = 230)
  
# Figure 49: TCGA subsets and relapse-free survival -----
  
  insert_msg('Figure 49: relapse-free survival in the hormone subsets, TCGA')
  
  figures$tcga_subsets_survival <- 
    plot_grid(tcga_relapse$classes$plot) %>% 
    as_figure(label = 'figure_49_tcga_subsets_relapse', 
              ref_name = 'tcga_subsets_survival', 
              caption = paste('Differences in relapse-free survival', 
                              'between the hormonal subsets of', 
                              'the TCGA cohort.'), 
              w = 140, 
              h = 95)
  
# Figure 50: Elastic Net modeling, coefficients ------
  
  insert_msg('Figure 50: Elastic Net coefficients')
  
  figures$tcga_elnet_coeffs <- 
    c(tcga_cox$coef_plots["clinical"], 
      list(ggdraw()), 
      tcga_cox$coef_plots[c("genes", "class")]) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_50_elastic_net_survival_coeffs', 
              ref_name = 'tcga_elnet_coeffs', 
              caption = paste('Development of multi-parameter', 
                              'Elastic Net Cox models of relapse-free', 
                              'survival in the TCGA cohort.'), 
              w = 180, 
              h = 220)
  
# Figure 51: performance of the Elastic Net models ------
  
  insert_msg('Figure 51: performance of the Elastic Net models')
  
  ## C-indexes and integrated Brier scores
  
  figures$tcga_elanet_performance <- tcga_cox$stat_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(tcga_cox$stat_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_51_elastic_net_survival_performance', 
              ref_name = 'tcga_elanet_performance', 
              caption = paste('Performance of the multi-parameter', 
                              'Elastic Net models at predicting', 
                              'relapse-free survival in the TCGA cohort.'), 
              w = 180, 
              h = 110)
  
# Figure 52: calibration of the Elastic Net models ------
  
  insert_msg('Figure 52: calibration, Elastic Net models')
  
  figures$tcga_elanet_calibration <- 
    tcga_cox$brier_plots[c("clinical", "genes", "class")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(tcga_cox$brier_plots[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_52_elastic_net_calibration', 
              ref_name = 'tcga_elanet_calibration', 
              caption = paste('Calibration of the multi-parameter', 
                              'Elastic Net models of', 
                              'relapse-free survival in the TCGA cohort.'), 
              w = 180, 
              h = 150)
  
# Figure 53: Elastic Net tertile scores ------
  
  insert_msg('Figure 53: Elastic Net linear predictor score tertiles')
  
  figures$tcga_elanet_tertiles <- 
    tcga_cox$tertiles_plots[c("clinical", "genes", "class")] %>% 
    map(~.x + theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>%
    as_figure(label = 'figure_53_elastic_net_tertiles', 
              ref_name = 'tcga_elanet_tertiles', 
              caption = paste('Relapse-free survival in tertiles', 
                              'of linear predictor scores of the', 
                              'Elastic Net models.'), 
              w = 180, 
              h = 170)
  
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