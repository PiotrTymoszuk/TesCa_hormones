# Supplementary Figures -------

  insert_head()

# container ----

  tesca_supfigures <- list()
  
# Figure S1: inclusion scheme ------
  
  insert_msg('Figure S1: inclusion scheme')
  
  tesca_supfigures$inclusion <- 
    plot_grid(ggdraw() + 
                draw_image('./schemes/inclusion_tesca.png')) %>% 
    as_figure(label = 'figure_s1_analysis_scheme', 
              ref_name = 'inclusion_scheme', 
              caption = paste('Analysis inclusion scheme', 
                              'and data analysis strategy.'), 
              w = 180, 
              h = 180 * 2352/3966)
  
# Figure S2: histology, absolute marker levels ------
  
  insert_msg('Figure S2: absolute cancer marker levels, histology')
  
  tesca_supfigures$histo_markers <- histology$plots[c("AFP", "HCG", "LDH")] %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(trans = 'log10')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_s2_histology_marker_absolute', 
              ref_name = 'histo_markers', 
              caption = paste('Absolute serum concentrations', 
                              'of cancer markers in seminoma', 
                              'and non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 75)
  
# Figure S3: PCA and MCA ------
  
  insert_msg('Figure S3: PCA and MCA')
  
  tesca_supfigures$hormone_components <- 
    list(pca$pca_plots$loadings, 
         hor_corresp$plots$column) %>% 
    map2(., c('PCA', 'MCA'), 
         ~.x + 
           geom_vline(xintercept = 0, 
                      linetype = 'dashed', 
                      size = 0.25) +
           geom_hline(yintercept = 0, 
                      linetype = 'dashed', 
                      size = 0.25) + 
           labs(title = .y) + 
           theme(plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 12) %>% 
    as_figure(label = 'figure_s3_components_hormones', 
              ref_name = 'hormone_components', 
              caption = paste('Principal component and correcpondence analysis', 
                              'of pre-surgery sex hormone levels.'), 
              w = 140,  
              h = 200)
  
# Figure S4: development of the LCA model -----
  
  insert_msg('Development of the LCA model')
  
  tesca_supfigures$lca <- 
    c(lca$tuning["bic_plot"], 
      lca$cond_prob$plots) %>% 
    map(~.x + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', 'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s4_lca_development', 
              ref_name = 'lca', 
              caption = paste('Definition of the hormonal subsets', 
                              'with latent class analysis.'), 
              w = 180, 
              h = 160)
  
# Figure S5: therapy in the hormonal subsets ------
  
  insert_msg('Figure S5: therapy in the hormonal subsets')
  
  tesca_supfigures$subsets_therapy <- 
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
    as_figure(label = 'figure_s5_subsets_therapy', 
              ref_name = 'subsets_therapy', 
              caption = paste('Therapy in the hormonal subsets', 
                              'of the retrospective cohort.'), 
              w = 180, 
              h = 155)
  
# Figure S6: AFP/HCG status ------
  
  insert_msg('Figure S6: AFP/HCG status')

  ## tumor size
  
  tesca_supfigures$marker_cancer$size <- 
    class_mark$plots %>% 
    map(~.x$max_size_cm) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(0, 16)) + 
          theme(legend.position = 'none', 
                axis.text = element_text(size = 7),
                axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(ggdraw(), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## LDH
  
  tesca_supfigures$marker_cancer$ldh <- 
    class_mark$plots %>% 
    map(~.x$LDH) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(90, 3000), 
                             trans = 'log10') + 
          theme(legend.position = 'none', 
                axis.text.x = element_text(size = 7), 
                axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(ggdraw(), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## tumor stage, Lugano, lymphovascular invasion and histology
  
  tesca_supfigures$marker_cancer[c('stage', 'lugano', 'lvi', 'histology')] <- 
    map2(class_mark$plots$neutral[c("pt_stage", 
                                    "cs_lugano", 
                                    "lymphovas_invasion", 
                                    "histology")] %>% 
           map(~.x + theme(axis.title.x = element_blank())), 
         class_mark$plots$pituitary[c("pt_stage", 
                                      "cs_lugano", 
                                      "lymphovas_invasion", 
                                      "histology")] %>% 
           map(~.x + theme(axis.title.x = element_blank())), 
         factor_plot_pair)
  
  ## the entire figure
  
  tesca_supfigures$marker_cancer <- 
    plot_grid(tesca_supfigures$marker_cancer$size, 
              tesca_supfigures$marker_cancer$ldh, 
              tesca_supfigures$marker_cancer$stage, 
              tesca_supfigures$marker_cancer$lugano,
              tesca_supfigures$marker_cancer$lvi, 
              tesca_supfigures$marker_cancer$histology,
              ncol = 2,
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s6_marker_status_subsets_pathology', 
              ref_name = 'marker_cancer', 
              caption = paste('Differences in tumor pathology', 
                              'between neutral and pituitary hormonal subset', 
                              'participants split by the alpha fetoprotein', 
                              'and human chorionic gonadotropin status.'), 
              w = 190, 
              h = 230)
  
# Figure S7: hormonal subset classifier -----
  
  insert_msg('Figure S7: classifier')
  
  tesca_supfigures$classes_random_forest <- 
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
    as_figure(label = 'figure_s7_random_forest_classifier', 
              ref_name = 'classes_random_forest', 
              caption = paste('Demographic and clinical signature', 
                              'of the hormonal subsets of testicular cancer', 
                              'developed by conditional', 
                              'Random Forest modeling.'), 
              w = 180, 
              h = 180)
  
# Figure S8: expression of hormone genes -------
  
  insert_msg('Figure S8: expression of hormone genes, TCGA')
  
  tesca_supfigures$expression <-  
    tcga_exp$entire_plots[c("pituitary", "testicle")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_s8_tcga_expression', 
              ref_name = 'tcga_expression', 
              caption = paste('Expression of sex hormone-related genes', 
                              'in the malignant tissue', 
                              'in the TCGA testicle cancer cohort.'), 
              w = 180, 
              h = 110)
  
# Saving the figures on the disc ------
  
  insert_msg('Saving figures on teh disc')
  
  tesca_supfigures %>% 
    walk(pickle, 
         path = './tesca paper/supplementary figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()