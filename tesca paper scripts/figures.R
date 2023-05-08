# Figures for the Tesca Manuscript

  insert_head()
  
# container -----
  
  tes_figures <- list()
  
# Figure 1: differences between the histologies -----
  
  insert_msg('Figure 1: differences between the histologies')
  
  tes_figures$histology <- histology$plots
  
  tes_figures$histology$IGCCCG_risk_group <- 
    tes_figures$histology$IGCCCG_risk_group + 
    scale_fill_brewer(labels = c('good', 'int', 'poor'))
  
  tes_figures$histology <- 
    c(list(tes_figures$histology$age_surgery + 
             theme(legend.position = 'none')), 
      tes_figures$histology[c("pt_stage", 
                              "cs_lugano", 
                              "IGCCCG_risk_group", 
                              "lymphovas_invasion")] %>% 
        map(~.x + theme(legend.position = 'bottom')), 
      tes_figures$histology[c("AFP_class", "HCG_class", "LDH_class")] %>% 
        map(~.x + theme(legend.position = 'bottom')), 
      list(histo_surv$plot + 
             labs(title = 'Survival') + 
             theme(legend.position = 'bottom'))) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_1_histology', 
              ref_name = 'histology', 
              caption = paste('Onset age, staging,', 
                              'invasiveness, serum cancer markers,',
                              'and relapse-free survival in seminoma and', 
                              'non-seminomatous germ cell tumors.'), 
              w = 180, 
              h = 230)
  
# Figure 2 - 3: sex hormones and histology -------
  
  insert_msg('Figure 2 - 3: sex hormones and histology')
  
  tes_figures[c('histo_gonadal', 'histo_pituitary')] <- 
    list(list(histology$plots$T_total + 
                theme(legend.position = 'none'), 
              histology$plots$T_total_class, 
              histology$plots$E2 + 
                theme(legend.position = 'none'), 
              histology$plots$E2_class), 
         list(histology$plots$FSH + 
                scale_y_continuous(trans = 'pseudo_log') + 
                theme(legend.position = 'none'), 
              histology$plots$FSH_class, 
              histology$plots$LH + 
                scale_y_continuous(trans = 'pseudo_log') + 
                theme(legend.position = 'none'), 
              histology$plots$LH_class, 
              histology$plots$PRL + 
                scale_y_continuous(trans = 'pseudo_log') + 
                theme(legend.position = 'none'), 
              histology$plots$PRL_class)) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv',
                   axis = 'tblr')) %>% 
    list(x = ., 
         label = c('figure_2_histology_gonadal', 
                   'figure_3_histology_pituitary'), 
         ref_name = c('histo_gonadal', 'histo_pituitary'), 
         caption = c(paste('Differences in preoperative serum levels of', 
                           'gonadal sex hormones between seminoma and', 
                           'non-seminomatous germ cell tumors.'), 
                     paste('Differences in preoperative serum levels of', 
                           'pituitary sex hormones between seminoma and', 
                           'non-seminomatous germ cell tumors.')), 
         h = c(140, 210)) %>% 
    pmap(as_figure, 
         w = 180)
  
# Figure 4: hormones in the hormonal subsets ------
  
  insert_msg('Figure 4: hormones in the hormonal subsets')
  
  tes_figures$subsets_hormones <-  
    class_hormo$plots[c("T_total_class", "E2_class", 
                        "FSH_class", "LH_class", "PRL_class")] %>% 
    map(~.x + 
          theme(axis.title = element_blank(), 
                legend.position = 'right')) %>% 
    c(list(class_hormo$ribbon_plot + 
             scale_y_discrete(labels = class_hormo$ribbon_short_labs) + 
             labs(title = 'Serum sex hormone levels') + 
             theme(legend.position = 'right', 
                   axis.text.x = element_text(size = 8)))) %>% 
    map(~.x + theme(legend.text = element_text(size = 8))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              labels = c('A', '', 
                         '', '', 
                         '', 'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_4_hormonal_subsets_hormones', 
              ref_name = 'subsets_hormones', 
              caption = paste('Pre-operative levels of sex hormones', 
                               'in the hormonal subsets of', 
                               'the retrospective cohort.'), 
              w = 180,
              h = 210)
  
# Figure 5: age, BMI, BMI class, staging and histology in the hormonal subsets -----
  
  insert_msg('Figure 5: clinical and demographic features of the hormonal subsets')
  
  tes_figures$subsets_clinics <- 
    c(class_bcg$plots[c("age_surgery", "bmi")], 
      class_bcg$plots["max_size_cm"], 
      class_bcg$plots[c("pt_stage", "cs_lugano", 
                        "histology", "lymphovas_invasion")] %>% 
        map(~.x + theme(legend.position = 'bottom')), 
      class_surv["plot"] %>% 
        map(~.x + 
              labs(title = 'Survival') + 
              guides(color = guide_legend(ncol = 2)) + 
              theme(legend.position = 'bottom'))) %>% 
    map(~.x + 
          theme(axis.title.x = element_blank(), 
                legend.title = element_blank(), 
                legend.text = element_text(size = 8))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_5_hormonal_subsets_demo_clinics', 
              ref_name = 'subsets_clinics', 
              caption = paste('Differences in onset age, body mass,', 
                              'staging, histology, invasiveness', 
                              'and relapse-free survival between', 
                              'the hormonal subsets of the', 
                              'retrospective cohort.'), 
              w = 180, 
              h = 230)
  
# Figure 6: cancer markers ---------
  
  insert_msg('Figure 6: cancer markers')
  
  tes_figures$subsets_markers <- 
    list(numeric = class_bcg$plots[c("AFP", "HCG", "LDH")] %>% 
           map(~.x + scale_y_continuous(trans = 'log10')), 
         factor = class_bcg$plots[c("AFP_class", "HCG_class", "LDH_class")] %>% 
           set_names(c('AFP', 'HCG', 'LDH'))) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_6_hormonal_subsets_markers', 
              ref_name = 'subsets_markers', 
              caption = paste('Serum concentratons of cancer markers', 
                              'in the homrmonal subsets'), 
              w = 180, 
              h = 210)
  
# Figure 7: TCGA, histology -------
  
  insert_msg('TCGA, histology')
  
  tes_figures$tcga_histology <- 
    c(tcga_exp$plots[c("GNRH1", "GNRH2", "PRL", "CGA", 
                       "FSHB", "LHB")], 
      list(ggdraw(), ggdraw()), 
      tcga_exp$plots[c("CYP11A1", "CYP17A1", "HSD17B3", "HSD3B1", 
                       "HSD3B2", "CYP19A1", "HSD17B1", "SHBG")]) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr',
              labels = c('A', '', '', '', 
                         '', '', '', '', 
                         'B', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_7_tcga_histology', 
              ref_name = 'tcga_histology', 
              caption = paste('Expression of sex hormone-related', 
                              'genes in the malignant tissue of', 
                              'seminoma and NSGCT samples', 
                              'from the TCGA cohort.'), 
              w = 180, 
              h = 230)

# Saving the figures on the disc -------
  
  insert_msg('Saving the figures')
  
  tes_figures %>% 
    walk(pickle, 
         path = './tesca paper/figures', 
         device = cairo_pdf)
  
# END -------
  
  insert_tail()