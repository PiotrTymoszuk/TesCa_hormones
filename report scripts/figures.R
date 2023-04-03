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
  
# Saving figures in the disc -------
  
  insert_msg('Disc save')
  
  figures %>% 
    walk(pickle, 
         format = 'pdf', 
         device = cairo_pdf(), 
         path = './report/figures')
  
# END -----
  
  insert_tail()