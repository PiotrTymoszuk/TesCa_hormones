# Correlation of the pre-surgery sex horomone levels

  insert_head()
  
# container -------
  
  corr <- list()

# analysis globals --------
  
  insert_msg('Analysis globals')
  
  ## analysis variables and analyis table: no transformation is applied
  ## except for normalization with median-centering
  
  corr$variables <- c('E2', 'T_total', 'HCG', 'FSH', 'LH', 'PRL')
  
  ## analysis table: median centered variable values
  
  corr$analysis_tbl <- tesca$data %>% 
    select(ID, all_of(corr$variables)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
  ## variable pairs
  
  corr$pairs <- corr$variables %>% 
    combn(m = 2, simplify = FALSE)
  
# Serial correlation ------
  
  insert_msg('Serial correlation')
  
  corr$test <- corr$pairs %>%
    map_dfr(~correlate_variables(corr$analysis_tbl, 
                                 variables = .x, 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 ci = TRUE, 
                                 pub_styled = FALSE)) %>% 
    adjust_fdr('p_value', 'BH') %>% 
    mutate(significant = ifelse(p_adjusted < 0.05, 
                                'p < 0.05', 'ns'), 
           fontface = ifelse(significant == 'ns', 
                             'plain', 'bold'))
  
# Correlogram --------
  
  insert_msg('Correlogram')
  
  corr$bubble_plot <- corr$test %>% 
    ggplot(aes(x = variable1, 
               y = variable2, 
               fill = estimate, 
               size = abs(estimate))) + 
    geom_point(shape = 21) + 
    geom_text(aes(label = signif(estimate, 2), 
                  fontface = fontface, 
                  color = significant), 
              size = 2.4, 
              vjust = -1.3) + 
    scale_color_manual(values = c('gray60', 'black'), 
                       name = '') + 
    scale_fill_gradient2(low = 'steelblue', 
                         mid = 'white', 
                         high = 'firebrick', 
                         limits = c(-1, 1), 
                         midpoint = 0, 
                         name = expression(rho)) + 
    scale_radius(limits = c(0, 1), 
                 range = c(0.5, 4.5), 
                 name = expression('abs(' * rho * ')')) + 
    scale_x_discrete(limits = corr$variables, 
                     labels = exchange(corr$variables, 
                                       dict = tesca$lexicon)) + 
    scale_y_discrete(limits = corr$variables, 
                     labels = exchange(corr$variables, 
                                       dict = tesca$lexicon)) + 
    guides(size = 'none') + 
    globals$common_theme + 
    theme(axis.title = element_blank()) + 
    labs(title = 'Pre-surgery sex hormone levels',
         subtitle = paste('Spearman correlation, n =', 
                          nrow(corr$analysis_tbl)))
  
# END -----

  insert_tail()