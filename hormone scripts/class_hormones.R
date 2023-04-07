# Characteristic of the latent classes: hormone strata and hormone levels

  insert_head()
  
# container -------
  
  class_hormo <- list()
  
# analysis globals ----
  
  insert_msg('Analysis globals')
  
  class_hormo$assignment <- lca$assingment
  
  ## analysis tables
  
  class_hormo$analysis_tbl <- 
    list(class_hormo$assignment, 
         hor_globals$numeric_data, 
         hor_globals$factor_data) %>% 
    reduce(left_join, by = 'ID') %>% 
    map_dfc(unname)
  
  ## variables 
  
  class_hormo$variables <- 
    c(hor_globals$numeric_variables, 
      hor_globals$factor_variables)
  
  ## variable lexicon
  
  class_hormo$lexicon <- tesca$lexicon %>% 
    filter(variable %in% class_hormo$variables) %>% 
    mutate(test_type = ifelse(format == 'factor', 
                              'cramer_v', 'kruskal_eta'), 
           plot_type = ifelse(format == 'factor', 
                              'stack', 'violin'), 
           axis_label = ifelse(format == 'factor', 
                               '% of subset', axis_label))
  
  ## n numbers 
  
  class_hormo$n_numbers <- class_hormo$analysis_tbl %>% 
    count(class)
  
  class_hormo$n_numbers <- 
    map2_chr(class_hormo$n_numbers[[1]], 
             class_hormo$n_numbers[[2]], 
             paste, sep = ': n = ') %>% 
    set_names(levels(class_hormo$analysis_tbl$class))
  
# Descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  class_hormo$stats <- class_hormo$analysis_tbl %>% 
    explore(variable = class_hormo$variables, 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(class_hormo$assignment$class)))
  
# Testing for differences between the latent classes ------
  
  insert_msg('Differences between the latent classes')
  
  class_hormo$test <- class_hormo$analysis_tbl %>% 
    compare_variables(variables = class_hormo$lexicon$variable, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = class_hormo$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Plots -----
  
  insert_msg('Plots')
  
  class_hormo$plots <- 
    list(variable = class_hormo$lexicon$variable, 
         plot_title = class_hormo$lexicon$label, 
         plot_subtitle = class_hormo$test$plot_cap, 
         y_lab = class_hormo$lexicon$axis_label, 
         type = class_hormo$lexicon$plot_type) %>% 
    pmap(plot_variable, 
         split_factor = 'class', 
         class_hormo$analysis_tbl, 
         x_lab = 'Hormonal subset', 
         cust_theme = globals$common_theme, 
         scale = 'percent', 
         x_n_labs = TRUE) %>% 
    set_names(class_hormo$lexicon$variable)
  
  ## adjustment of the numeric variable plots
  
  class_hormo$plots[hor_globals$numeric_variables] <- 
    class_hormo$plots[hor_globals$numeric_variables] %>% 
    map(~.x + 
          scale_fill_manual(values = hor_globals$class_colors))
  
  ## factor variables
  
  class_hormo$plots[hor_globals$factor_variables] <- 
    class_hormo$plots[hor_globals$factor_variables] %>% 
    map(~.x + 
          scale_fill_brewer())
  
# Ribbon plot of normalized hormone levels ------
  
  insert_msg('Ribbon plot')
  
  ## hormone labels with  effect sizes and p values

  class_hormo$ribbon_labs <- class_hormo$test %>% 
    mutate(plot_cap = paste(exchange(variable, 
                                     dict = tesca$lexicon), 
                            plot_cap, sep = '\n'))
  
  class_hormo$ribbon_labs <- 
    set_names(class_hormo$ribbon_labs$plot_cap, 
              class_hormo$ribbon_labs$variable)
  
  ## plot

  class_hormo$ribbon_plot <- class_hormo$analysis_tbl %>% 
    select(class, all_of(hor_globals$numeric_variables)) %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x) %>% 
    draw_stat_panel(variables = hor_globals$numeric_variables, 
                    split_factor = 'class', 
                    stat = 'mean', 
                    err_stat = '2se', 
                    form = 'line', 
                    plot_title = 'Sex hormone levels', 
                    cust_theme = globals$common_theme) + 
    scale_fill_manual(values = hor_globals$class_colors, 
                      labels = class_hormo$n_numbers) + 
    scale_color_manual(values = hor_globals$class_colors, 
                       labels = class_hormo$n_numbers) + 
    scale_y_discrete(limits = hor_globals$numeric_variables, 
                     labels = class_hormo$ribbon_labs) + 
    coord_polar(theta = 'y', 
                clip = 'off') + 
    theme(legend.title = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.y = element_blank(), 
          axis.line = element_blank()) + 
    labs(subtitle = 'Mean Z-score, 2 \u00D7 SEM')
  
  ## annotation 
  
  for(i in seq(-0.5, 1.5, by = 0.5)) {
    
    class_hormo$ribbon_plot <- class_hormo$ribbon_plot + 
      annotate('text', 
               label = i, 
               x = i, 
               y = 0.5, 
               size = 2.5, 
               color = 'gray60')
    
  }
  
# END ------
  
  rm(i)
  
  insert_tail()