# Latent class analysis for hormone level strata dataset

  insert_head()

# container ------

  lca <- list()

# Analysis globals -------

  insert_msg('Analysis globals')
  
  ## analysis data
  
  lca$analysis_tbl <- hor_globals$factor_data %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID')

  
  ## analysis formula
  
  lca$formula <- 
    cbind(E2_class, T_total_class, FSH_class, LH_class, PRL_class) ~ 1
  
# Tuning ------
  
  insert_msg('Model tuning')
  
  ## class range
  
  lca$tuning$range <- 1:8
  
  ## comparing model fit stats for varying class numbers
  
  set.seed(1234)
  
  lca$tuning$models <- 
    list(nclass = lca$tuning$range) %>% 
    pmap(poLCA, 
         formula = lca$formula, 
         data = lca$analysis_tbl, 
         maxiter = 5000, 
         nrep = 5) %>% 
    set_names(paste0('class_', lca$tuning$range))
  
  ## fit stats
  
  lca$tuning$stats <- lca$tuning$models %>% 
    map(~.x[c('aic', 'bic', 'Gsq', 'Chisq', 'llik')]) %>% 
    map(as_tibble) %>% 
    compress(names_to = 'model') %>% 
    mutate(class_number = stri_extract(model, regex = '\\d+$'), 
           class_number = as.numeric(class_number))
  
  ## plotting BIC as a function of class number
  
  lca$tuning[c('aic_plot', 
               'bic_plot', 
               'Gsq_plot', 
               'Chisq_plot', 
               'llik_plot')] <- 
    list(x = c('aic', 'bic', 'Gsq', 'Chisq', 'llik'), 
         y = c('Akaike Information Criterion', 
               'Bayesian Information Criterion', 
               'Likelihood ratio', 
               '\u03C7\u00B2', 
               'log-likelihood'), 
         z = c('AIC', 'BIC', 'Likelihood ratio', 
               '\u03C7\u00B2', 'log-likelihood')) %>% 
    pmap(function(x, y, z) lca$tuning$stats %>% 
           ggplot(aes(x = class_number, 
                      y = .data[[x]], 
                      group = 'a')) + 
           geom_vline(xintercept = 3, 
                      linetype = 'dashed', 
                      color = 'coral3') + 
           geom_path(color = 'steelblue') + 
           geom_point(shape = 16, 
                      color = 'steelblue') + 
           scale_x_continuous(breaks = lca$tuning$range) + 
           globals$common_theme + 
           labs(title = y, 
                subtitle = 'MLE-driven latent class analysis', 
                x = 'Class number, k', 
                y = z))
  
# final model -------
  
  insert_msg('Final model')
  
  lca$model <- lca$tuning$models$class_3
  
  ## posterior p for class assignment of the observations
  
  lca$posterior <- lca$model$posterior %>% 
    set_rownames(rownames(lca$analysis_tbl)) %>% 
    set_colnames(c('#1', '#2', '#3'))
  
  ## class assignment of the observation by simple voting
  
  lca$assingment <- nodal_vote(lca$posterior)
  
  ## renaming after the type of deregulated hormone family
  
  lca$assingment <- lca$assingment %>% 
    mutate(class = car::recode(class, 
                               "'#1' = 'pituitary'; 
                               '#2' = 'neutral'; 
                               '#3' = 'testicle'"), 
           class = factor(class, c('neutral', 'testicle', 'pituitary')))
  
# Plots of the conditional probabilities ---------
  
  insert_msg('Plots of the conditional probabilities')
  
  ## plotting data
  
  lca$cond_prob$plot_data <- lca$model$probs %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'class') %>% 
    map(mutate, 
        class = ifelse(class == 'class 1: ', 
                       'pituitary', 
                       ifelse(class == 'class 2: ', 
                              'neutral', 'testicle')), 
        class = factor(class, c('neutral', 'testicle', 'pituitary'))) %>% 
    map(~pivot_longer(.x, 
                      cols = names(.x)[names(.x) != 'class'], 
                      names_to = 'strata', 
                      values_to = 'p'))
  
  lca$cond_prob$plot_data <- 
    map2(lca$cond_prob$plot_data, 
         map(lca$analysis_tbl[names(lca$cond_prob$plot_data)], 
             levels), 
         ~mutate(.x, 
                 strata = factor(strata, .y), 
                 strata = as.numeric(strata)))
  
  lca$cond_prob$plot_data[c("E2_class", 
                            "T_total_class", 
                            "FSH_class", 
                            "LH_class")] <- 
    lca$cond_prob$plot_data[c("E2_class", 
                              "T_total_class", 
                              "FSH_class", 
                              "LH_class")] %>% 
    map(mutate, 
        strata = car::recode(strata, 
                             "1 = 'low'; 
                             2 = 'normal'; 
                             3 = 'high'"), 
        strata = factor(strata, c('low', 'normal', 'high')))
  
  lca$cond_prob$plot_data$PRL_class <- 
    lca$cond_prob$plot_data$PRL_class %>% 
    mutate(strata = car::recode(strata, 
                                "1 = 'normal'; 
                                2 = 'high'"), 
           strata = factor(strata, c('low', 'normal', 'high')))
  
  lca$cond_prob$plot_data <- lca$cond_prob$plot_data %>% 
    compress(names_to = 'variable') %>% 
    blast(class)
  
  ## numbers and percentages of observations in the subsets
  
  lca$cond_prob$n_numbers <- lca$assingment %>% 
    count(class) %>% 
    mutate(percent = n/sum(n) * 100)
  
  lca$cond_prob$plot_cap <- 
    map2(lca$cond_prob$n_numbers$n, 
         lca$cond_prob$n_numbers$percent, 
         ~paste0('n = ', .x, ', ', signif(.y, 2), '% of observations'))
  
  ## heat maps
  
  lca$cond_prob$plots <- 
    list(x = lca$cond_prob$plot_data, 
         y = c('Neutral subset', 'Testicle subset', 'Pituitary subset'), 
         z = lca$cond_prob$plot_cap) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = strata, 
                      y = variable, 
                      fill = p)) + 
           geom_tile(color = 'black') + 
           geom_text(aes(label = signif(p, 2)), 
                     size = 2.5) + 
           scale_fill_gradient(low = 'white', 
                               high = 'firebrick3', 
                               limits = c(0, 1), 
                               name = 'Conditional p') + 
           scale_y_discrete(labels = function(x) stri_replace(x, regex = '_.*', replacement = ''), 
                            limits = c('PRL_class', 
                                       'LH_class', 
                                       'FSH_class', 
                                       'E2_class', 
                                       'T_total_class')) + 
           globals$common_theme + 
           theme(axis.title = element_blank()) + 
           labs(title = y, 
                subtitle = z))

# END -------
  
  insert_tail()