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
    cbind(E2_class, T_total_class, HCG_class, FSH_class, LH_class, PRL_class) ~ 1
  
# Tuning ------
  
  insert_msg('Model tuning')
  
  ## class range
  
  lca$tuning$range <- 2:6
  
  ## comparing model fit stats for varying class numbers
  
  set.seed(1234)
  
  lca$tuning$models <- 
    list(nclass = lca$tuning$range ) %>% 
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
           ggplot(aes(x = factor(class_number), 
                      y = .data[[x]], 
                      group = 'a')) + 
           geom_vline(xintercept = '3', 
                      linetype = 'dashed', 
                      color = 'coral3') + 
           geom_path(color = 'steelblue') + 
           globals$common_theme + 
           labs(title = y, 
                subtitle = 'MLE-driven latent class analysis', 
                x = 'class number', 
                y = z))
  
# final model -------
  
  insert_msg('Final model')
  
  lca$model <- lca$tuning$models$class_3
  
  ## posterior p for class assignment of the observations
  
  lca$posterior <- lca$model$posterior %>% 
    set_rownames(rownames(lca$analysis_tbl)) %>% 
    set_colnames(c('#1', '#2', '#3'))
  
  ## class assignment of the observation by simple voting
  
  lca$assingment <- rownames(lca$posterior) %>% 
    map(~lca$posterior[.x, ]) %>% 
    map(~.x[.x == max(.x)]) %>% 
    map_chr(names) %>% 
    set_names(rownames(lca$posterior)) %>% 
    compress(names_to = 'ID', 
             values_to = 'class') %>% 
    mutate(class = factor(class))
  
  ## renaming after the type of deregulated hormone family
  
  lca$assingment <- lca$assingment %>% 
    mutate(class = car::recode(class, 
                               "'#1' = 'neutral'; 
                               '#2' = 'pituitary'; 
                               '#3' = 'testicle'"), 
           class = factor(class, c('neutral', 'testicle', 'pituitary')))
  
# END -------
  
  insert_tail()