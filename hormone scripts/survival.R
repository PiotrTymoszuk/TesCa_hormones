# Differences in survival between the latent classes

  insert_head()
  
# container -----
  
  class_surv <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')

  class_surv$assignment <- lca$assingment
  
  ## analysis table with the survival data
  
  class_surv$analysis_tbl <- 
    left_join(class_surv$assignment, 
              tesca$data[c('ID', 'relapse', 'rfs_days')], 
              by = 'ID') %>% 
    filter(complete.cases(.))
  
  ## n numbers of events
  
  class_surv$n_numbers <- class_surv$analysis_tbl %>% 
    count(relapse)
  
  class_surv$n_numbers <- 
    map2_chr(c('total', 'events'), 
             class_surv$n_numbers[[2]], 
             paste, sep = ': n = ') %>% 
    paste(collapse = ', ')
  
  ## n numbers for clusters
  
  class_surv$n_strata <- class_surv$analysis_tbl %>% 
    count(class)
  
  class_surv$n_strata <- 
    map2_chr(class_surv$n_strata[[1]], 
             class_surv$n_strata[[2]], 
             paste, sep = ': n = ') %>% 
    set_names(levels(class_surv$analysis_tbl$class))
  
# Survfit object and testing -----
  
  insert_msg('Survfit object and testing')
  
  class_surv$survfit_obj <- 
    survminer::surv_fit(formula = Surv(rfs_days, relapse) ~ class, 
                        data = class_surv$analysis_tbl)
  
  ## log-rank test
  
  class_surv$test <- class_surv$survfit_obj %>% 
    surv_pvalue(method = 'S1') %>% 
    adjust_fdr('pval', 'none')
  
# Kaplan-Meier plot ------
  
  insert_msg('Kaplan-Meier plot')
  
  class_surv$plot <- class_surv$survfit_obj %>% 
    ggsurvplot(palette = unname(hor_globals$class_colors), 
               pval = class_surv$test$significance, 
               pval.size = 2.75, 
               legend.labs = class_surv$n_strata, 
               title = 'Hormonal subsets and survival', 
               xlab = 'Relapse-free survival, days')
  
  class_surv$plot <- class_surv$plot$plot + 
    labs(subtitle = class_surv$n_numbers) + 
    globals$common_theme
  
# END -----
  
  insert_tail()