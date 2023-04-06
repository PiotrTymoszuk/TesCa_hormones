# Comparison of survival between seminomas and mixed histology tumors

  insert_head()
  
# container ------
  
  histo_surv <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  histo_surv$analysis_tbl <- tesca$data %>% 
    select(ID, histology, relapse, rfs_days) %>% 
    filter(complete.cases(.))
  
  ## numbers of total and event cases
  
  histo_surv$n_numbers <- histo_surv$analysis_tbl %>% 
    count(relapse)
  
  histo_surv$n_numbers <- 
    paste0('total: n = ', sum(histo_surv$n_numbers$n), 
           ', events: n = ', histo_surv$n_numbers$n[2])
  
  ## numbers of histologies
  
  histo_surv$n_strata <- histo_surv$analysis_tbl %>% 
    count(histology)
  
  histo_surv$n_strata <- 
    map2_chr(histo_surv$n_strata[[1]], 
             histo_surv$n_strata[[2]], 
             paste, sep = '\nn = ')
  
# survftit objects and testing for differences in survival ------
  
  insert_msg('Survfit object and testing')
  
  histo_surv$survfit_obj <- 
    survminer::surv_fit(Surv(rfs_days, relapse) ~ histology, 
                        data = histo_surv$analysis_tbl)
  
  histo_surv$test <- histo_surv$survfit_obj %>% 
    surv_pvalue %>%
    adjust_fdr('pval', 'none')
  
# Kaplan-Meier plots ------
  
  insert_msg('Kaplan-Meier plots')
  
  histo_surv$plot <- histo_surv$survfit_obj %>% 
    ggsurvplot(palette = c('steelblue', 'coral3'), 
               title = 'Tumor histology and survival', 
               legend.labs = histo_surv$n_strata, 
               pval = histo_surv$test$significance[1], 
               pval.size = 2.75)
  
  histo_surv$plot <- histo_surv$plot$plot + 
    globals$common_theme + 
    labs(subtitle = histo_surv$n_numbers, 
         x = 'Relapse-free survial, days') + 
    theme(legend.position = 'right', 
          legend.title = element_blank())
  
# END ------
  
  insert_tail()