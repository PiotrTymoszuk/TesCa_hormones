# Development of a mixture model classifying the TCGA cancers
# in respect to expression of hormone-related genes

  insert_head()
  
# container ------
  
  tcga_mix <- list()
  
# parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables and analysis tables
  
  tcga_mix$lexicon <- tcga$gene_lexicon %>% 
    filter(gene_symbol %in% tcga_globals$clust_genes)
  
  tcga_mix$analysis_tbl <- tcga$expression %>% 
    select(ID, all_of(tcga_mix$lexicon$gene_symbol)) %>% 
    filter(complete.cases(.)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
  ## numbers of classes
  
  tcga_mix$n_classes <- 1:8
  
  ## an object with the cutpoints for construction of mix models
  
  tcga_mix$mix_obj <- 
    makemultdata(tcga_mix$analysis_tbl, 
                 cuts = c(PRL = -0.25, 
                          CGA = -0.15, 
                          FSHB = 0.5,
                          LHB = -1.5, 
                          CYP11A1 = 0, 
                          CYP17A1 = -0.4, 
                          HSD17B3 = -0.85, 
                          HSD3B1 = 0.15, 
                          HSD3B2 = -0.9, 
                          CYP19A1 = -0.5588, 
                          HSD17B1 = 0))

# Determination of the optimal class number: non-parametric model -----
  
  insert_msg('Class numbers: non-parameteric model')
  
  ## the 2 class solution seems to be the best
  ## based on the BIC gradient
  
  set.seed(1234)
  
  tcga_mix$np_tuning$models <- tcga_mix$n_classes %>% 
    future_map(~npEM(x = as.matrix(tcga_mix$analysis_tbl), 
                     mu0 = .x, 
                     maxit = 5000, 
                     verb = FALSE), 
               .options = furrr_options(seed = TRUE)) %>% 
    set_names(paste0('class_', tcga_mix$n_classes))
  
  ## fit stats 
  
  tcga_mix$np_tuning$stats <- tcga_mix$np_tuning$models %>% 
    map_dfr(get_mix_stats) %>% 
    mutate(bic_gradient = lag(bic) - bic, 
           bic_gradient = ifelse(is.na(bic_gradient), 0, bic_gradient))
  
  ## plotting the BIC
  
  tcga_mix$np_tuning$plots <- 
    plot_class_bic(data = tcga_mix$np_tuning$stats, 
                   plot_subtitle = 'Non-parametric EM model')
  
# Tuning the GMM models ------
  
  insert_msg('Tuning the cutpoint models')
  
  set.seed(1234)
  
  tcga_mix$cut_tuning$models <- tcga_mix$n_classes[tcga_mix$n_classes %in% c(2:5)] %>% 
    map(~multmixEM(y = tcga_mix$mix_obj, 
                   k = .x)) %>% 
    set_names(paste0('class_', 
                     tcga_mix$n_classes[tcga_mix$n_classes %in% c(2:5)]))
  
  ## fit stats 
  
  tcga_mix$cut_tuning$stats <- tcga_mix$cut_tuning$models %>% 
    map_dfr(get_mix_stats) %>% 
    mutate(bic_gradient = lag(bic) - bic, 
           bic_gradient = ifelse(is.na(bic_gradient), 0, bic_gradient))
  
  ## mxtools fit stats: BIC is not adjusted for the class number!
  
  tcga_mix$cut_tuning$mixtools_stats <- tcga_mix$mix_obj %>% 
    multmixmodel.sel %>% 
    t %>% 
    as.data.frame %>% 
    rownames_to_column('k') %>% 
    filter(k != 'Winner') %>% 
    set_names(c('k', 'aic', 'bic', 'caic', 'icl', 'loglik')) %>% 
    mutate(k = as.numeric(k), 
           bic = -bic, 
           aic = -aic, 
           caic = -caic, 
           icl = -icl, 
           bic_gradient = lag(bic) - bic, 
           bic_gradient = ifelse(is.na(bic_gradient), 0, bic_gradient))
    
  ## plotting the BIC
  
  tcga_mix$cut_tuning$plots <- 
    plot_class_bic(data = tcga_mix$cut_tuning$stats, 
                   plot_subtitle = 'Cutpoint EM model')
  
  tcga_mix$cut_tuning$mixtools_plots <- 
    plot_class_bic(data = tcga_mix$cut_tuning$mixtools_stats, 
                   plot_subtitle = 'Cutpoint EM model')
  
# Fitting the Mclust GMM models -----
  
  insert_msg('GMM models')
  
  set.seed(1234)
  
  ## models
  
  tcga_mix$gmm_tuning$models <-  tcga_mix$n_classes %>% 
    map(~Mclust(data = tcga_mix$analysis_tbl, 
                G = .x, 
                prior = priorControl(functionName = 'defaultPrior', 
                                     mean = map_dbl(tcga_mix$analysis_tbl, 
                                                    median)))) %>% 
    set_names(paste0('class_', tcga_mix$n_classes))
  
  ## fit stats 
  
  tcga_mix$gmm_tuning$stats <- tcga_mix$gmm_tuning$models %>% 
    map_dfr(get_mclust_stats) %>% 
    mutate(bic_gradient = lag(bic) - bic, 
           bic_gradient = ifelse(is.na(bic_gradient), 0, bic_gradient))
  
  ## plotting the BIC
  
  tcga_mix$cut_tuning$plots <- 
    plot_class_bic(data = tcga_mix$gmm_tuning$stats, 
                   plot_subtitle = 'Gaussian mixture EM model', 
                   k_sel = 4)
  
# The final model ------
  
  insert_msg('The final model')
  
  set.seed(1234)
  
  tcga_mix$model <- tcga_mix$gmm_tuning$models$class_4
  
  ## posteriors and class assignment
  
  tcga_mix$posterior <- tcga_mix$model$z %>% 
    set_rownames(rownames(tcga_mix$analysis_tbl)) %>% 
    set_colnames(paste0('#', 1:ncol(tcga_mix$model$z)))
  
  tcga_mix$assignment <- tcga_mix$posterior %>% 
    nodal_vote
  
  ## clustering object
  
  tcga_mix$clust_obj <- 
    list(data = quo(tcga_mix$analysis_tbl),
         dist_mtx = calculate_dist(tcga_mix$analysis_tbl, 
                                   'cosine'), 
         dist_method = 'cosine', 
         clust_obj = NULL, 
         clust_assignment = set_names(tcga_mix$assignment, 
                                      c('observation', 'clust_id')), 
         clust_fun = 'kmeans', 
         dots = NULL) %>% 
    clust_analysis

# END ------
  
  plan('sequential')
  
  insert_tail()