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
                   plot_subtitle = 'Non-parametric EM model', 
                   k_sel = 5)
  
# Tuning the cutpoint models ------
  
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
  
  tcga_mix$gmm_tuning$plots <- 
    plot_class_bic(data = tcga_mix$gmm_tuning$stats, 
                   plot_subtitle = 'Gaussian mixture EM model', 
                   k_sel = 5)
  
# The final model ------
  
  insert_msg('The final model')
  
  set.seed(1234)
  
  tcga_mix$model <- tcga_mix$gmm_tuning$models$class_5
  
  ## posteriors and class assignment
  
  tcga_mix$posterior <- tcga_mix$model$z %>% 
    set_rownames(rownames(tcga_mix$analysis_tbl)) %>% 
    set_colnames(c('SEM1', 'NS PRL', 'SEM2', 'NS E2', 'NS T'))
  
  tcga_mix$assignment <- tcga_mix$posterior %>% 
    nodal_vote %>% 
    mutate(class = factor(class, 
                          c('SEM1', 
                            'SEM2', 
                            'NS PRL', 
                            'NS E2', 
                            'NS T')))
  
  ## clustering object
  
  tcga_mix$clust_obj <- 
    list(data = quo(tcga_mix$analysis_tbl),
         dist_mtx = calculate_dist(tcga_mix$analysis_tbl, 
                                   'euclidean'), 
         dist_method = 'euclidean', 
         clust_obj = NULL, 
         clust_assignment = set_names(tcga_mix$assignment, 
                                      c('observation', 'clust_id')), 
         clust_fun = 'kmeans', 
         dots = NULL) %>% 
    clust_analysis
  
# Heat map of the component means ------
  
  insert_msg('Hat map of the means')
  
  ## fitted means
  
  tcga_mix$mean_hm$means <- tcga_mix$model$parameters$mean %>% 
    set_colnames(c('SEM1', 'NS PRL', 'SEM2', 'NS E2', 'NS T')) %>% 
    as.data.frame %>% 
    rownames_to_column('gene_symbol') %>% 
    as_tibble
  
  ## variances/SD
  
  tcga_mix$mean_hm$variances <- 
    c(1:length(levels(tcga_mix$assignment$class))) %>% 
    map(~tcga_mix$model$parameters$variance$sigma[, , .x]) %>% 
    map(diag) %>% 
    set_names(c('SEM1', 'NS PRL', 'SEM2', 'NS E2', 'NS T')) %>% 
    map(compress, 
        names_to = 'gene_symbol', 
        values_to = 'variance') %>% 
    compress(names_to = 'class') %>% 
    mutate(class = factor(class, levels(tcga_mix$assignment$class)), 
           sd = sqrt(variance))

  ## gene hierarchy
  
  tcga_mix$mean_hm$cluster <- tcga_mix$mean_hm$means %>% 
    column_to_rownames('gene_symbol') %>% 
    as.data.frame %>% 
    hcluster(k = 3)
  
  tcga_mix$mean_hm$order <- tcga_mix$mean_hm$cluster$clust_assignment %>% 
    arrange(desc(clust_id)) %>% 
    set_names(c('gene_symbol', 'clust_id'))
  
  ## plotting data
  
  tcga_mix$mean_hm$data <-  tcga_mix$mean_hm$means %>% 
    pivot_longer(cols = levels(tcga_mix$assignment$class), 
                 names_to = 'class',
                 values_to = 'mean') %>% 
    left_join(tcga_mix$mean_hm$variances, 
              by = c('class', 'gene_symbol')) %>% 
    mutate(plot_lab = paste(signif(mean, 2), 
                            signif(sd, 2), 
                            sep = '\nSD = '), 
           class = factor(class, levels = levels(tcga_mix$assignment$class)))
  
  ## heat map
  
  tcga_mix$mean_hm$plot <- tcga_mix$mean_hm$data %>% 
    mutate(gene_symbol = factor(gene_symbol, 
                                tcga_mix$mean_hm$order$gene_symbol)) %>% 
    ggplot(aes(x = class, 
               y = gene_symbol, 
               fill = mean)) + 
    geom_tile(color = 'black') + 
    geom_text(aes(label = plot_lab), 
              size = 2.1, 
              color = 'gray20') + 
    scale_fill_gradient2(low = 'steelblue', 
                         mid = 'white', 
                         high = 'firebrick', 
                         name = 'µ') + 
    globals$common_theme + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_text(face = 'italic')) + 
    labs(title = 'Component means', 
         subtitle = 'Gaussian mixture EM model')

# Scatter plot of the expression pairs ------  
  
  insert_msg('Variable pair scatterplot')
  
  tcga_mix$pair_plot$data <- tcga_mix$analysis_tbl %>% 
    rownames_to_column('ID') %>% 
    left_join(tcga_mix$assignment,
              by = 'ID') %>% 
    as_tibble
  
  ## with GGally
  
  tcga_mix$pair_plot$ggally_plot <- tcga_mix$pair_plot$data %>% 
    ggpairs(aes(color = class, 
                alpha = 0.4, 
                size = 1), 
            columns = 2:12, 
            upper = list(continuous = 'cor')) + 
    scale_color_manual(values = tcga_globals$clust_colors) + 
    scale_fill_manual(values = tcga_globals$clust_colors) + 
    globals$common_theme + 
    theme(strip.text = element_text(face = 'italic')) + 
    labs(title = 'Expression of the hormonal subset-defining genes', 
         x = 'Expression Z score', 
         y = 'Expression Z score')
  
  ## particular pairs, custom function
  
  tcga_mix$pair_plot$gene_pairs <-  tcga_mix$lexicon$gene_symbol %>% 
    combn(m = 2, simplify = FALSE)
  
  tcga_mix$pair_plot$gene_pairs <- tcga_mix$pair_plot$gene_pairs %>% 
    set_names(map_chr(tcga_mix$pair_plot$gene_pairs, paste, collapse = '_'))
  
  tcga_mix$pair_plot$scatters <- tcga_mix$pair_plot$data %>% 
    plot_gmm_pairs(variable_pairs = tcga_mix$pair_plot$gene_pairs, 
                   fit_data = tcga_mix$mean_hm$data)

# END ------
  
  plan('sequential')
  
  insert_tail()