# Functions

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)
  library(ggforce)
  library(ggExtra)
  library(clustTools)
  library(igraph)
  library(ggnetwork)
  library(proxy)

# formatting of tables with descriptive stats ------

  format_tbl <- function(x, 
                         lexicon = tesca$lexicon, 
                         rm_complete = FALSE, ...) {
    
    ## common format of the descriptive statistic tables
    
    x <- x %>% 
      map_dfc(stri_replace, 
              regex = '^Mean.*\\nMedian\\s{1}=\\s{1}', 
              replacement = '') %>% 
      map_dfc(stri_replace, 
              fixed = 'Range', 
              replacement = 'range') %>% 
      map_dfc(stri_replace, 
              fixed = 'Complete', 
              replacement = 'complete') %>% 
      map_dfc(stri_replace, 
              regex = '^no:.*\\nyes:\\s{1}', 
              replacement = '')
    
    if(rm_complete) {
      
      x <- x %>% 
        map_dfc(stri_replace, 
                regex = '\\ncomplete.*$', 
                replacement = '')
      
    }
    
    if(!is.null(lexicon)) {
      
      x <- x %>% 
        mutate(variable = exchange(variable, 
                                   dict = lexicon, ...))
      
    }
    
    x
    
  }
  
# multiple testing -------
  
  adjust_fdr <- function(x, variable, adj_method = 'BH') {
    
    x %>% 
      mutate(p_adjusted = p.adjust(.data[[variable]], adj_method), 
             significance = ifelse(p_adjusted < 0.05, 
                                   paste('p =', signif(p_adjusted, 2)), 
                                   paste0('ns (p = ', 
                                          signif(p_adjusted, 2), ')')))
    
  }
  
# plot panels -------
  
  factor_plot_pair <- function(x, y) {
    
    ## stitches two plots for factor variables together
    ## and provides a common legend below the plot
    
    list(x, y) %>% 
      map(~.x + 
            theme(legend.position = 'none',
                  axis.text.x = element_text(size = 7))) %>% 
      plot_grid(plotlist = ., 
                ncol = 2, 
                align = 'hv',
                axis = 'tblr') %>% 
      plot_grid(get_legend(x + 
                             theme(legend.position = 'bottom')), 
                nrow = 2, 
                rel_heights = c(0.8, 0.2))
    
  }
  
  
# Mixture models -----
  
  get_mix_stats <- function(mix_model) {
    
    ## calculates BIC corrected for the class number
    ## in accordance with the BIC returned by poLCA
    
    if(class(mix_model) == 'mixEM') {
      
      n_classes <- length(mix_model$lambda) ## number of classes
      
      n_expl <- ncol(mix_model$y) ## number of explanatory factors
      
      n_obs <- nrow(mix_model$y) ## number of observations
      
    } else {
      
      n_classes <- ncol(mix_model$lambda) ## number of classes
      
      if(is.null(n_classes)) n_classes <- 1
      
      n_expl <- ncol(mix_model$data) ## number of explanatory factors
      
      n_obs <- nrow(mix_model$data) ## number of observations
      
    }
    
    n_par <- n_classes * (n_expl - 1)
    
    loglik <- max(mix_model$loglik)
    
    tibble(k = n_classes, 
           n_var = n_expl, 
           n_par = n_par, 
           n = n_obs, 
           loglik = loglik, 
           bic = -2 * loglik + log(n_obs) * n_par)
    
  }
  
  get_mclust_stats <- function(mclust_model) {
    
    ## extracts stats from the model
    ## The genuine BIC value is multiplied by -1
    ## to keep it consistent with other packages
    ## (the lowest BIC -> optimal model)
    
    tibble(k = mclust_model$G, 
           n_var = mclust_model$d, 
           n_par = mclust_model$G * (mclust_model$d - 1), 
           n = mclust_model$n, 
           loglik = mclust_model$loglik, 
           bic = -mclust_model$bic)
    
  }
  
  plot_class_bic <- function(data, 
                             plot_subtitle = NULL, 
                             x_lab = 'Class number, k', 
                             k_sel = 2) {
    
    list(x = c('Class number choice, BIC', 
               'Class number choice, BIC gradient'), 
         y = c('bic', 'bic_gradient'), 
         z = list('BIC', expression(Delta * BIC))) %>% 
      pmap(function(x, y, z) data %>% 
             ggplot(aes(x = k, 
                        y = .data[[y]])) + 
             geom_vline(xintercept = k_sel, 
                        linetype = 'dashed', 
                        color = 'coral3') + 
             geom_path(aes(group = 'a'), 
                       color = 'steelblue') + 
             geom_point(shape = 16, 
                        color = 'steelblue') + 
             globals$common_theme + 
             scale_x_continuous(breaks = data$k) + 
             labs(title = x, 
                  subtitle = plot_subtitle, 
                  x = x_lab, 
                  y = z)) %>% 
      set_names(c('bic_plot', 'bic_gradient_plot'))
    
  }
  
  nodal_vote <- function(posterior_mtx) {
    
    rownames(posterior_mtx) %>% 
      map(~posterior_mtx[.x, ]) %>% 
      map(~.x[.x == max(.x)]) %>% 
      map_chr(names) %>% 
      set_names(rownames(posterior_mtx)) %>% 
      compress(names_to = 'ID', 
               values_to = 'class') %>% 
      mutate(class = factor(class))
    
  }

# Visualization -----
  
  draw_class_hm <- function(data, 
                            variables, 
                            plot_title = NULL, 
                            x_lab = 'Sample', ...) {
    
    ## draws the requested features in hormonal classes of the cancers
    
    ##m metadata
    
    strata_n <- data %>% 
      count(class)
    
    strata_n <- 
      map2_chr(strata_n[[1]], 
               strata_n[[2]], 
               paste, sep = ': n = ')
    
    data <- data %>% 
      pivot_longer(cols = all_of(variables), 
                   names_to = 'variable', 
                   values_to = 'value') %>% 
      mutate(variable = factor(variable, variables))
    
    ## plot
    
    data %>% 
      ggplot(aes(x = reorder(ID, value), 
                 y = variable, 
                 fill = value)) + 
      geom_tile() + 
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'black', 
                           high = 'firebrick', 
                           ...) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.x = element_blank()) + 
      labs(title = plot_title, 
           subtitle = paste(strata_n, collapse = ', '), 
           x = x_lab) + 
      facet_grid(. ~ class, 
                 scales = 'free', 
                 space = 'free')
    
  }
  
  plot_gmm_pairs <- function(data, 
                             variable_pairs, 
                             fit_data, 
                             point_alpha = 0.8, 
                             ellipse_alpha = 0.25, 
                             ellipse_angle = 0) {
    
    ## plots scatterplots of pairs of the class-defining variables
    ## with the fitted means and variances in form of data ellipses
    ##
    ## as such, useful for GMM models with elliptic or variant class shapes
    ## which are parallel to an axis
    
    ## fit means and variances, variable pairs ------
    
    mean_tbl <- fit_data %>% 
      select(c('class', 'gene_symbol', 'mean')) %>% 
      pivot_wider(names_from = 'gene_symbol', 
                  values_from = 'mean')
    
    var_tbl <- fit_data %>% 
      select(c('class', 'gene_symbol', 'variance')) %>% 
      pivot_wider(names_from = 'gene_symbol', 
                  values_from = 'variance')
    
    names(var_tbl)[names(var_tbl) != 'class'] <- 
      names(var_tbl)[names(var_tbl) != 'class'] %>% 
      paste0('_var')
    
    fit_tbl <- left_join(mean_tbl, 
                         var_tbl, 
                         by = 'class')
    
    strata_n <- count(data, class)
    
    strata_lab <- 
      map2_chr(strata_n[[1]], strata_n[[2]], 
               paste, sep = ': n = ')
    
    ## plotting -------
    
    pair_plots <- variable_pairs %>% 
      future_map(~ggplot(data, 
                         aes(x = .data[[.x[1]]], 
                             y = .data[[.x[2]]], 
                             fill = class, 
                             colour = class)) + 
                   geom_ellipse(data = fit_tbl, 
                                aes(x0 = .data[[.x[1]]], 
                                    y0 = .data[[.x[2]]], 
                                    a = qnorm(0.975) * .data[[paste0(.x, '_var')[1]]], 
                                    b = qnorm(0.975) * .data[[paste0(.x, '_var')[2]]], 
                                    angle = ellipse_angle), 
                                alpha = ellipse_alpha, 
                                color = 'gray60') + 
                   geom_point(shape = 21, 
                              size = 2, 
                              alpha = point_alpha) + 
                   geom_point(data = mean_tbl, 
                              size = 4, 
                              shape = 23, 
                              show.legend = FALSE) + 
                   scale_fill_manual(values = tcga_globals$clust_colors,
                                     labels = strata_lab, 
                                     name = '') + 
                   scale_color_manual(values = tcga_globals$clust_colors,
                                      labels = strata_lab, 
                                      name = '') + 
                   globals$common_theme + 
                   theme(axis.title.x = element_markdown(), 
                         axis.title.y = element_markdown(), 
                         plot.title = element_markdown()) + 
                   labs(title = paste0('<b><em>', .x[1], 
                                       '</em> and <em>', .x[2], '</em></b>'), 
                        x = paste0('<em>', .x[1], '</em>, expression Z score'), 
                        y = paste0('<em>', .x[2], '</em>, expression Z score')), 
                 .options = furrr_options(seed = TRUE))
    
    pair_plots %>% 
      future_map(ggMarginal, 
                 type = 'density', 
                 groupColour = TRUE, 
                 groupFill = TRUE, 
                 .options = furrr_options(seed = TRUE))
    
  }
  
  draw_clustered_hm <- function(data, 
                                variables, 
                                plot_title = NULL, 
                                x_lab = 'Sample', 
                                distance = 'euclidean', 
                                k = 3, 
                                return_clust = FALSE, ...) {
    
    ## plots a heat map depicting differences of the given variables
    ## between the hormonal subsets (variable 'class' in the data)
    ## the variables are clustered by PAM with the given distance 
    ## and cluster number k for a more comprehensive visualization
    
    data <- data[c('ID', 'class', variables)]
    
    ## clustering ------
    
    data_cluster <- data %>% 
      column_to_rownames('ID') %>% 
      select(-class) %>% 
      t %>% 
      as.data.frame %>% 
      kcluster(k = k, 
               distance_method = distance, 
               clust_fun = 'pam', 
               variant = 'faster')
    
    if(return_clust) return(data_cluster)
    
    plotting_order <- data_cluster$clust_assignment %>% 
      arrange(clust_id) %>% 
      set_names(c('variable', 'clust_id'))
    
    ## heat map ------
    
    hm_plot <- 
      draw_class_hm(data = data, 
                    variables = variables, 
                    plot_title = plot_title, 
                    x_lab = x_lab, ...) + 
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank())
    
    ## adding the clustering structure ------
    
    hm_plot$data <- hm_plot$data %>% 
      left_join(plotting_order, by = 'variable')
    
    hm_plot + 
      facet_grid(clust_id ~ class, 
                 space = 'free', 
                 scales = 'free') + 
      theme(strip.background.y = element_blank(), 
            strip.text.y = element_blank())
    
  }
  
  plot_similarity <- function(red_obj, 
                              class_assignment, 
                              distance = 'euclidean', 
                              x_lab = 'Dim 1', 
                              y_lab = 'Dim 2', 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              point_alpha = 1, 
                              center_fun = mean, 
                              min_max_similarity = TRUE, 
                              weighting_order = 1, 
                              leg_title = paste(distance, 'similarity'), 
                              point_size = 2, 
                              net_theme = globals$common_theme) {
    
    ## visualizes similarity between observations
    
    plot_data <- red_obj$component_tbl %>% 
      mutate(ID = observation) %>% 
      left_join(class_assignment, by = 'ID') %>% 
      column_to_rownames('ID')
    
    strata_n <- count(class_assignment, class)
    
    strata_lab <-
      map2_chr(strata_n[[1]], 
               strata_n[[2]], 
               paste, sep = ': n = ') %>% 
      set_names(strata_n[[1]])
    
    ## mass centers ------
    
    mc_data <- plot_data %>% 
      select(comp_1, comp_2, class) %>% 
      group_by(class) %>% 
      summarise(comp_1 = center_fun(comp_1), 
                comp_2 = center_fun(comp_2))
    
    ## datasets per class ------
    
    class_data <- model.frame(red_obj) %>% 
      rownames_to_column('ID') %>% 
      inner_join(class_assignment, by = 'ID') %>% 
      blast(class) %>% 
      map(select, -class, -ID)
    
    ## mean cross-similarities between the classes and a similarity graph -----
    
    class_simil_mtx <- matrix(1, 
                              nrow = length(levels(class_assignment$class)), 
                              ncol = length(levels(class_assignment$class))) %>% 
      set_colnames(levels(class_assignment$class)) %>% 
      set_rownames(levels(class_assignment$class))
    
    class_pairs <- levels(class_assignment$class) %>% 
      combn(m = 2, simplify = FALSE)

    class_cross_simil <- class_pairs %>% 
      map(~proxy::simil(x = class_data[[.x[1]]], 
                        y = class_data[[.x[2]]], 
                        method = distance)) %>% 
      map_dbl(center_fun) %>% 
      map2(., class_pairs, 
           ~list(class_1 = .y[1], 
                 class_2 = .y[2], 
                 simil = .x))
    
    for(i in class_cross_simil) {
      
      class_simil_mtx[i[[1]], i[[2]]] <- i[[3]]
      class_simil_mtx[i[[2]], i[[1]]] <- i[[3]]
      
    }
      
    if(min_max_similarity) {
      
      class_simil_mtx <- 
        (class_simil_mtx - min(class_simil_mtx))/(max(class_simil_mtx) - min(class_simil_mtx))
      
    } 
    
    class_graph <- 
      graph_from_adjacency_matrix(class_simil_mtx, 
                                  mode = 'undirected', 
                                  diag = FALSE, 
                                  weighted = TRUE)
    
    ## plots of the components -------
    
    comp_plots <-
      list(components = plot_data, 
           centers = mc_data) %>% 
      map(~ ggplot(.x, 
                   aes(x = comp_1, 
                       y = comp_2, 
                       fill = class)) + 
            geom_point(shape = 21, 
                       size = 2, 
                       alpha = point_alpha) + 
            scale_fill_manual(values = tcga_globals$clust_colors, 
                              labels = strata_lab, 
                              name = '') + 
            globals$common_theme + 
            labs(title = plot_title, 
                 subtitle = plot_subtitle, 
                 x = x_lab, 
                 y = y_lab))
    
    comp_plots$centers <- comp_plots$centers + 
      geom_text_repel(aes(label = class, 
                          color = class), 
                      size = 2.75) + 
      scale_color_manual(values = tcga_globals$clust_colors, 
                        labels = strata_lab, 
                        name = '')
    
    ## network plot ------
    
    if(weighting_order > 1) {
      
      leg_title <- 
        paste0('(', leg_title, ')<sup>', weighting_order, '</sup>')
      
    }
    
    comp_plots$network <- class_graph %>% 
      ggplot(aes(x = x, 
                 y = y, 
                 xend = xend, 
                 yend = yend)) + 
      geom_edges(aes(alpha = weight^weighting_order, 
                     size = weight^weighting_order)) + 
      geom_nodes(aes(color = name, 
                     fill = name), 
                 size = point_size) + 
      geom_nodelabel_repel(aes(label = name, 
                               fill = name), 
                           color = 'white', 
                           size = 2.75, 
                           show.legend = FALSE) + 
      geom_edgelabel_repel(aes(label = signif(weight, 2)), 
                           size = 2.75, 
                           show.legend = FALSE) + 
      scale_color_manual(values = tcga_globals$clust_colors, 
                         labels = strata_lab, 
                         name = '') + 
      scale_fill_manual(values = tcga_globals$clust_colors, 
                         labels = strata_lab, 
                         name = '') + 
      scale_size(limits = c(0, 1), 
                 range = c(0.2, 1.2), 
                 name = leg_title) + 
      scale_alpha_continuous(limits = c(0, 1), 
                             range = c(0.2, 1)) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           alpha = leg_title) + 
      net_theme + 
      theme(legend.title = element_markdown(size = 8), 
            legend.text = globals$common_text, 
            plot.subtitle = globals$common_text, 
            plot.title = element_text(size = 8, face = 'bold'))
    
    ## output -------
    
    return(comp_plots)
    
  }
  
# variable importance -----
  
  plot_rf_importance <- function(importance_data, 
                                 bar_fill = 'steelblue', 
                                 plot_title = 'Variable importance', 
                                 plot_subtitle = NULL, 
                                 x_lab = expression(Delta * ' accuracy')) {
    
    ## Plots permutation importance measure for a Random Forest model
    
    importance_data %>% 
      ggplot(aes(x = delta_accuracy, 
                 y = reorder(var_label, delta_accuracy))) + 
      geom_bar(stat = 'identity',
               color = 'black', 
               fill = bar_fill) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = x_lab)
    
  }
  
# Factors -----
  
  new_baseline <- function(x, baseline) {
    
    stopifnot(is.factor(x))
    stopifnot(baseline %in% levels(x))
    
    new_levels <- 
      c(baseline, levels(x)[levels(x) != baseline])
    
    factor(x, new_levels)
    
  }
  
# END -------