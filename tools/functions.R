# Functions

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)

# formatting of tabled with descriptive stats ------

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
  
# END -------