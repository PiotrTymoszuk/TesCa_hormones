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
  
  
# END -------