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
  
# END -------