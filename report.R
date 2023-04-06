# Analysis report

# tools -----

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(soucer)
  
  library(cowplot)
  library(flextable)
  library(writexl)
  library(figur)

  library(rmarkdown)
  library(knitr)
  library(bookdown)
  library(rmdformats)

  insert_head()
  
  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# report scripts --------
  
  insert_msg('Report scripts')
  
  c('./report scripts/tables.R', 
    './report scripts/figures.R', 
    './report scripts/html.R', 
    './report scripts/links.R', 
    './report scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()