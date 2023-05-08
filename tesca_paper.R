# Figures, Tables and supplementary material for the testicle cancer 
# manuscript with the local registry data

# tools -----

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)

  library(exda)
  library(stringi)
  
  library(soucer)

  library(bookdown)
  library(flextable)
  library(writexl)
  library(knitr)
  library(rmarkdown)

  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Paper scripts ------
  
  insert_msg('Paper scripts')
  
  c('./tesca paper scripts/tables.R', 
    './tesca paper scripts/supplementary_tables.R', 
    './tesca paper scripts/figures.R', 
    './tesca paper scripts/supplementary_figures.R', 
    './tesca paper scripts/html.R', 
    './tesca paper scripts/links.R', 
    './tesca paper scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()
  
