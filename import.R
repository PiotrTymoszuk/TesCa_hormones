# Import of the testis cancer study data provided by the Urology Team

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  library(readxl)
  library(soucer)
  library(lubridate)
  
  insert_head()
  
  c('./tools/globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# import scripts -------
  
  insert_msg('Import scripts')

  c('./import scripts/IBK.R', 
    './import scripts/TCGA.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()