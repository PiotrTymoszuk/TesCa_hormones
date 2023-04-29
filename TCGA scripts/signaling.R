# Differences in signaling between the subsets

  insert_head()
  
# container -----
  
  tcga_spia <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## Entrez IDs for all genes 
  
  tcga_spia$all <- tcga_dge$lexicon %>% 
    filter(!duplicated(entrez_id)) %>% 
    .$entrez_id
  
  ## log2 fold-regulation estimates for significantly regulated genes
  
  tcga_spia$de <- tcga_dge$significant_lm %>% 
    map(filter, 
        !is.na(entrez_id), 
        !duplicated(entrez_id)) %>% 
    map(blast, level) %>% 
    unlist(recursive = FALSE) %>% 
    map(~set_names(.x$estimate, 
                   .x$entrez_id))

# Testing -----
  
  insert_msg('Testing')
  
  tcga_spia$test <- tcga_spia$de %>% 
    future_map(~spia(de = .x, 
                     all = tcga_spia$all, 
                     verbose = FALSE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(as_tibble)
  
# Caching the results ------
  
  insert_msg('Saving the results')
  
  save(tcga_spia, file = './cache/tcga_spia.RData')
  
# END ------
  
  plan('sequential')
  
  insert_tail()