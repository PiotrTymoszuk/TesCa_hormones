# Infirltration estimates by QuanTIseq and xCell

  insert_head()
  
# tools ----
  
  library(immunedeconv)
  library(furrr)
  
# container ------
  
  infiltration <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
# analysis globals ----
  
  insert_msg('Analysis globals')
  
  infiltration$analysis_tbl <- 
    tcga$expression[c('ID', tcga$annotation$gene_symbol)] %>% 
    column_to_rownames('ID') %>% 
    t
  
  infiltration$analysis_tbl <- 2^infiltration$analysis_tbl - 1
  
# Immunodeconvolution ------
  
  insert_msg('Deconvolution')
  
  infiltration[c('quantiseq', 
                 'xcell')] <- c('quantiseq', 'xcell') %>% 
    future_map(~deconvolute(gene_expression = infiltration$analysis_tbl, 
                            method = .x, 
                            arrays = FALSE, 
                            tumor = TRUE), 
               .options = furrr_options(seed = TRUE))
  
# clearing the infiltration results ------
  
  insert_msg('Clearing the output')
  
  ## lexicons
  
  infiltration[c('quantiseq_lexicon', 
                 'xcell_lexicon')] <- 
    infiltration[c("quantiseq", "xcell")] %>% 
    map(transmute, 
        label = cell_type, 
        variable = make.names(cell_type))
  
  ## infiltration estimates
  
  infiltration[c("quantiseq", "xcell")] <- 
    infiltration[c("quantiseq", "xcell")] %>% 
    map(mutate, cell_type = make.names(cell_type)) %>% 
    map(column_to_rownames, 'cell_type') %>% 
    map(t) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'ID') %>% 
    map(as_tibble)
  
# saving the results on the disc ------
  
  insert_msg('Saving the results')
  
  infiltration <- 
    infiltration[c("quantiseq_lexicon", "xcell_lexicon", 
                   "quantiseq", "xcell")]
  
  save(infiltration, file = './data/TCGA/infiltration.RData')

# END ----
  
  plan('sequential')
  
  insert_tail()