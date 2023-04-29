# Signature scores (ssGSEA) for the Recon subsystems

  insert_head()
  
# tools -------
  
  library(biggrExtra)
  library(gseaTools)
  
# container --------
  
  metabolism <- list()
  
# signature database ------
  
  insert_msg('Sigature database')
  
  ## genes, reactions and subsystems
  
  metabolism$db$subs <- extract_subsystems(Recon2D)
  
  metabolism$db$genes <- extract_genes(Recon2D)
  
  metabolism$db <- reduce(metabolism$db, 
                          inner_join, by = 'react_id')
  
  ## variable lexicon
  
  metabolism$recon_lexicon <- metabolism$db %>% 
    transmute(variable = make.names(subsystem), 
              label = subsystem) %>% 
    filter(!duplicated(variable))
  
  ## unique gene identifiers, translation to gene symbols
  
  metabolism$db <- metabolism$db %>% 
    mutate(variable = make.names(subsystem)) %>% 
    select(variable, entrez_id) %>% 
    blast(variable) %>% 
    map(~.x$entrez_id) %>% 
    map(reduce, union)
  
  metabolism$db <- metabolism$db %>% 
    map(~mapIds(x = org.Hs.eg.db, 
                keys = .x, 
                keytype = 'ENTREZID', 
                column = 'SYMBOL'))
  
# calculation of the signatures scores -------
  
  insert_msg('Calculation of the signature scores')
  
  metabolism$recon <- 
    calculate(metabolism$db, 
              data = tcga$expression[tcga$annotation$gene_symbol])
  
  metabolism$recon <- metabolism$recon %>% 
    mutate(ID = tcga$expression$ID) %>% 
    relocate(ID)

  metabolism$recon_lexicon <- metabolism$recon_lexicon %>% 
    filter(variable %in% names(metabolism$recon))
  
# saving the results --------
  
  insert_msg('Saving the signature data')
  
  save(metabolism, file = './data/TCGA/metabolism.RData')
  
# END ------
  
  insert_tail()