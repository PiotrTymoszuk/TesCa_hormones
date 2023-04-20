# Calculation of ssGSEA scores for the Reactome pathways

  insert_head()
  
# tools ------
  
  library(gseaTools)
  
# container ------
  
  sig <- list()
  
# reading the signature database -----
  
  insert_msg('Signature database')
  
  sig$lexicon <- 
    load_dbsig(path = './data/signatures/msigdb.v7.5.1.symbols.gmt')
  
  sig$lexicon <- sig$lexicon %>% 
    filter(stri_detect(sign_name, regex = '^REACTOME'))

# Calculating the ssGSEA scores ------
  
  insert_msg('Calculating the ssGSEA scores')
  
  sig$reactome <- 
    calculate(sig$lexicon, 
              data = tcga$expression[tcga$annotation$gene_symbol])
  
  sig$reactome$ID <- tcga$expression$ID
  
# Clearing the signature lexicon (user-friendly labels) -----
  
  insert_msg('Clearing the signature lexicon')
  
  sig$lexicon <- sig$lexicon %>% 
    mutate(variable = sign_name, 
           label = stri_replace(variable, 
                                regex = '^REACTOME_', 
                                replacement = ''), 
           label = stri_replace_all(label, 
                                    fixed = '_', 
                                    replacement = ' ')) %>% 
    mutate(variable %in% names(sig$reactome))

# saving the signatures on the disc ------
  
  insert_msg('Saving the signatures on the disc')
  
  save(sig, file = './data/TCGA/reactome.RData')
  
# END ------
  
  insert_tail()