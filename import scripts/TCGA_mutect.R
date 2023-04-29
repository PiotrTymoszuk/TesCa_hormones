# Reads data on somatic mutations identified by Mutect2 

  insert_head()
  
# tools ------
  
  library(xena)
  
# container -----
  
  somut <- list()

# reading the database -------
  
  insert_msg('Reading the database')
  
  somut$db <- load_xena('./data/TCGA/TCGA-TGCT.mutect2_snv.tsv.gz')
  
# Formatting of the data --------
  
  insert_msg('Formatting')
  
  somut$mutect_tmb <- count_mutations(somut$db)
  
  somut$mutect_frequency <- freq_mutations(somut$db)
  
  somut$mutect <- tab_mutations(somut$db, as_matrix = FALSE)
  
# caching the results -------
  
  insert_msg('Saving the results')
  
  save(somut, file = './data/TCGA/somut.RData')
  
# END ------
  
  insert_tail()