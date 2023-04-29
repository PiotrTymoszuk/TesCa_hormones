# Import and formatting of the TCGA dataset

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
  
# container ----
  
  tcga <- list()
  
# reading the clinical and expression data ------
  
  insert_msg('Reading the clinical and expression data')
  
  tcga[c('clinical', 
         'annotation', 
         'expression', 
         'follow_up', 
         'protein')] <- 
    list(file = c('./data/TCGA/data_clinical_patient.txt', 
                  './data/TCGA/data_mrna_seq_v2_rsem.txt', 
                  './data/TCGA/data_mrna_seq_v2_rsem.txt', 
                  './data/TCGA/data_timeline_status.txt', 
                  './data/TCGA/data_rppa.txt'), 
         skip = c(4, 0, 0, 0, 0)) %>% 
    pmap(read_tsv)

# Wrangling the clinical data ------
  
  insert_msg('Wrangling the clinical data')
  
  tcga$clinical <- tcga$clinical %>% 
    transmute(ID = PATIENT_ID, 
              histology = stri_replace(SUBTYPE, 
                                       regex = '^.*_', replacement = ''), 
              histology = car::recode(histology, "'non-seminoma' = 'NSGCT'"), 
              histology = factor(histology, c('seminoma', 'NSGCT')), 
              age_surgery = as.numeric(AGE), 
              pt_stage = stri_replace(AJCC_PATHOLOGIC_TUMOR_STAGE,
                                      regex = '.*\\s{1}', 
                                      replacement = ''), 
              pt_stage = stri_replace(pt_stage, 
                                      regex = '(A|B|S)$', 
                                      replacement = ''), 
              pt_stage = factor(pt_stage, c('I', 'II', 'III', 'IV')), 
              fup_days = as.numeric(DAYS_LAST_FOLLOWUP), 
              neoadjuvant = tolower(HISTORY_NEOADJUVANT_TRTYN), 
              neoadjuvant = factor(neoadjuvant, c('no', 'yes')), 
              pm_stage = stri_extract(PATH_M_STAGE, regex = '^M\\d{1}'), 
              pm_stage = factor(pm_stage, c('M0', 'M1')), 
              pn_stage = stri_extract(PATH_N_STAGE, 
                                      regex = '^N\\d{1}'), 
              pn_stage = factor(pn_stage, c('N0', 'N1', 'N2')), 
              path_t_stage = stri_extract(PATH_T_STAGE, 
                                          regex = '^T\\d{1}'), 
              path_t_stage = factor(path_t_stage, c('T1', 'T2', 'T3', 'T4')), 
              race = factor(RACE), 
              radiotherapy = tolower(RADIATION_THERAPY), 
              radiotherapy = factor(radiotherapy, c('no', 'yes')), 
              death = stri_extract(OS_STATUS, regex = '^\\d{1}'), 
              death = as.numeric(death), 
              os_days = as.numeric(OS_MONTHS) * 30.437, 
              tumor_death = stri_extract(DSS_STATUS, regex = '^\\d{1}'), 
              tumor_death = as.numeric(tumor_death), 
              dss_days = as.numeric(DSS_MONTHS) * 30.437, 
              relapse = stri_extract(DFS_STATUS, regex = '^\\d{1}'), 
              relapse = as.numeric(relapse), 
              rfs_days = as.numeric(DFS_MONTHS) * 30.437, 
              progression = stri_extract(PFS_STATUS, regex = '^\\d{1}'), 
              progression = as.numeric(progression), 
              progression_factor = ifelse(progression == 1, 'yes', 'no'), 
              progression_factor = factor(progression_factor, c('no', 'yes')), 
              pfs_days = as.numeric(PFS_MONTHS) * 30.437, 
              relapse = ifelse(is.na(relapse) & !is.na(progression_factor), 
                               ifelse(progression_factor == 'yes', 
                                      1, 0), 
                               relapse), 
              rfs_days = ifelse(is.na(rfs_days), pfs_days, rfs_days), 
              relapse_factor = ifelse(relapse == 1, 'yes', 'no'), 
              relapse_factor = factor(relapse_factor, c('no', 'yes')), 
              death_factor = ifelse(death == 1, 'yes', 'no'), 
              death_factor = factor(death_factor, c('no', 'yes')))
  
# Wrangling the follow-up data -----
  
  insert_msg('Wrangling the follow-up data')
  
  ## Serum marker and IGCCCG staging available at the initial diagnosis
  
  tcga$initial_diag <- tcga$follow_up %>% 
    filter(STATUS == 'Initial Diagnosis') %>% 
    transmute(ID = PATIENT_ID, 
              marker_status = toupper(SERUM_MARKERS), 
              marker_status = factor(marker_status, c('S0', 'S1', 'S2', 'S3')), 
              IGCCCG_risk_group = factor(tolower(IGCCCG_STAGE), 
                                         c('good', 'intermediate', 'poor')))
  
  ## recurrence at the follow-up: only the first tumor event is recorded
  
  tcga$new_event <- tcga$follow_up %>% 
    filter(STATUS %in% c('Locoregional Recurrence', 
                         'New Primary Tumor', 
                         'Distant Metastasis', 
                         'Last Follow Up')) %>% 
    transmute(ID = PATIENT_ID, 
              event_type = STATUS, 
              location = ANATOMIC_SITE, 
              event_days = as.numeric(START_DATE)) %>% 
    dlply(c('ID', 'event_type')) %>% 
    map_dfr(function(x) {
      
      if(x$event_type[[1]] == 'Last Follow Up') {
        
        return(filter(x, event_days == max(event_days)))
        
      } else {
        
        return(filter(x, event_days == min(event_days)))
        
      }
      
    }) %>% 
    blast(ID) %>% 
    map(filter, event_days == min(event_days)) %>% 
    map_dfr(~.x[1, ]) %>% 
    as_tibble
  
# Merging the additional diagnosis and follow-up data with the clinics ------
  
  insert_msg('Merging the clinical and follow-up information')
  
  tcga$clinical <- tcga[c("clinical", "initial_diag", "new_event")] %>% 
    reduce(left_join, by = 'ID')
  
  ## removal of the already included data
  
  tcga$initial_diag <- NULL
  tcga$new_event <- NULL
  tcga$follow_up <- NULL
  
  tcga <- compact(tcga)
  
# Annotation -------
  
  insert_msg('Annotation')
  
  ## keeps genes with symbols. Manual correction/check 
  ## of the duplicated symbols
  
  tcga$annotation <- tcga$annotation %>% 
    select(Hugo_Symbol, Entrez_Gene_Id) %>% 
    set_names(c('gene_symbol', 'entrez_id')) %>% 
    mutate(entrez_id = as.character(entrez_id)) %>% 
    filter(complete.cases(.)) %>% 
    filter(!entrez_id %in% ('200058')) %>% 
    filter(!duplicated(gene_symbol))
  
# Expression --------
  
  insert_msg('Expression')
  
  ## log2-transformation
  
  tcga$expression <- tcga$expression %>% 
    filter(Hugo_Symbol %in% tcga$annotation$gene_symbol) %>% 
    filter(!duplicated(Hugo_Symbol)) %>% 
    select(- Entrez_Gene_Id) %>% 
    column_to_rownames('Hugo_Symbol') %>% 
    t
  
  tcga$expression <- log2(tcga$expression + 1)
  
  tcga$expression <- tcga$expression %>% 
    as.data.frame %>% 
    rownames_to_column('sample_id') %>%
    mutate(ID = stri_extract(sample_id, 
                             regex = '^TCGA-\\w{2}-\\w{4}'))
  
# Merging expression with clinical information -----
  
  insert_msg('Merging clinical information and expression')
  
  tcga$expression <- 
    left_join(tcga$clinical, 
              tcga$expression, 
              by = 'ID') %>% 
    as_tibble
  
# Reading the variable lexicon ------
  
  insert_msg('Reading the variable lexicon')
  
  tcga$lexicon <- read_xlsx('./data/TCGA/variable_lexicon.xlsx') %>% 
    mutate(axis_label = ifelse(!is.na(unit), 
                               paste(label, unit, sep = ', '), 
                               label), 
           table_label = ifelse(!is.na(unit), 
                                paste(label_long, unit, sep = ', '), 
                                label_long), 
           class = factor(class, 
                          c('index', 'demography', 
                            'pathology', 'hormones', 
                            'treatment', 'prognosis')))
  
  ## categories for the factor variables
  
  tcga$levels <- tcga$clinical %>% 
    map(function(x) if(is.factor(x)) levels(x) else NA) %>% 
    map_chr(paste, collapse = ', ') %>% 
    compress(names_to = 'variable', 
             values_to = 'levels') %>% 
    mutate(levels = ifelse(levels == 'NA', 
                           NA, levels))
  
  tcga$lexicon <- left_join(tcga$lexicon, 
                            tcga$levels, 
                            by = 'variable')
  
# genes of interest -------
  
  insert_msg('Genes of interest')
  
  tcga$genes <- c('GNRH1', 'GNRH2', 'PRL', 'CGA', 'FSHB', 'LHB', ## pituitary hormones
                  'CYP11A1', 'CYP17A1', 'HSD17B3', 'HSD3B1', 'HSD3B2',  ## T
                  'CYP19A1', 'HSD17B1', ## E2
                  'SHBG') ## E2 and T
  
  tcga$gene_lexicon <- 
    c('GNRH1' = 'pituitary', 
      'GNRH2' = 'pituitary', 
      'PRL' = 'pituitary', 
      'CGA' = 'pituitary', 
      'FSHB' = 'pituitary', 
      'LHB' = 'pituitary',
      'CYP11A1' = 'testicle', 
      'CYP17A1' = 'testicle', 
      'HSD17B3' = 'testicle', 
      'HSD3B1' = 'testicle', 
      'HSD3B2' = 'testicle', 
      'CYP19A1' = 'testicle', 
      'HSD17B1' = 'testicle', 
      'SHBG' = 'testicle') %>% 
    compress(names_to = 'gene_symbol', 
             values_to = 'class') %>% 
    mutate(class = factor(class, c('testicle', 'pituitary')))
  
# Signatures of Reactome pathways ------
  
  insert_msg('Signatures of Reactome pathways')
  
  if(file.exists('./data/TCGA/reactome.RData')) {
    
    insert_msg('Loading cached Reactome data')
    
    load('./data/TCGA/reactome.RData')
    
  } else {
    
    source_all('./import scripts/TCGA_Reactome.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  tcga$reactome <- sig$reactome %>% 
    relocate(ID)
  
  tcga$reactome_lexicon <- sig$lexicon %>% 
    select(variable, label, genes)
  
  rm(sig)
  
# QuanTIseq and xCell infiltration estimates -----
  
  insert_msg('QuanTIseq and xCell infiltration')
  
  if(file.exists('./data/TCGA/infiltration.RData')) {
    
    insert_msg('Loading cached immunodeconvolution results')
    
    load('./data/TCGA/infiltration.RData')
    
  } else {
    
    source_all('./import scripts/TCGA_infiltration.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  tcga[names(infiltration)] <- infiltration
  
  rm(infiltration)
  
# Wrangling the protein expression --------
  
  insert_msg('Wrangling the protein expression')
  
  tcga$protein_lexicon <- tcga$protein %>% 
    transmute(variable = Composite.Element.REF, 
              label = stri_replace(variable, 
                                   regex = '^.*\\|', 
                                   replacement = ''))
  
  tcga$protein <- tcga$protein %>% 
    column_to_rownames('Composite.Element.REF') %>% 
    t %>% 
    as.data.frame %>% 
    rownames_to_column('sample_id') %>% 
    mutate(ID = stri_extract(sample_id, 
                             regex = '^TCGA-\\w{2}-\\w{4}')) %>% 
    relocate(ID) %>% 
    as_tibble
  
# somatic mutation data -------
  
  insert_msg('Somatic mutation data')
  
  if(file.exists('./data/TCGA/somut.RData')) {
    
    insert_msg('Loading cached somatic mutation data')
    
    load('./data/TCGA/somut.RData')
    
  } else {
    
    source_all('./import scripts/TCGA_mutect.R')
    
  }
  
  tcga[c('mutect_tmb', 
         'mutect_frequency',
         'mutect')] <- somut[c("mutect_tmb", 
                               "mutect_frequency", 
                               "mutect")]
  
  tcga[c('mutect_tmb', 
         'mutect')] <- tcga[c('mutect_tmb', 
                              'mutect')] %>% 
    map(mutate, 
        ID = stri_extract(sample_id, regex = '^TCGA-\\w{2}-\\w{4}')) %>% 
    map(relocate, ID)
  
  tcga$mutect_lexicon <- 
    tibble(gene_symbol = unique(names(tcga$mutect))) %>% 
    filter(!gene_symbol %in% c('sample_id', 'ID')) %>% 
    mutate(entrez_id = mapIds(org.Hs.eg.db, 
                              keys = gene_symbol, 
                              column = 'ENTREZID', 
                              keytype = 'ALIAS'))
  
  rm(somut)
  
# Copy number alterations by GISTIC ------
  
  insert_msg('Copy number alterations')
  
  ## only genes with unequivocal symbol assignment are included
  
  tcga$gistic <- read_tsv('./data/TCGA/TCGA-TGCT.gistic.tsv.gz') %>% 
    mutate(ensembl_id = stri_replace(`Gene Symbol`, 
                                      regex = '\\..*',
                                      replacement = ''), 
           gene_symbol = mapIds(org.Hs.eg.db, 
                                keys = ensembl_id, 
                                column = 'SYMBOL', 
                                keytype = 'ENSEMBL')) %>% 
    filter(!ensembl_id %in% c('ENSG00000261832', 
                              'ENSG00000280987', 
                              'ENSG00000261130', 
                              'ENSG00000258724', 
                              'ENSG00000279195', 
                              'ENSG00000258790', 
                              'ENSG00000275740', 
                              'ENSG00000255330', 
                              'ENSG00000273167', 
                              'ENSG00000269226', 
                              'ENSG00000255508', 
                              'ENSG00000272916', 
                              'ENSG00000270011'))
  
  ## manual correction of ambiguous gene identifiers
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000228696', 'gene_symbol'] <- 
    'ARL17B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000169627', 'gene_symbol'] <- 
    'BOLA2B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000226023', 'gene_symbol'] <- 
    'CT47A6'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000176797', 'gene_symbol'] <- 
    'DEFB103A'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000186599', 'gene_symbol'] <- 
    'DEFB105B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000198129', 'gene_symbol'] <- 
    'DEFB107B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000177257', 'gene_symbol'] <- 
    'DEFB4B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000256966', 'gene_symbol'] <- 
    'H3BUX3'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000184206', 'gene_symbol'] <- 
    'GOLGA6L10'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000237541', 'gene_symbol'] <- 
    'HLA-DQA2'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000249624', 'gene_symbol'] <- 
    'H7C3V1'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000178934', 'gene_symbol'] <- 
    'LGALS7B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000221995', 'gene_symbol'] <- 
    'TIAF7'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000111215', 'gene_symbol'] <- 
    'PRR4'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000258653', 'gene_symbol'] <- 
    'G3V3Y1'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000205572', 'gene_symbol'] <- 
    'SERF1B'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000205571', 'gene_symbol'] <- 
    'SMN2'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000274512', 'gene_symbol'] <- 
    'TBC1D3L'
  
  tcga$gistic[tcga$gistic$ensembl_id == 'ENSG00000236125', 'gene_symbol'] <- 
    'USP17L4'
  
  tcga$gistic <- tcga$gistic %>% 
    filter(!is.na(gene_symbol)) %>% 
    select(-`Gene Symbol`, -ensembl_id) %>% 
    column_to_rownames('gene_symbol') %>% 
    t %>% 
    as.data.frame %>% 
    rownames_to_column('sample_id') %>% 
    mutate(ID = stri_extract(sample_id, regex = '^TCGA-\\w{2}-\\w{4}')) %>% 
    as_tibble
  
  ## filtering out non-primary tumor samples
  
  tcga$gistic <- tcga$gistic %>% 
    filter(stri_detect(sample_id, regex = '01A$'))
  
  tcga$gistic_lexicon <- tibble(gene_symbol = names(tcga$gistic)) %>% 
    filter(!gene_symbol %in% c('sample_id', 'ID')) %>% 
    mutate(entrez_id = mapIds(x = org.Hs.eg.db, 
                              keys = gene_symbol, 
                              column = 'ENTREZID', 
                              keytype = 'SYMBOL'))
  
# Recon metabolism subsystem signature scores ------
  
  insert_msg('Recon signature scores')
  
  if(file.exists('./data/TCGA/metabolism.RData')) {
    
    insert_msg('Loading cached metabolis signature data')
    
    load('./data/TCGA/metabolism.RData')
    
  } else {
    
    source_all('./import scripts/TCGA_Recon.R', 
               crash = TRUE, message = TRUE)
    
  }
  
  tcga[c('recon_lexicon', 
         'recon')] <- metabolism[c('recon_lexicon', 
                                   'recon')]
  
  rm(metabolism)
    
# END -------
  
  insert_tail()