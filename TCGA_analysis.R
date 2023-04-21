# Analyses done with the TCGA cohort:
#
# 1) Expression, pairwise correlation and components of genes related to 
# sex hormones are investigated by statistical hypothesis 
# testing, correlation and PCA
#
# 2) Hormonal subsets of cancer specimens are defined by 
# Gaussian mixture modeling (GMM)
#
# 3) Characteristic of demographic and clinical factors, biological processes 
# (GSVA with Reactome pathways), immune status (QuanTIseq and xCell) 
# and protein and gene expression is 
# investigated by statistical hypothesis testing (Kruskal-Wallis test and 
# one-way ANOVA)
#
# 4) Effects of hormone-related gene expression (optimal cutpoints)
# and hormonal subsets on relapse free survival investigated by Peto-Peto 
# test and Elastic Net Cox regression.
# 
# 

# tools ------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(rlang)
  
  library(exda)
  library(rstatix)
  library(DescTools)
  
  library(ggrepel)
  library(ggtext)
  
  library(soucer)
  library(furrr)

  library(clustTools)
  library(mixtools)
  library(mclust)

  library(microViz)
  library(SPIA)

  library(survminer)
  library(survival)
  library(coxExtensions)
  
  insert_head()
  
  select <- dplyr::select
  explore <- exda::explore
  set_rownames <- trafo::set_rownames
  map <- purrr::map
  
  c('./tools/globals.R', 
    './tools/functions.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  tcga_globals <- list()
  
  tcga_globals$gene_class_colors <- 
    c('testicle' = 'coral3', 
      'pituitary' = 'plum3')
  
  ## clustering genes: genes with clearly bi-modal distribution
  
  tcga_globals$clust_genes <- tcga$genes

  ## hormone class colors
  
  tcga_globals$clust_colors <- 
    c('#1' = 'darkseagreen4', 
      '#2' = 'steelblue2', 
      '#3' = 'coral2', 
      '#4' = 'coral4', 
      '#5' = 'gray60')
  
# analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  ## definition of the hormonal subsets, levels of explanatory factors
  
  c('./TCGA scripts/expression.R', 
    './TCGA scripts/correlation.R', 
    './TCGA scripts/pca.R', 
    './TCGA scripts/clustering.R', 
    './TCGA scripts/mixture.R', 
    './TCGA scripts/characteristic.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## demographic, clinical and biological characteristic
  
  c('./TCGA scripts/background.R', 
    './TCGA scripts/biology.R', 
    './TCGA scripts/quantiseq.R', 
    './TCGA scripts/xcell.R', 
    './TCGA scripts/protein.R', 
    './TCGA scripts/dge.R', 
    './TCGA scripts/dge_plots.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  if(file.exists('./cache/tcga_spia.RData')) {
    
    insert_msg('Loading cached signaling analysis results')
    
    load('./cache/tcga_spia.RData')
    
  } else {
    
    source_all('./TCGA scripts/signaling.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  c('./TCGA scripts/signaling_plots.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## survival
  
  c('./TCGA scripts/relapse.R', 
    './TCGA scripts/elanet.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()