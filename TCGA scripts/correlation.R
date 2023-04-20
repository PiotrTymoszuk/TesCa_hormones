# Canonical correlation analysis of the hormone-related genes of interest
# in the TCGA cohort
# done for the entire cohort and the seminoma/NSGCT split

  insert_head()
  
# container -----
  
  tcga_corr <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  tcga_corr$lexicon <- tcga$gene_lexicon
  
  tcga_corr$gene_pairs <- 
    combn(tcga_corr$lexicon$gene_symbol, 
          m = 2, 
          simplify = FALSE)
  
  ## analysis tables
  
  tcga_corr$analysis_tbl$tcga <- tcga$expression %>% 
    select(histology, all_of(tcga_corr$lexicon$gene_symbol)) %>% 
    filter(complete.cases(.))
  
  tcga_corr$analysis_tbl[c('seminoma', 'NSGCT')] <- 
    tcga_corr$analysis_tbl$tcga %>% 
    blast(histology)
  
  tcga_corr$analysis_tbl <- tcga_corr$analysis_tbl %>% 
    map(select, -histology)

# testing: Spearman's correlation -----
  
  insert_msg('Correlation')
  
  tcga_corr$test <- tcga_corr$analysis_tbl %>% 
    map(function(data) tcga_corr$gene_pairs %>% 
          map_dfr(~correlate_variables(data, 
                                       variables = .x, 
                                       what = 'correlation', 
                                       type = 'spearman', 
                                       ci = TRUE, 
                                       pub_styled = FALSE)) %>% 
          adjust_fdr(variable = 'p_value') %>% 
          mutate(significant = ifelse(p_adjusted < 0.05, 'yes', 'no'), 
                 fface = ifelse(significant == 'yes', 'bold', 'plain')))
  
# correlograms ------
  
  insert_msg('Correlogram')
  
  tcga_corr$plots <- 
    list(x = tcga_corr$test, 
         y = paste('Correlation,', 
                   c('TCGA entire cohort', 
                     'TCGA seminoma', 
                     'TCGA NSGCT')), 
         z = map(tcga_corr$analysis_tbl, nrow)) %>% 
    pmap(function(x, y, z) x%>% 
           ggplot(aes(x = variable1, 
                      y = variable2, 
                      fill = estimate, 
                      size = abs(estimate))) + 
           geom_point(shape = 21) + 
           geom_text(aes(label = signif(estimate, 2), 
                         fontface = fface, 
                         alpha = significant), 
                     size = 2.5, 
                     hjust = 0.5, 
                     vjust = -1.2) + 
           scale_radius(limits = c(0, 1), 
                        range = c(0.5, 4.5), 
                        name = expression('abs(' * rho * ')')) + 
           scale_alpha_manual(values = c(no = 0.15, 
                                         yes = 1)) +
           scale_x_discrete(limits = tcga_corr$lexicon$gene_symbol) + 
           scale_y_discrete(limits = tcga_corr$lexicon$gene_symbol) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 0, 
                                limits = c(-1, 1), 
                                name = expression(rho)) + 
           guides(alpha = 'none') + 
           globals$common_theme + 
           theme(axis.title = element_blank(), 
                 axis.text = element_text(face = 'italic')) +
           labs(title = y, 
                subtitle = paste('Spearman correlation, n =', z)))

# END -------
  
  insert_tail()