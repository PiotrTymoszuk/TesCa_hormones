# Comparison of somatic mutation frequencies between the hormonal subsets

  insert_head()
  
# container -----
  
  tcga_mut <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  tcga_mut$analysis_tbl <- 
    inner_join(tcga_mix$assignment, 
               tcga$mutect, 
               by = 'ID') %>% 
    map_dfc(unname)
  
  tcga_mut$analysis_tbl[make.names(tcga$mutect_frequency$gene)] <- 
    tcga_mut$analysis_tbl[make.names(tcga$mutect_frequency$gene)] %>% 
    map_dfc(car::recode, "0 = 'no'; 1 = 'yes'") %>% 
    map_dfc(factor, c('no', 'yes'))
  
  ## n numbers 
  
  tcga_mut$strata_n <- tcga_mut$analysis_tbl %>% 
    count(class)
  
# Mutation frequency, identification of the top mutated genes ------
  
  insert_msg('Top mutations')
  
  ## top mutations present in at least 3% of samples
  
  tcga_mut$top_mutations <- tcga$mutect_frequency %>% 
    filter(percent >= 3) %>% 
    .$gene
  
  ## plotting mutation frequencies
  
  tcga_mut$frequency_plot <- tcga$mutect_frequency %>% 
    top_n(20, percent) %>% 
    ggplot(aes(x = percent, 
               y = reorder(gene, percent))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    geom_text(aes(label = signif(percent, 2)), 
              size = 2.5, 
              hjust = -1.4) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(face = 'italic')) + 
    labs(title = 'Top most Frequent mutations', 
         subtitle = paste('Mutect2 algorithm, n =', nrow(tcga$mutect)), 
         x = '% of cohort')
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  tcga_mut$stats <- tcga_mut$analysis_tbl %>% 
    explore(variables = tcga_mut$top_mutations, 
            split_factor = 'class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, 
           by = 'variable') %>% 
    set_names(c('variable', levels(tcga_mut$analysis_tbl$class)))
  
# Testing for differences -------
  
  insert_msg('Testing for differences in frequency between the subsets')
  
  tcga_mut$test <- tcga_mut$analysis_tbl %>% 
    compare_variables(variables = tcga_mut$top_mutations, 
                      split_factor = 'class', 
                      what = 'eff_size', 
                      types = 'cramer_v', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')
  
  tcga_mut$plots <- 
    list(variable = tcga_mut$test$variable, 
         plot_title = paste0('<b><em>', 
                             tcga_mut$test$variable, 
                             '</em>, TCGA</b>'), 
         plot_subtitle = tcga_mut$test$plot_cap) %>% 
    pmap(plot_variable, 
         tcga_mut$analysis_tbl, 
         type = 'stack', 
         scale = 'percent', 
         split_factor = 'class', 
         y_lab = '% of subset', 
         x_n_labs = TRUE, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_fill_manual(values = c(no = 'cornsilk', 
                                       yes = 'coral3'), 
                            labels = c(no = 'WT', 
                                       yes = 'mutated'), 
                            name = '') + 
          theme(plot.title = element_markdown(), 
                axis.title.x = element_blank())) %>% 
    set_names(tcga_mut$test$variable)
  
# Result table ------
  
  insert_msg('Result table')
  
  tcga_mut$result_tbl <- 
    left_join(tcga_mut$stats, 
              tcga_mut$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_tbl(lexicon = tcga$mutect_frequency, 
               key = 'gene', 
               value = 'gene', 
               rm_complete = TRUE) %>% 
    mutate(entrez_id = exchange(variable, 
                                dict = tcga$mutect_lexicon, 
                                key = 'gene_symbol', 
                                value = 'entrez_id')) %>% 
    full_rbind(cbind(variable = 'Samples, n', 
                     tcga_mut$strata_n %>% 
                       column_to_rownames('class') %>% 
                       t) %>% 
                 as.data.frame, .) %>% 
    as_tibble
  
# Similarity -------
  
  insert_msg('Similarity of the hormonal subsets')
  
  ## dimensionality reduction
  
  tcga_mut$similarity$red_obj <- tcga_mut$analysis_tbl %>% 
    map_dfc(function(x) if(is.factor(x)) as.numeric(x) - 1 else x) %>% 
    column_to_rownames('ID') %>% 
    select(-class, -sample_id) %>% 
    reduce_data(distance_method = 'jaccard', 
                kdim = 2, 
                red_fun = 'mds')
  
  ## plotting
  
  set.seed(1234)
  
  tcga_mut$similarity$plots <- 
    plot_similarity(tcga_mut$similarity$red_obj, 
                    class_assignment = tcga_mut$analysis_tbl[c('ID', 'class')], 
                    plot_title = 'Similarity of hormonal subsets, somatic mutations', 
                    plot_subtitle = '2D MDS, Jaccard distance', 
                    distance = 'Jaccard', 
                    min_max_similarity = TRUE, 
                    weighting_order = 1, 
                    net_theme = theme_void() + 
                      theme(plot.margin = globals$common_margin))
  
# END -------
  
  insert_tail()