# Analysis of patient's and variable data missingness

  insert_head()
  
# container -----
  
  missing <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  missing$variables <- ex_globals$variables
  
  ## analysis matrix with logical indexes of missing data
  ## 1: missing, 0: present
  
  missing$analysis_mtx <- tesca$data %>% 
    select(all_of(missing$variables)) %>% 
    map_dfc(is.na) %>% 
    map_dfc(as.numeric) %>% 
    as.matrix %>% 
    set_rownames(tesca$data$ID)
  
  ## common subtitle with the participant and variable number
  
  missing$cmm_caption <- 
    paste0('Patients: n = ', 
           nrow(missing$analysis_mtx), 
           ', variables: n = ', 
           ncol(missing$analysis_mtx))

# missingness per individual --------
  
  insert_msg('Missingness per individual')
  
  ## counts of missing variables per patient
  
  missing$patient$stats <- missing$analysis_mtx %>% 
    rowSums %>% 
    compress(names_to = 'ID', 
             values_to = 'n_missing') %>% 
    mutate(perc_missing = n_missing/ncol(missing$analysis_mtx) * 100)
  
  ## plotting: bar plot and histogram
  
  missing$patient$plot <- missing$patient$stats %>% 
    ggplot(aes(x = perc_missing, 
               y = reorder(ID, perc_missing))) + 
    geom_bar(stat = 'identity', 
             color = NA, 
             fill = 'steelblue') + 
    scale_x_continuous(breaks = seq(0, 50, by = 10)) + 
    globals$common_theme + 
    theme(panel.grid.major.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()) + 
    labs(title = 'Missing variables per participant', 
         y = 'Participant', 
         x = '% of missing variables', 
         subtitle = missing$cmm_caption)
  
  missing$patient$histogram <- missing$patient$stats %>% 
    ggplot(aes(x = perc_missing)) + 
    geom_histogram(bins = 20, 
                   fill = 'steelblue', 
                   color = 'black') + 
    globals$common_theme + 
    labs(title = 'Missing variables per participant', 
         y = 'participant number, n', 
         x = '% of missing variables', 
         subtitle = missing$cmm_caption)
  
# missngness per variable -------
  
  insert_msg('Missingness per variable')
  
  ## number of missing cells per variable
  
  missing$variable <- list()
  
  missing$variable$stats <- missing$analysis_mtx %>% 
    colSums %>% 
    compress(names_to = 'variable', 
             values_to = 'n_missing') %>% 
    mutate(perc_missing = n_missing/nrow(missing$analysis_mtx) * 100)
  
  ## plotting
  
  missing$variable$plot <- missing$variable$stats %>% 
    ggplot(aes(x = perc_missing, 
               y = reorder(variable, perc_missing))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'darkolivegreen4') +
    scale_y_discrete(labels = exchange(missing$variables, 
                                       dict = tesca$lexicon)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Missing records per variable', 
         subtitle = missing$cmm_caption,
         x = '% of missing records')
  
# END ------
  
  insert_tail()