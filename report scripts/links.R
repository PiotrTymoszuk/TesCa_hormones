# Handling of common links in the Rmarkdown project

  insert_head()
  
# container ------
  
  proj_links <- list()
  
# Links to development packages --------
  
  insert_msg('Links to development packages')
  
  proj_links <- 
    list(ExDA = 'https://github.com/PiotrTymoszuk/ExDA', 
         trafo = 'https://github.com/PiotrTymoszuk/trafo', 
         figur = 'https://github.com/PiotrTymoszuk/figur', 
         microViz = 'https://github.com/PiotrTymoszuk/microViz', 
         clustTools = 'https://github.com/PiotrTymoszuk/clustTools', 
         biggrExtra = 'https://github.com/PiotrTymoszuk/biggrExtra', 
         xena = 'https://github.com/PiotrTymoszuk/xena', 
         gseaTools = 'https://github.com/PiotrTymoszuk/gseaTools') %>% 
    compress(names_to = 'ref_name', 
             values_to = 'x')
  
  proj_links <- proj_links %>% 
    pmap(mdlink) %>% 
    set_names(proj_links$ref_name)
  
# END ------
  
  insert_tail()
