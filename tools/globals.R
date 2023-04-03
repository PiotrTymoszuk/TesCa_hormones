# Project globals

# tools -----

  library(plyr)
  library(tidyverse)
  library(trafo)

# container -----

  globals <- list()

# graphic globals ------

  globals$common_text <- element_text(size = 8,
                                      face = 'plain',
                                      color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')
  
  globals$common_theme <- theme_classic() +
    theme(axis.text = globals$common_text,
          axis.title = globals$common_text,
          plot.title = element_text(size = 8,
                                    face = 'bold'),
          plot.subtitle = globals$common_text,
          plot.tag = element_text(size = 8,
                                  face = 'plain',
                                  color = 'black',
                                  hjust = 0,
                                  vjust = 1),
          plot.tag.position = 'bottom',
          legend.text = globals$common_text,
          legend.title = globals$common_text,
          strip.text = globals$common_text,
          strip.background = element_rect(fill = 'gray95',
                                          color = 'gray80'),
          plot.margin = globals$common_margin,
          panel.grid.major = element_line(color = 'gray90'))
  
# END -------