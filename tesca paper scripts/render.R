# Renders the report

  insert_head()
  
# Bibliography -----
  
  insert_msg('Reading the bibliography')
  
  tesca_bib <- read_bib('./tesca paper/markdown/tesca.bib') %>% 
    as_mdbib
  
# Rendering the report ------
  
  insert_msg('Rendering the report')
  
  render('./tesca paper/markdown/figures_and_tables.Rmd', 
         output_format = word_document2(reference_docx = 'ms_template.docx', 
                                        number_sections = FALSE), 
         output_dir = './tesca paper')
  
# Rendering the supplementary material ------
  
  insert_msg('Rendering the supplementary material')
  
  render('./tesca paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(reference_docx = 'ms_template.docx', 
                                        number_sections = FALSE), 
         output_dir = './tesca paper')
  
# END ----
  
  insert_tail()