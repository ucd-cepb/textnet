# Exported function
#generate_networks

#' Creates an edgelist and nodelist for each document
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param keep_hyph_together Set to true to replace hyphens within a single word with underscores. Defaults to false.
#' @param phrases_to_concatenate character vector of phrases, in which each element is a string consisting of tokens separated by spaces.
#' @param concatenator This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.
#' @param pages This is a character vector, in which each element is a string that represents one page of text
#' @param file_ids This is a vector defining which pages are associated with which documents. The length is equal to the number of total pages. 
#' @param parsed_filenames This is a character vector in which each element represents a filepath associated with its respective document. If 
#' parse_from_file is T, the files located at these file paths will be read in. If parse_from_file is F, the parsed data 
#' will be exported to these files.
#' @param nodeedge_filenames This is a character vector in which each element represents a filepath associated with its respective document.
#' A list consisting of a nodelist, edgelist, verblist, and list of appositives found in the corresponding document will be saved for each document.
#' @param parse_from_file This is a logical vector. T denotes that the parsed_filenames should be used to locate existing 
#' files of parsed text. F indicates that the parsing should be carried out and that the resulting files should be 
#' saved to the filepaths given in parsed_filenames.
#' @import reticulate
#' @import spacyr
#' @import magrittr
#' @import dplyr
#' @import tidytext
#' @import quanteda
#' @import pbapply
#' @import stringr
#' @import stringi
#' @import network
#' @import data.table
#' @export

generate_networks <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA, 
                              concatenator="_", pages, file_ids, parsed_filenames, 
                              nodeedge_filenames, parse_from_file=F, cl=1,  
                              keep_entities = c('ORG','GPE','PERSON'),
                              overwrite=T){
  
  #prerequisites: step 1, install python
  #step 2, install miniconda from https://conda.io/miniconda.html
  #step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  py_config()
  #spacy_install()
  #spacy_download_langmodel(model = 'en_core_web_lg')
  spacy_initialize(model = "en_core_web_lg")
  
  #generate phrases defaults to false, since it appears spaCy has a more difficult time 
  #identifying proper name phrases linked by underscore as entities
  if(!is.na(phrases_to_concatenate)){
    phrases_grouped <- gsub("\\s+", concatenator, x = phrases_to_concatenate)
    pages <- pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern = phrases_to_concatenate,
                             replacement = phrases_grouped,
                             vectorize= F)
    })
  }
  #keep_hyph_together defaults to false, since it appears spaCy has a more difficult time 
  #identifying proper name phrases linked by underscore as entities
  if(keep_hyph_together){
    pages <- pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern= "(?<=\\w)[\\-–](?=\\w)", replacement ="_", vectorize=F)
    })
  }
  
  unique_files <- unique(file_ids)
  for (m in 1:length(unique_files)){
      if(overwrite==T | (overwrite==F & !(file.exists(nodeedge_filenames[m])))){
        if(parse_from_file==F){
          single_plan_text <- unlist(pages[file_ids==unique_files[m]])
          
          parsedtxt <- spacy_parse(single_plan_text,
                                   pos = T,
                                   tag = T,
                                   lemma = T,
                                   entity = T,
                                   dependency = T,
                                   nounphrase = T)
          saveRDS(parsedtxt, parsed_filenames[m])
          print(paste0("parsing complete: ",unique_files[m]))
        }else{
          #parse_from_file==T
          parsedtxt <- readRDS(parsed_filenames[m])
        }
        textnet_extract(parsedtxt,concatenator,file = nodeedge_filenames[m],cl,keep_entities, 
                              return_to_memory=F, keep_incomplete_edges=T)
      }
      
  }
  spacy_finalize()
  
}




















