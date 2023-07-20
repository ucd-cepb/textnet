# Exported function
#generate_networks

#' Creates an edgelist and nodelist for each document
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param generate_phrases 
#' @param pages
#' @param file_ids
#' @param parsed_filenames
#' @param parse_from_file
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

generate_networks <- function(ret_path, generate_phrases=F, keep_hyph_together=F, phrases_to_concatenate=NA, 
                              concatenator="_", pages, file_ids, parsed_filenames, 
                              nodeedge_filenames, parse_from_file=F){
  #source('R/custom_entity_extract2.R')
  
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
  if(generate_phrases){
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
      custom_entity_extract(parsedtxt,concatenator,file = nodeedge_filenames[m], 
                             return_to_memory=F, keep_incomplete_edges=T)
  }
  spacy_finalize()
  
}




















