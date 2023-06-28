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
#' @import network
#' @import data.table
#' @export

generate_networks <- function(ret_path, generate_phrases, pages, file_ids, parsed_filenames, parse_from_file=F){
  source('R/generate_proper_names.R')
  source('custom_entity_extract.R')
  
  #prerequisites: step 1, install python
  #step 2, install miniconda from https://conda.io/miniconda.html
  #step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  py_config()
  #spacy_install()
  #spacy_download_langmodel(model = 'en_core_web_lg')
  spacy_initialize(model = "en_core_web_lg")
  
  if(generate_phrases){
    pr_names_grouped <- generate_proper_names(underscore = T,to_lower=F)
    
    pr_names_sep <- generate_proper_names(underscore = F,to_lower=F)
    
    pages <- pblapply(1:length(pages), function(i){
      stri_replace_all_regex(pages[i], pattern = pr_names_sep,
                             replacement = pr_names_grouped,
                             vectorize= F)
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
      }
      else{
        #parse_from_file==T
        parsedtxt <- readRDS(parsed_filenames[m])
      }
      custom_entity_extract2(parsedtxt,concatenator="_",file_ext = unique_files[m])
  }
  spacy_finalize()
  
}




















