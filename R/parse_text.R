# Exported function
#parse_text

#' Creates an edgelist and nodelist for each document
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param keep_hyph_together Set to true to replace hyphens within a single word with underscores. Defaults to false.
#' @param phrases_to_concatenate character vector of phrases, in which each element is a string consisting of tokens separated by spaces. These are replaced with their concatenated version in order, from left to right. It is suggested that the most specific phrases, with the most words, are arranged at the left.
#' @param concatenator This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.
#' @param pages This is a character vector, in which each element is a string that represents one page of text
#' @param file_ids This is a vector defining which pages are associated with which documents. The length is equal to the number of total pages. 
#' @param parsed_filenames This is a character vector in which each element represents a filepath associated with its respective document. 
#' The parsed data will be exported to these files.
#' @param overwrite A boolean. Whether to overwrite existing files
#' @import data.table
#' @export

parse_text <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA, 
                              concatenator="_", pages, file_ids, parsed_filenames,
                              overwrite=T){
  
  #prerequisites: step 1, install python
  #step 2, install miniconda from https://conda.io/miniconda.html
  #step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  reticulate::py_config()
  #spacy_install()
  #spacy_download_langmodel(model = 'en_core_web_lg')
  spacyr::spacy_initialize(model = "en_core_web_lg")
  
  #generate phrases defaults to false, since it appears spaCy has a more difficult time 
  #identifying proper name phrases linked by underscore as entities
  if(length(phrases_to_concatenate) > 1 || !is.na(phrases_to_concatenate)){
    phrases_grouped <- gsub("\\s+", concatenator, x = phrases_to_concatenate)
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern = phrases_to_concatenate,
                             replacement = phrases_grouped,
                             vectorize= F)
    })
  }
  #keep_hyph_together defaults to false, since it appears spaCy has a more difficult time 
  #identifying proper name phrases linked by underscore as entities
  if(keep_hyph_together){
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern= "(?<=\\w)[\\-\\u2013](?=\\w)", replacement ="_", vectorize=F)
    })
  }
  
  unique_files <- base::unique(file_ids)
  all_parsed <- vector(mode="list",length=length(unique_files))
  for (m in 1:length(unique_files)){
      if(overwrite==T | (overwrite==F & !(file.exists(parsed_filenames[m])))){
        
          single_plan_text <- unlist(pages[file_ids==unique_files[m]])
          
          parsedtxt <- spacyr::spacy_parse(single_plan_text,
                                   pos = T,
                                   tag = T,
                                   lemma = T,
                                   entity = T,
                                   dependency = T,
                                   nounphrase = T)
          saveRDS(parsedtxt, parsed_filenames[m])
          lettertokens <- parsedtxt$token[stringr::str_detect(parsedtxt$token, "[a-zA-Z]")]
          lettertokensunicodeescaped <- stringi::stri_escape_unicode(lettertokens)
          utils::data(eng_words)
          pctlettersineng <- sum(lettertokensunicodeescaped %in% eng_words)/length(lettertokensunicodeescaped) 
          
          if(pctlettersineng<0.5){
            warning("Fewer than 50% of letter-containing tokens in this PDF are English words. This may be due to a PDF formatting issue. It is not recommended to use textnet_extract on this pdf.")
          }
          print(paste0("parsing complete: ",unique_files[m]))
        
        
      }
    else{
      print(paste0("parsed_filenames[",m,"] already exists. Using existing file as element ",m," of the list returned by this function."))
      parsedtxt <- readRDS(parsed_filenames[m])
    }
      all_parsed[[m]] <- parsedtxt
  }
  spacyr::spacy_finalize()
  return(all_parsed)
}

