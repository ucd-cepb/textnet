# Exported function
#parse_text

#' Creates an edgelist and nodelist for each document
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param keep_hyph_together Set to true to replace hyphens within a single word with underscores. Defaults to false.
#' @param phrases_to_concatenate character vector of phrases, in which each element is a string consisting of tokens separated by spaces. These are replaced with their concatenated version in order, from left to right. It is suggested that the most specific phrases, with the most words, are arranged at the left.
#' @param concatenator This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.
#' @param text_list This is a named list, an object of the type resulting from pdf_clean, in which each list element is a document, and each string within a list element represents the text on one page
#' @param parsed_filenames This is a character vector in which each element represents a filepath associated with its respective document. 
#' The parsed data will be exported to these files.
#' @param overwrite A boolean. Whether to overwrite existing files
#' @param custom_entities A named list. This does not overwrite the entity determination of the NLP engine, but rather catches user-defined entities that are not otherwise detected by the engine. Best used in combination with phrases_to_concatenate, since the custom entity label will only be applied if the entire token matches the definition. Does not search multiple consecutive tokens to define a match. These will be applied to all documents.
#' @return A data.frame of tokens. For more information on the format, see the spacyr::spacy_parse help file
#' @importFrom data.table setDT
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stringi stri_replace_all_regex stri_escape_unicode
#' @importFrom pbapply pblapply
#' @importFrom utils data
#' @export

parse_text <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA, 
                              concatenator="_", text_list, parsed_filenames,
                              overwrite=T, custom_entities = NULL, model = "en_core_web_lg"){
  if(!requireNamespace("spacyr", quietly = T)){
    stop("Package 'spacyr' must be installed to use this function.",
         call.=F)
  }
  # Input validation
  if(!is.character(ret_path) || length(ret_path) != 1) {
    stop("'ret_path' must be a single character string")
  }
  
  if(!is.logical(keep_hyph_together) || length(keep_hyph_together) != 1) {
    stop("'keep_hyph_together' must be a single logical value")
  }
  
  if(!is.character(phrases_to_concatenate) && !is.na(phrases_to_concatenate)) {
    stop("'phrases_to_concatenate' must be either NA or a character vector")
  }
  
  if(!is.character(concatenator) || length(concatenator) != 1) {
    stop("'concatenator' must be a single character string")
  }
  
  if(!is.list(text_list)) {
    stop("'text_list' must be a list")
  }
  
  if(!is.character(parsed_filenames)) {
    stop("'parsed_filenames' must be a character vector")
  }
  
  if(!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be a single logical value")
  }
  
  if(!is.null(custom_entities)){
    if(!is.list(custom_entities) | is.null(names(custom_entities)) |
       "" %in% names(custom_entities)){
      stop("custom_entities must be a named list.")
    }
  }
  #prerequisites: step 1, install python
  #step 2, if necessary: install miniconda from https://conda.io/miniconda.html
  #step 3, if necessary: install virtualenv, numpy, conda, and spacy
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  if(!requireNamespace("reticulate", quietly = T)){
    stop("Package 'reticulate' must be installed to use this function.",
         call.=F)
  }
  if(requireNamespace("reticulate", quietly = T)){
    reticulate::py_config()
  }
  #spacy_install()
  #spacy_download_langmodel(lang_mode = 'en_core_web_lg')
  if(!stringr::str_detect(model, "^en_")){
    warning("This package was developed and tested on English texts. Use of this package to extract event networks in other languages may yield unexpected results.")
  }
  tryCatch(
    if(requireNamespace("spacyr", quietly = T)){
      spacyr::spacy_initialize(model = model)
    },
           error = function(e){
             stop(paste0("Model ", model, " is not installed. Install models via spacyr::spacy_download_langmodel('", model, "')"))
           })
  
  
  #pages is a character vector, in which each element is a string that represents one page of text
  pages <- unlist(text_list)
  
  #file_ids is a vector defining which pages are associated with which documents. The length is equal to the number of total pages.
  if(is.null(names(text_list)) | "" %in% names(text_list)){
    stop("text_list must be a named list")
  }
  file_ids <- unlist(sapply(1:length(text_list), function(q) rep(names(text_list[q]),length(text_list[[q]]))))
  
  
  #automatically gets rid of phrases without a space
  phrases_to_concatenate <- phrases_to_concatenate[str_detect(phrases_to_concatenate,"\\s")]
  
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
          
          if(requireNamespace("spacyr", quietly = T)){
            parsedtxt <- spacyr::spacy_parse(single_plan_text,
                                             pos = T,
                                             tag = T,
                                             lemma = T,
                                             entity = T,
                                             dependency = T,
                                             nounphrase = T)
          }
          
          lettertokens <- parsedtxt$token[str_detect(parsedtxt$token, "[a-zA-Z]")]
          lettertokensunicodeescaped <- stri_escape_unicode(lettertokens)
          utils::data(eng_words)
          pctlettersineng <- sum(lettertokensunicodeescaped %in% eng_words)/length(lettertokensunicodeescaped) 
          
          if(pctlettersineng<0.5){
            warning(paste0("Fewer than 50% of letter-containing tokens in the document ", unique_files[m] ," are English words."))
          }
          print(paste0("parsing complete: ",unique_files[m]))
        
        
      }
    else{
      print(paste0("parsed_filenames[",m,"] already exists. Using existing file as element ",m," of the list returned by this function."))
      parsedtxt <- readRDS(parsed_filenames[m])
    }
      all_parsed[[m]] <- parsedtxt
      if(!is.null(custom_entities)){
        for(k in 1:length(custom_entities)){
          custom_entities[[k]] <- stringr::str_replace_all(custom_entities[[k]] ,"\\s",concatenator)
          #this if condition prevents an error if the custom entity doesn't exist as a token in the doc
          if(length(all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity)>0){
            all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity <- paste0(names(custom_entities[k]), "_B")
          }
        }
      }
      saveRDS(all_parsed[[m]], parsed_filenames[m])
  }
  if(requireNamespace("spacyr", quietly = T)){
    spacyr::spacy_finalize()
  }
  
  
  return(all_parsed)
}
