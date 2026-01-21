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
#' @param test A boolean. If TRUE and overwrite is FALSE, will still run parsing but won't save results. Useful for testing parsing on a subset without affecting saved files.
#' @param custom_entities A named list. This does not overwrite the entity determination of the NLP engine, but rather catches user-defined entities that are not otherwise detected by the engine. Best used in combination with phrases_to_concatenate, since the custom entity label will only be applied if the entire token matches the definition. Does not search multiple consecutive tokens to define a match. These will be applied to all documents.
#' @param entity_ruler_patterns A list of pattern dictionaries for spaCy's EntityRuler. Each pattern should be a named list with 'label' and 'pattern' elements. The 'pattern' can be a string for exact matches or a list of token attributes for complex patterns. If provided, these patterns will be added to spaCy's NLP pipeline. Use entity_specify() to create patterns from a dictionary.
#' @param ruler_position A string controlling where EntityRuler is placed in the pipeline: "after" (default) places it after NER so custom patterns can override, "before" places it before NER so NER has final say.
#' @param overwrite_ents A boolean. If TRUE (default), EntityRuler patterns override NER's entity assignments for overlapping spans. If FALSE, EntityRuler only fills gaps where NER found nothing.
#' @param model A string referencing a spaCy model, currently supports two options: English large and English transformer, see https://spacy.io/models/en
#' @param use_gpu A string controlling GPU usage: "auto" (default) tries GPU if available and falls back to CPU, "cpu" forces CPU usage, "gpu" requires GPU (will error if unavailable). Transformer models (e.g., en_core_web_trf) benefit from GPU acceleration but work on CPU.
#' @return A data.frame of tokens. For more information on the format, see the spacyr::spacy_parse help file
#' @importFrom data.table setDT
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stringi stri_replace_all_regex stri_escape_unicode
#' @importFrom pbapply pblapply
#' @importFrom utils data
#' @export

parse_text <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA,
                              concatenator="_", text_list, parsed_filenames,
                              overwrite=T, test=F, custom_entities = NULL, entity_ruler_patterns = NULL,
                              ruler_position = c("after", "before"), overwrite_ents = TRUE,
                              model = c("en_core_web_lg",'en_core_web_trf'),
                              use_gpu = c("auto", "cpu", "gpu")){
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

  if(!is.logical(test) || length(test) != 1) {
    stop("'test' must be a single logical value")
  }

  # Resolve model, use_gpu, and ruler_position from choices
  model <- match.arg(model)
  use_gpu <- match.arg(use_gpu)
  ruler_position <- match.arg(ruler_position)

  if(!is.logical(overwrite_ents) || length(overwrite_ents) != 1) {
    stop("'overwrite_ents' must be a single logical value")
  }

  if(!is.null(custom_entities)){
    if(!is.list(custom_entities) | is.null(names(custom_entities)) |
       "" %in% names(custom_entities)){
      stop("custom_entities must be a named list.")
    }
  }
  
  if(!is.null(entity_ruler_patterns)){
    if(!is.list(entity_ruler_patterns)){
      stop("entity_ruler_patterns must be a list.")
    }
    for(i in 1:length(entity_ruler_patterns)){
      if(!is.list(entity_ruler_patterns[[i]]) ||
         !all(c("label", "pattern") %in% names(entity_ruler_patterns[[i]]))){
        stop("Each element in entity_ruler_patterns must be a list with 'label' and 'pattern' elements.")
      }
    }
  }

  # Fix OpenMP library conflict (common on macOS with conda)
  # This prevents crashes from multiple OpenMP runtimes being loaded
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Prevent OpenMP thread conflicts between R packages (e.g., data.table) and PyTorch
  # Only needed for transformer models which use PyTorch
  # This must be set before Python is initialized
  if(grepl("trf", model)){
    Sys.setenv(OMP_NUM_THREADS = "1")
    message("Note: Setting OMP_NUM_THREADS=1 to prevent OpenMP conflicts with transformer model. This may reduce data.table performance during this session.")
  }

  # Set CUDA_VISIBLE_DEVICES BEFORE Python initialization for CPU mode
  # This must happen before reticulate::py_config() to prevent PyTorch/CUDA crashes
  if(use_gpu == "cpu"){
    Sys.setenv(CUDA_VISIBLE_DEVICES = "")
  } else if(use_gpu == "auto"){
    # For auto mode, also set CUDA_VISIBLE_DEVICES if we detect transformer model
    # This prevents crashes when GPU is not properly configured
    if(grepl("trf", model)){
      Sys.setenv(CUDA_VISIBLE_DEVICES = "")
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

  # Configure GPU/CPU usage for spaCy
  # Must be done BEFORE spacyr::spacy_initialize() to prevent crashes
  if(requireNamespace("reticulate", quietly = T)){
    spacy <- reticulate::import("spacy")

    if(use_gpu == "cpu"){
      # Force CPU mode
      spacy$require_cpu()
      message("spaCy configured to use CPU")
    } else if(use_gpu == "gpu"){
      # Require GPU - will error if unavailable
      gpu_available <- tryCatch({
        spacy$require_gpu()
        TRUE
      }, error = function(e){
        stop("GPU requested but not available. Set use_gpu='auto' or use_gpu='cpu' to use CPU instead.")
      })
      message("spaCy configured to use GPU")
    } else {
      # Auto mode: try GPU, fall back to CPU
      # Check if CUDA is actually available via PyTorch
      gpu_available <- tryCatch({
        torch <- reticulate::import("torch")
        torch$cuda$is_available()
      }, error = function(e){
        FALSE
      })

      if(gpu_available){
        spacy$prefer_gpu()
        message("spaCy configured to use GPU")
      } else {
        Sys.setenv(CUDA_VISIBLE_DEVICES = "")
        spacy$require_cpu()
        message("GPU not available, spaCy configured to use CPU")
      }
    }
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

  # Configure EntityRuler if patterns are provided
  if(!is.null(entity_ruler_patterns)){
    if(requireNamespace("reticulate", quietly = T)){
      tryCatch({
        # Access spacyr's nlp object from Python's global namespace
        # spacyr stores the nlp object there after initialization
        nlp <- reticulate::py_eval("nlp")

        # Determine position argument based on ruler_position
        if(ruler_position == "after"){
          position_arg <- list(after = "ner")
        } else {
          position_arg <- list(before = "ner")
        }

        # Create EntityRuler with appropriate settings
        # overwrite_ents controls whether EntityRuler can override NER's decisions
        ruler_config <- list(overwrite_ents = overwrite_ents)

        # Add EntityRuler to the pipeline
        ruler <- do.call(nlp$add_pipe,
                         c(list("entity_ruler"),
                           position_arg,
                           list(config = ruler_config)))
        ruler$add_patterns(entity_ruler_patterns)

        message(paste0("EntityRuler configured: position=", ruler_position,
                      ", overwrite_ents=", overwrite_ents,
                      ", ", length(entity_ruler_patterns), " patterns loaded"))

      }, error = function(e){
        warning(paste0("Failed to configure EntityRuler: ", e$message, ". Proceeding without EntityRuler."))
      })
    }
  }
  
  
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
      file_exists <- file.exists(parsed_filenames[m])
      should_parse <- overwrite || !file_exists || test
      should_save <- overwrite || !file_exists

      if(should_parse){
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

          if(test && file_exists && !overwrite){
            print(paste0("parsing complete (test mode, not saved): ",unique_files[m]))
          } else {
            print(paste0("parsing complete: ",unique_files[m]))
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

          if(should_save){
            saveRDS(all_parsed[[m]], parsed_filenames[m])
          }
      } else {
          # overwrite=F, file exists, test=F: skip entirely
          print(paste0("Skipping parsed_filenames[",m,"] - file already exists (set overwrite=T to reparse or test=T to test without saving)"))
          all_parsed[[m]] <- NULL
      }
  }
  if(requireNamespace("spacyr", quietly = T)){
    spacyr::spacy_finalize()
  }
  
  
  return(all_parsed)
}
