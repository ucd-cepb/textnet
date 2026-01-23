# Exported function
#parse_text_trf

#' Parse text using spaCy transformer model with GPU support
#'
#' This function is specifically designed for the en_core_web_trf transformer model
#' with GPU acceleration. For CPU-only processing with en_core_web_lg, use parse_text().
#'
#' @param python_path filepath to python executable with spacy, cupy, and en_core_web_trf installed.
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
#' @return A data.frame of tokens with columns: doc_id, sentence_id, token_id, token, lemma, pos, tag, head_token_id, dep_rel, entity.
#'   Format is compatible with spacyr::spacy_parse output.
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stringi stri_replace_all_regex stri_escape_unicode
#' @importFrom pbapply pblapply
#' @importFrom utils data
#' @export

parse_text_trf <- function(python_path, keep_hyph_together=F, phrases_to_concatenate=NA,
                           concatenator="_", text_list, parsed_filenames,
                           overwrite=T, test=F, custom_entities = NULL, entity_ruler_patterns = NULL,
                           ruler_position = c("after", "before"), overwrite_ents = TRUE){

  if(!requireNamespace("jsonlite", quietly = T)){
    stop("Package 'jsonlite' must be installed to use this function.", call.=F)
  }

  # Input validation
  if(!is.character(python_path) || length(python_path) != 1) {
    stop("'python_path' must be a single character string")
  }

  if(!file.exists(python_path)) {
    stop(paste0("Python executable not found: ", python_path))
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

  # Find the Python parsing script
  script_path <- system.file("python", "parse_gpu.py", package = "textNet")
  if(script_path == "") {
    # Fallback for development: look relative to package source
    script_path <- file.path(dirname(dirname(sys.frame(1)$ofile)), "inst", "python", "parse_gpu.py")
    if(!file.exists(script_path)) {
      stop("Could not find parse_gpu.py script. Is the package installed correctly?")
    }
  }

  # Process text
  pages <- unlist(text_list)

  if(is.null(names(text_list)) | "" %in% names(text_list)){
    stop("text_list must be a named list")
  }
  file_ids <- unlist(sapply(1:length(text_list), function(q) rep(names(text_list[q]),length(text_list[[q]]))))

  phrases_to_concatenate <- phrases_to_concatenate[stringr::str_detect(phrases_to_concatenate,"\\s")]

  if(length(phrases_to_concatenate) > 1 || !is.na(phrases_to_concatenate)){
    phrases_grouped <- gsub("\\s+", concatenator, x = phrases_to_concatenate)
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern = phrases_to_concatenate,
                                      replacement = phrases_grouped,
                                      vectorize= F)
    })
  }

  if(keep_hyph_together){
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern= "(?<=\\w)[\\-\\u2013](?=\\w)", replacement ="_", vectorize=F)
    })
  }

  unique_files <- base::unique(file_ids)
  all_parsed <- vector(mode="list",length=length(unique_files))

  # Create temp directory for communication with Python
  temp_dir <- tempdir()

  # Write entity ruler patterns if provided
  entity_ruler_file <- NULL
  if(!is.null(entity_ruler_patterns)){
    entity_ruler_file <- file.path(temp_dir, "entity_ruler_patterns.json")
    jsonlite::write_json(entity_ruler_patterns, entity_ruler_file, auto_unbox = TRUE)
  }

  for (m in 1:length(unique_files)){
    file_exists <- file.exists(parsed_filenames[m])
    should_parse <- overwrite || !file_exists || test
    should_save <- overwrite || !file_exists

    if(should_parse){
      single_plan_text <- unlist(pages[file_ids==unique_files[m]])

      # Create doc_ids for each text element (text1, text2, etc.)
      text_doc_ids <- paste0("text", seq_along(single_plan_text))

      # Write input JSON
      input_file <- file.path(temp_dir, paste0("input_", m, ".json"))
      output_file <- file.path(temp_dir, paste0("output_", m, ".json"))

      input_data <- list(
        texts = as.list(single_plan_text),
        doc_ids = as.list(text_doc_ids)
      )
      jsonlite::write_json(input_data, input_file, auto_unbox = TRUE)

      # Build command
      cmd_args <- c(
        shQuote(script_path),
        "--input", shQuote(input_file),
        "--output", shQuote(output_file),
        "--ruler-position", ruler_position
      )

      if(overwrite_ents) {
        cmd_args <- c(cmd_args, "--overwrite-ents")
      }

      if(!is.null(entity_ruler_file)) {
        cmd_args <- c(cmd_args, "--entity-ruler", shQuote(entity_ruler_file))
      }

      cmd <- paste(shQuote(python_path), paste(cmd_args, collapse = " "))

      # Run Python script
      message(paste0("Parsing document ", m, " of ", length(unique_files), ": ", unique_files[m]))
      result <- system(cmd, intern = FALSE, wait = TRUE)

      if(result != 0) {
        stop(paste0("Python parsing failed for document ", unique_files[m],
                    ". Check that cupy, spacy, and en_core_web_trf are installed."))
      }

      # Read output
      if(!file.exists(output_file)) {
        stop(paste0("Output file not created for document ", unique_files[m]))
      }

      output_data <- jsonlite::read_json(output_file, simplifyVector = TRUE)

      parsedtxt <- as.data.frame(output_data$tokens)
      nounphrases <- as.data.frame(output_data$nounphrases)

      # Attach nounphrases as attribute (matches spacyr behavior)
      attr(parsedtxt, "nounphrases") <- nounphrases

      # Clean up temp files
      unlink(input_file)
      unlink(output_file)

      lettertokens <- parsedtxt$token[stringr::str_detect(parsedtxt$token, "[a-zA-Z]")]
      lettertokensunicodeescaped <- stringi::stri_escape_unicode(lettertokens)
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
          if(length(all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity)>0){
            all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity <- paste0(names(custom_entities[k]), "_B")
          }
        }
      }

      if(should_save){
        saveRDS(all_parsed[[m]], parsed_filenames[m])
      }
    } else {
      print(paste0("Skipping parsed_filenames[",m,"] - file already exists (set overwrite=T to reparse or test=T to test without saving)"))
      all_parsed[[m]] <- NULL
    }
  }

  # Clean up entity ruler file
  if(!is.null(entity_ruler_file) && file.exists(entity_ruler_file)) {
    unlink(entity_ruler_file)
  }

  return(all_parsed)
}
