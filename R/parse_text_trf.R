#' Parse text using spaCy transformer model
#'
#' This function parses text using spaCy's en_core_web_trf transformer model.
#' Unlike the CPU version (parse_text()), this function uses a pure Python
#' implementation that allows PyTorch to handle GPU acceleration automatically.
#'
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param keep_hyph_together Set to true to replace hyphens within a single word with underscores. Defaults to false.
#' @param phrases_to_concatenate character vector of phrases, in which each element is a string consisting of tokens separated by spaces. These are replaced with their concatenated version in order, from left to right. It is suggested that the most specific phrases, with the most words, are arranged at the left.
#' @param concatenator This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.
#' @param text_list This is a named list, an object of the type resulting from pdf_clean, in which each list element is a document, and each string within a list element represents the text on one page
#' @param parsed_filenames This is a character vector in which each element represents a filepath associated with its respective document.
#' The parsed data will be exported to these files (as .parquet files for efficiency).
#' @param overwrite A boolean. Whether to overwrite existing files
#' @param test A boolean. If TRUE and overwrite is FALSE, will still run parsing but won't save results. Useful for testing parsing on a subset without affecting saved files.
#' @param custom_entities A named list. This does not overwrite the entity determination of the NLP engine, but rather catches user-defined entities that are not otherwise detected by the engine. Best used in combination with phrases_to_concatenate, since the custom entity label will only be applied if the entire token matches the definition. Does not search multiple consecutive tokens to define a match. These will be applied to all documents.
#' @param entity_ruler_patterns A list of pattern dictionaries for spaCy's EntityRuler. Each pattern should be a named list with 'label' and 'pattern' elements. The 'pattern' can be a string for exact matches or a list of token attributes for complex patterns. If provided, these patterns will be added to spaCy's NLP pipeline. Use entity_specify() to create patterns from a dictionary.
#' @param ruler_position A string controlling where EntityRuler is placed in the pipeline: "after" (default) places it after NER so custom patterns can override, "before" places it before NER so NER has final say.
#' @param overwrite_ents A boolean. If TRUE (default), EntityRuler patterns override NER's entity assignments for overlapping spans. If FALSE, EntityRuler only fills gaps where NER found nothing.
#' @param batch_size Integer. Batch size for spaCy's nlp.pipe() processing. Default 50.
#' @return A list of data.frames of tokens, one per document. Format matches spacyr::spacy_parse output.
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stringi stri_replace_all_regex stri_escape_unicode
#' @importFrom pbapply pblapply
#' @importFrom utils data
#' @export

parse_text_trf <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA,
                           concatenator="_", text_list, parsed_filenames,
                           overwrite=T, test=F, custom_entities = NULL, entity_ruler_patterns = NULL,
                           ruler_position = c("after", "before"), overwrite_ents = TRUE,
                           batch_size = 50L){

  # Input validation
  if(!is.character(ret_path) || length(ret_path) != 1) {
    stop("'ret_path' must be a single character string")
  }

  if(!is.logical(keep_hyph_together) || length(keep_hyph_together) != 1) {
    stop("'keep_hyph_together' must be a single logical value")
  }

  if(!is.character(phrases_to_concatenate) && !all(is.na(phrases_to_concatenate))) {
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

  if(is.null(names(text_list)) | "" %in% names(text_list)){
    stop("text_list must be a named list")
  }

  # Check for duckdb package (needed for Parquet I/O)
  if(!requireNamespace("duckdb", quietly = T)){
    stop("Package 'duckdb' must be installed to use this function (for Parquet file I/O).",
         call.=F)
  }

  # Fix OpenMP library conflict (common on macOS with conda)
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Prevent OpenMP thread conflicts between R packages (e.g., data.table) and PyTorch
  Sys.setenv(OMP_NUM_THREADS = "1")
  message("Note: Setting OMP_NUM_THREADS=1 to prevent OpenMP conflicts with transformer model.")

  # Set up Python environment
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  if(!requireNamespace("reticulate", quietly = T)){
    stop("Package 'reticulate' must be installed to use this function.",
         call.=F)
  }
  reticulate::py_config()

  # Load the Python parsing module
  python_module_path <- system.file("python", "parse_text_trf.py", package = "textNet")
  if(python_module_path == ""){
    stop("Could not find parse_text_trf.py in textNet package. Please reinstall the package.")
  }

  # Source the Python module (convert=FALSE to handle conversion explicitly)
  reticulate::source_python(python_module_path, convert = FALSE)

  # Initialize the parser
  message("Initializing spaCy transformer model...")
  tryCatch({
    initialize(
      model = "en_core_web_trf",
      entity_ruler_patterns = entity_ruler_patterns,
      ruler_position = ruler_position,
      overwrite_ents = overwrite_ents
    )
  }, error = function(e){
    if(grepl("Can't find model", e$message, ignore.case = TRUE) ||
       grepl("not found", e$message, ignore.case = TRUE)){
      stop("Model en_core_web_trf is not installed. Install via: python -m spacy download en_core_web_trf")
    } else {
      stop(paste0("Failed to initialize spaCy: ", e$message))
    }
  })

  message("spaCy transformer model initialized")

  # Process text - flatten pages and track file IDs
  pages <- unname(unlist(text_list))
  file_ids <- unlist(sapply(1:length(text_list), function(q) rep(names(text_list[q]),length(text_list[[q]]))))

  # Apply phrase concatenation
  phrases_to_concatenate <- phrases_to_concatenate[stringr::str_detect(phrases_to_concatenate,"\\s")]

  if(length(phrases_to_concatenate) > 1 || !all(is.na(phrases_to_concatenate))){
    phrases_grouped <- gsub("\\s+", concatenator, x = phrases_to_concatenate)
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern = phrases_to_concatenate,
                                      replacement = phrases_grouped,
                                      vectorize_all = FALSE)
    })
    pages <- unlist(pages)
  }

  # Apply hyphen handling
  if(keep_hyph_together){
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern= "(?<=\\w)[\\-\\u2013](?=\\w)", replacement ="_", vectorize_all = FALSE)
    })
    pages <- unlist(pages)
  }

  # Process each unique file
  unique_files <- base::unique(file_ids)

  # Validate alignment between text_list entries and parsed_filenames.
  # Empty text_list entries produce no file_ids, so unique_files will be
  # shorter than parsed_filenames, causing outputs to be saved to the
  # wrong filenames from that point onward.
  if (length(unique_files) != length(parsed_filenames)) {
    empty_names <- names(text_list)[sapply(text_list, length) == 0]
    stop("text_list and parsed_filenames are misaligned: text_list has ",
         length(unique_files), " non-empty entries but parsed_filenames has ",
         length(parsed_filenames), " entries. ",
         "Remove empty text_list entries and their corresponding parsed_filenames before calling this function. ",
         "Empty entries: ", paste(empty_names, collapse = ", "))
  }

  all_parsed <- vector(mode="list", length=length(unique_files))
  names(all_parsed) <- unique_files

  # Load English words data for validation
  utils::data(eng_words, envir = environment())

  for (m in 1:length(unique_files)){
    # Determine output filename - convert .rds to .parquet if needed
    output_file <- parsed_filenames[m]
    if(grepl("\\.rds$", output_file, ignore.case = TRUE)){
      output_file <- sub("\\.rds$", ".parquet", output_file, ignore.case = TRUE)
    } else if(!grepl("\\.parquet$", output_file, ignore.case = TRUE)){
      output_file <- paste0(output_file, ".parquet")
    }

    file_exists <- file.exists(output_file)
    should_parse <- overwrite || !file_exists || test
    should_save <- (overwrite || !file_exists) && !test

    if(should_parse){
      # Get texts for this file
      file_mask <- file_ids == unique_files[m]
      file_texts <- pages[file_mask]
      # Create doc_ids for each page (e.g., "docname_1", "docname_2", etc.)
      page_doc_ids <- paste0(unique_files[m], "_", seq_along(file_texts))

      # Parse using Python
      message(paste0("Parsing: ", unique_files[m], " (", length(file_texts), " pages)"))

      parsedtxt <- tryCatch({
        py_result <- parse_to_dataframe(
          texts = as.list(file_texts),
          doc_ids = as.list(page_doc_ids),
          batch_size = as.integer(batch_size)
        )
        # Explicitly convert each column by name to avoid reticulate misalignment
        r_result <- reticulate::py_to_r(py_result)
        data.frame(
          doc_id = as.character(r_result[["doc_id"]]),
          sentence_id = as.integer(r_result[["sentence_id"]]),
          token_id = as.integer(r_result[["token_id"]]),
          token = as.character(r_result[["token"]]),
          lemma = as.character(r_result[["lemma"]]),
          pos = as.character(r_result[["pos"]]),
          tag = as.character(r_result[["tag"]]),
          entity = as.character(r_result[["entity"]]),
          head_token_id = as.integer(r_result[["head_token_id"]]),
          dep_rel = as.character(r_result[["dep_rel"]]),
          stringsAsFactors = FALSE
        )
      }, error = function(e){
        stop(paste0("Error parsing ", unique_files[m], ": ", e$message))
      })

      # English word validation
      lettertokens <- parsedtxt$token[stringr::str_detect(parsedtxt$token, "[a-zA-Z]")]
      lettertokensunicodeescaped <- stringi::stri_escape_unicode(tolower(lettertokens))
      pctlettersineng <- sum(lettertokensunicodeescaped %in% eng_words)/length(lettertokensunicodeescaped)

      if(pctlettersineng < 0.5){
        warning(paste0("Fewer than 50% of letter-containing tokens in the document ", unique_files[m] ," are English words."))
      }

      # Apply custom entities
      if(!is.null(custom_entities)){
        for(k in 1:length(custom_entities)){
          patterns <- stringr::str_replace_all(custom_entities[[k]], "\\s", concatenator)
          mask <- parsedtxt$token %in% patterns & parsedtxt$entity == ""
          if(sum(mask) > 0){
            parsedtxt$entity[mask] <- paste0(names(custom_entities[k]), "_B")
          }
        }
      }

      all_parsed[[m]] <- parsedtxt

      # Save if appropriate
      if(should_save){
        # Ensure directory exists
        dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
        # Write parquet using duckdb
        con <- DBI::dbConnect(duckdb::duckdb())
        duckdb::duckdb_register(con, "parsed_data", parsedtxt)
        DBI::dbExecute(con, sprintf("COPY parsed_data TO '%s' (FORMAT PARQUET)", output_file))
        DBI::dbDisconnect(con, shutdown = TRUE)
        message(paste0("Saved: ", output_file))
      } else if(test){
        message(paste0("Parsing complete (test mode, not saved): ", unique_files[m]))
      }

    } else {
      message(paste0("Skipping ", unique_files[m], " - ", output_file, " already exists (set overwrite=TRUE to reparse)"))
      # Load existing file using duckdb
      con <- DBI::dbConnect(duckdb::duckdb())
      all_parsed[[m]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s'", output_file))
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }

  # Clean up Python resources
  tryCatch({
    finalize()
  }, error = function(e){
    warning(paste0("Error during cleanup: ", e$message))
  })

  return(all_parsed)
}


#' Read parsed text from Parquet file
#'
#' Helper function to read Parquet files created by parse_text_trf().
#'
#' @param filepath Path to the .parquet file
#' @return A data.frame with parsed tokens
#' @export
read_parsed_trf <- function(filepath){
  if(!requireNamespace("duckdb", quietly = T)){
    stop("Package 'duckdb' must be installed to read Parquet files.", call.=F)
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s'", filepath))
}
