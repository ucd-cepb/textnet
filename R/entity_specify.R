# Exported function
#entity_specify

#' Creates EntityRuler patterns from entity names
#' @param entity_names A character vector of entity names to be recognized. If file_path is provided, this parameter is ignored.
#' @param entity_label A character string specifying the entity label to assign to all entities. Defaults to "CUSTOM". Ignored when reading from file.
#' @param case_sensitive A logical value indicating whether pattern matching should be case sensitive. Defaults to FALSE. Ignored when reading from file.
#' @param whole_word_only A logical value indicating whether to match only whole words. Defaults to TRUE. Ignored when reading from file.
#' @param aliases A character vector of aliases for each entity. Can be semicolon-separated strings for multiple aliases per entity. Defaults to NULL. Ignored when reading from file.
#' @param file_path Optional. Path to a CSV or TXT file containing entity specifications. Supports two formats:
#'   (1) Standard format with columns: entity_names, entity_label, case_sensitive, whole_word_only, aliases
#'   (2) Reviewed suggestions format from suggest_entities() with columns: phrase, keep, final_label (or suggested_label).
#'       Rows with keep=TRUE/T/true/yes/Y/1 are included. Uses final_label if provided, otherwise suggested_label.
#' @return A list of pattern dictionaries formatted for spaCy's EntityRuler
#' @importFrom utils read.csv read.table
#' @export

entity_specify <- function(entity_names = NULL, entity_label = "CUSTOM", case_sensitive = FALSE, whole_word_only = TRUE, aliases = NULL, file_path = NULL) {
  
  # Handle file input or direct parameter input
  if(!is.null(file_path)) {
    # File input mode
    if(!is.character(file_path) || length(file_path) != 1) {
      stop("'file_path' must be a single character string")
    }
    
    if(!file.exists(file_path)) {
      stop(paste0("File not found: ", file_path))
    }
    
    # Determine file type and read accordingly
    file_ext <- tolower(tools::file_ext(file_path))
    
    if(file_ext == "csv") {
      entity_data <- utils::read.csv(file_path, stringsAsFactors = FALSE)
    } else if(file_ext %in% c("txt", "tsv")) {
      entity_data <- utils::read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else {
      stop("Supported file formats: .csv, .txt, .tsv")
    }
    
    # Detect file format: suggest_entities format vs standard format
    is_suggestions_format <- all(c("phrase", "keep") %in% names(entity_data))

    if(is_suggestions_format) {
      # Handle suggest_entities() reviewed format
      message("Detected suggest_entities format, processing reviewed suggestions...")

      # Filter to kept rows
      keep_values <- tolower(trimws(as.character(entity_data$keep)))
      kept_rows <- keep_values %in% c("true", "t", "yes", "y", "1")
      entity_data <- entity_data[kept_rows, ]

      if(nrow(entity_data) == 0) {
        warning("No rows with keep=TRUE found in suggestions file")
        return(list())
      }

      # Determine label: use final_label if provided, otherwise suggested_label
      if("final_label" %in% names(entity_data)) {
        entity_data$entity_label <- ifelse(
          !is.na(entity_data$final_label) & nchar(trimws(entity_data$final_label)) > 0,
          trimws(entity_data$final_label),
          entity_data$suggested_label
        )
      } else {
        entity_data$entity_label <- entity_data$suggested_label
      }

      # Map columns to standard format
      entity_data$entity_names <- entity_data$phrase
      entity_data$case_sensitive <- FALSE
      entity_data$whole_word_only <- TRUE
      entity_data$aliases <- ""

      message(paste0("Loaded ", nrow(entity_data), " entities from reviewed suggestions"))

    } else {
      # Standard format - validate required columns
      required_cols <- c("entity_names", "entity_label", "case_sensitive", "whole_word_only", "aliases")
      missing_cols <- setdiff(required_cols, names(entity_data))
      if(length(missing_cols) > 0) {
        stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
      }
    }

    # Validate and convert data types
    if(!is.character(entity_data$entity_names)) {
      entity_data$entity_names <- as.character(entity_data$entity_names)
    }
    if(!is.character(entity_data$entity_label)) {
      entity_data$entity_label <- as.character(entity_data$entity_label)
    }
    if(!is.logical(entity_data$case_sensitive)) {
      entity_data$case_sensitive <- as.logical(entity_data$case_sensitive)
    }
    if(!is.logical(entity_data$whole_word_only)) {
      entity_data$whole_word_only <- as.logical(entity_data$whole_word_only)
    }
    if(!is.character(entity_data$aliases)) {
      entity_data$aliases <- as.character(entity_data$aliases)
    }
    # Replace NA aliases with empty strings
    entity_data$aliases[is.na(entity_data$aliases)] <- ""

    # Remove rows with missing entity names
    entity_data <- entity_data[!is.na(entity_data$entity_names) & nchar(entity_data$entity_names) > 0, ]

    if(nrow(entity_data) == 0) {
      warning("No valid entity names found in file after filtering")
      return(list())
    }
    
  } else {
    # Direct parameter input mode
    if(is.null(entity_names)) {
      stop("Either 'entity_names' or 'file_path' must be provided")
    }
    
    if(!is.character(entity_names) || length(entity_names) == 0) {
      stop("'entity_names' must be a non-empty character vector")
    }
    
    # Create data frame for consistent processing
    if(is.null(aliases)) {
      aliases <- rep("", length(entity_names))
    } else if(length(aliases) == 1) {
      aliases <- rep(aliases, length(entity_names))
    } else if(length(aliases) != length(entity_names)) {
      stop("'aliases' must be NULL, a single value, or the same length as 'entity_names'")
    }
    
    entity_data <- data.frame(
      entity_names = entity_names,
      entity_label = entity_label,
      case_sensitive = case_sensitive,
      whole_word_only = whole_word_only,
      aliases = aliases,
      stringsAsFactors = FALSE
    )
    
    # Remove empty strings and NA values
    entity_data <- entity_data[!is.na(entity_data$entity_names) & nchar(entity_data$entity_names) > 0, ]
    
    if(nrow(entity_data) == 0) {
      warning("No valid entity names provided after filtering")
      return(list())
    }
  }
  
  # Validate parameters for direct input mode only
  if(is.null(file_path)) {
    if(!is.character(entity_label) || length(entity_label) != 1) {
      stop("'entity_label' must be a single character string")
    }
    
    if(!is.logical(case_sensitive) || length(case_sensitive) != 1) {
      stop("'case_sensitive' must be a single logical value")
    }
    
    if(!is.logical(whole_word_only) || length(whole_word_only) != 1) {
      stop("'whole_word_only' must be a single logical value")
    }
    
    if(!is.null(aliases) && !is.character(aliases)) {
      stop("'aliases' must be a character vector or NULL")
    }
  }
  
  # Create patterns list using vectorized approach
  create_pattern <- function(entity_name, entity_label_i, case_sensitive_i, whole_word_only_i) {
    # Create basic pattern structure
    if(whole_word_only_i) {
      # For whole word matching, use token-based patterns
      # Split multi-word entities into tokens
      tokens <- strsplit(entity_name, "\\s+")[[1]]
      
      if(length(tokens) == 1) {
        # Single token pattern
        pattern_obj <- list(
          list("LOWER" = if(case_sensitive_i) entity_name else tolower(entity_name))
        )
      } else {
        # Multi-token pattern
        pattern_obj <- lapply(tokens, function(token) {
          list("LOWER" = if(case_sensitive_i) token else tolower(token))
        })
      }
    } else {
      # For substring matching, use simple text pattern
      pattern_obj <- if(case_sensitive_i) entity_name else tolower(entity_name)
    }
    
    # Create the pattern dictionary
    list(
      label = entity_label_i,
      pattern = pattern_obj
    )
  }
  
  # Expand entities to include aliases
  expand_entities <- function(entity_name, entity_label_i, case_sensitive_i, whole_word_only_i, aliases_i) {
    # Start with the main entity
    all_names <- entity_name
    
    # Add aliases if they exist
    if(!is.na(aliases_i) && nchar(aliases_i) > 0) {
      # Split aliases by semicolon and clean whitespace
      alias_list <- trimws(strsplit(aliases_i, ";")[[1]])
      alias_list <- alias_list[nchar(alias_list) > 0]  # Remove empty strings
      all_names <- c(all_names, alias_list)
    }
    
    # Create patterns for all names (main + aliases)
    lapply(all_names, function(name) {
      create_pattern(name, entity_label_i, case_sensitive_i, whole_word_only_i)
    })
  }
  
  # Vectorize the pattern creation with aliases expansion
  patterns_nested <- mapply(
    expand_entities,
    entity_data$entity_names,
    entity_data$entity_label,
    entity_data$case_sensitive,
    entity_data$whole_word_only,
    entity_data$aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  
  # Flatten the nested list structure
  patterns <- unlist(patterns_nested, recursive = FALSE)
  
  return(patterns)
}