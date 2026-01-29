# Exported function
#suggest_entities

#' Suggests potential missed entities from parsed text output
#'
#' Analyzes parsed output from parse_text() to identify tokens and phrases that
#' may be entities but were not tagged by spaCy's NER. Exports suggestions to a
#' CSV file for user review. After review, the CSV can be used with entity_specify()
#' to create EntityRuler patterns.
#'
#' @param parsed_output A list of dataframes from parse_text(), or a single dataframe
#' @param min_frequency Minimum number of occurrences for a token/phrase to be suggested. Defaults to 2.
#' @param max_suggestions Maximum number of suggestions to return. Defaults to 100.
#' @param export_path Optional. Path to export CSV file. If NULL, returns dataframe without exporting.
#' @param include_context If TRUE (default), includes example contexts for each suggestion.
#' @param context_examples Number of example contexts to include per suggestion. Defaults to 3.
#' @return A dataframe of suggested entities with columns: phrase, frequency, pos_tags, suggested_label, context_examples, keep, final_label. Also exports to CSV if export_path is provided.
#' @importFrom stats aggregate
#' @export

suggest_entities <- function(parsed_output,
                             min_frequency = 2,
                             max_suggestions = 100,
                             export_path = NULL,
                             include_context = TRUE,
                             context_examples = 3) {

 # Input validation
  if(!is.list(parsed_output)) {
    stop("'parsed_output' must be a list of dataframes or a single dataframe")
  }

  # Handle single dataframe vs list of dataframes
  if(is.data.frame(parsed_output)) {
    all_tokens <- parsed_output
  } else {
    # Combine all dataframes
    all_tokens <- do.call(rbind, parsed_output)
  }

  # Validate required columns
  required_cols <- c("token", "pos", "entity", "doc_id", "sentence_id")
  missing_cols <- setdiff(required_cols, names(all_tokens))
  if(length(missing_cols) > 0) {
    stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  }

  # Create sentence reconstruction for context
  all_tokens$row_id <- 1:nrow(all_tokens)

  # ============================================
  # Strategy 1: Untagged proper nouns (NNP/NNPS)
  # ============================================
  proper_nouns <- all_tokens[
    all_tokens$pos %in% c("PROPN") &
    all_tokens$entity == "" &
    nchar(all_tokens$token) > 1 &
    grepl("^[A-Z]", all_tokens$token),
  ]

  # ============================================
  # Strategy 2: Find consecutive proper noun sequences (multi-word entities)
  # ============================================
  find_consecutive_proper_nouns <- function(df) {
    if(nrow(df) == 0) return(data.frame())

    # Sort by doc_id, sentence_id, token_id
    df <- df[order(df$doc_id, df$sentence_id, df$row_id), ]

    # Identify proper nouns that are untagged
    df$is_proper <- df$pos %in% c("PROPN") & df$entity == ""

    # Find runs of consecutive proper nouns within same sentence
    phrases <- list()
    current_phrase <- NULL
    current_doc <- NULL
    current_sent <- NULL

    for(i in 1:nrow(df)) {
      row <- df[i, ]

      if(row$is_proper) {
        if(is.null(current_phrase) ||
           row$doc_id != current_doc ||
           row$sentence_id != current_sent ||
           row$row_id != (df$row_id[i-1] + 1)) {
          # Start new phrase
          if(!is.null(current_phrase) && length(current_phrase$tokens) > 1) {
            phrases <- c(phrases, list(current_phrase))
          }
          current_phrase <- list(
            tokens = row$token,
            pos_tags = row$pos,
            doc_id = row$doc_id,
            sentence_id = row$sentence_id,
            start_row = row$row_id
          )
          current_doc <- row$doc_id
          current_sent <- row$sentence_id
        } else {
          # Continue phrase
          current_phrase$tokens <- c(current_phrase$tokens, row$token)
          current_phrase$pos_tags <- paste(current_phrase$pos_tags, row$pos)
        }
      } else {
        # End current phrase if exists
        if(!is.null(current_phrase) && length(current_phrase$tokens) > 1) {
          phrases <- c(phrases, list(current_phrase))
        }
        current_phrase <- NULL
      }
    }

    # Don't forget last phrase
    if(!is.null(current_phrase) && length(current_phrase$tokens) > 1) {
      phrases <- c(phrases, list(current_phrase))
    }

    if(length(phrases) == 0) return(data.frame())

    # Convert to dataframe
    phrase_df <- data.frame(
      phrase = sapply(phrases, function(p) paste(p$tokens, collapse = " ")),
      pos_tags = sapply(phrases, function(p) p$pos_tags),
      doc_id = sapply(phrases, function(p) p$doc_id),
      sentence_id = sapply(phrases, function(p) p$sentence_id),
      stringsAsFactors = FALSE
    )

    return(phrase_df)
  }

  multi_word <- find_consecutive_proper_nouns(all_tokens)

  # ============================================
  # Strategy 3: Pattern-based detection
  # ============================================
  # Look for "the X" patterns where X is capitalized and followed by org-like words
  org_suffixes <- c("Agency", "Department", "Bureau", "Commission", "Authority",
                    "Administration", "Service", "Board", "Council", "Committee",
                    "Corporation", "Company", "District", "Division", "Office",
                    "Institute", "Association", "Foundation", "Organization",
                    "Trust", "Fund", "Project", "Program", "Initiative")

  find_org_patterns <- function(df) {
    if(nrow(df) == 0) return(data.frame())

    df <- df[order(df$doc_id, df$sentence_id, df$row_id), ]

    patterns <- list()

    for(i in 1:(nrow(df) - 1)) {
      # Check if current token ends with org suffix
      if(df$token[i] %in% org_suffixes && df$entity[i] == "") {
        # Look back for capitalized words
        phrase_tokens <- df$token[i]
        j <- i - 1
        while(j >= 1 &&
              df$doc_id[j] == df$doc_id[i] &&
              df$sentence_id[j] == df$sentence_id[i] &&
              df$row_id[j] == df$row_id[j+1] - 1 &&
              (grepl("^[A-Z]", df$token[j]) || df$token[j] %in% c("of", "the", "and", "for"))) {
          phrase_tokens <- c(df$token[j], phrase_tokens)
          j <- j - 1
        }

        if(length(phrase_tokens) > 1) {
          # Remove leading "the" if present
          if(tolower(phrase_tokens[1]) == "the") {
            phrase_tokens <- phrase_tokens[-1]
          }

          if(length(phrase_tokens) > 1) {
            patterns <- c(patterns, list(list(
              phrase = paste(phrase_tokens, collapse = " "),
              doc_id = df$doc_id[i],
              sentence_id = df$sentence_id[i]
            )))
          }
        }
      }
    }

    if(length(patterns) == 0) return(data.frame())

    data.frame(
      phrase = sapply(patterns, function(p) p$phrase),
      pos_tags = "ORG_PATTERN",
      doc_id = sapply(patterns, function(p) p$doc_id),
      sentence_id = sapply(patterns, function(p) p$sentence_id),
      stringsAsFactors = FALSE
    )
  }

  org_patterns <- find_org_patterns(all_tokens)

  # ============================================
  # Combine all suggestions
  # ============================================

  # Single-word proper nouns
  if(nrow(proper_nouns) > 0) {
    single_word <- data.frame(
      phrase = proper_nouns$token,
      pos_tags = proper_nouns$pos,
      doc_id = proper_nouns$doc_id,
      sentence_id = proper_nouns$sentence_id,
      stringsAsFactors = FALSE
    )
  } else {
    single_word <- data.frame()
  }

  # Combine all
  all_suggestions <- rbind(single_word, multi_word, org_patterns)

  if(nrow(all_suggestions) == 0) {
    message("No entity suggestions found")
    return(data.frame(
      phrase = character(),
      frequency = integer(),
      pos_tags = character(),
      suggested_label = character(),
      context_examples = character(),
      keep = logical(),
      final_label = character()
    ))
  }

  # ============================================
  # Aggregate by phrase and count frequency
  # ============================================
  freq_table <- as.data.frame(table(all_suggestions$phrase))
  names(freq_table) <- c("phrase", "frequency")
  freq_table$phrase <- as.character(freq_table$phrase)

  # Filter by minimum frequency
  freq_table <- freq_table[freq_table$frequency >= min_frequency, ]

  if(nrow(freq_table) == 0) {
    message(paste0("No suggestions meet minimum frequency of ", min_frequency))
    return(data.frame(
      phrase = character(),
      frequency = integer(),
      pos_tags = character(),
      suggested_label = character(),
      context_examples = character(),
      keep = logical(),
      final_label = character()
    ))
  }

  # Sort by frequency descending
  freq_table <- freq_table[order(-freq_table$frequency), ]

  # Limit to max_suggestions
  if(nrow(freq_table) > max_suggestions) {
    freq_table <- freq_table[1:max_suggestions, ]
  }

  # ============================================
  # Add POS tags (most common for each phrase)
  # ============================================
  get_common_pos <- function(phrase) {
    matches <- all_suggestions[all_suggestions$phrase == phrase, ]
    if(nrow(matches) == 0) return("")
    pos_freq <- table(matches$pos_tags)
    names(pos_freq)[which.max(pos_freq)]
  }

  freq_table$pos_tags <- sapply(freq_table$phrase, get_common_pos)

  # ============================================
  # Suggest labels based on patterns
  # ============================================
  suggest_label <- function(phrase, pos_tags) {
    phrase_lower <- tolower(phrase)

    # Organization patterns
    if(grepl("(agency|department|bureau|commission|authority|administration|service|board|council|committee|corporation|company|district|division|office|institute|association|foundation|organization|trust|fund)$", phrase_lower)) {
      return("ORG")
    }

    # Location patterns
    if(grepl("(valley|river|lake|mountain|creek|bay|coast|beach|island|county|city|state|region|park|forest|reserve)$", phrase_lower)) {
      return("LOC")
    }

    # Facility patterns
    if(grepl("(dam|reservoir|plant|facility|center|station|airport|port|bridge|highway|road|trail)$", phrase_lower)) {
      return("FAC")
    }

    # Project/program patterns
    if(grepl("(project|program|plan|initiative|act|bill|law|agreement|treaty)$", phrase_lower)) {
      return("LAW")
    }

    # If from org pattern detection
    if(pos_tags == "ORG_PATTERN") {
      return("ORG")
    }

    # Default based on capitalization (likely ORG or GPE)
    return("ORG")
  }

  freq_table$suggested_label <- mapply(suggest_label, freq_table$phrase, freq_table$pos_tags)

  # ============================================
  # Add context examples
  # ============================================
  if(include_context) {
    get_context <- function(phrase) {
      # Find sentences containing this phrase
      phrase_tokens <- strsplit(phrase, " ")[[1]]

      # For single-word phrases
      if(length(phrase_tokens) == 1) {
        matches <- all_tokens[all_tokens$token == phrase, ]
      } else {
        # For multi-word phrases, find first token occurrences
        matches <- all_tokens[all_tokens$token == phrase_tokens[1], ]
      }

      if(nrow(matches) == 0) return("")

      # Get unique sentence contexts
      contexts <- unique(paste(matches$doc_id, matches$sentence_id, sep = "_"))
      contexts <- head(contexts, context_examples)

      # Reconstruct sentences
      sentences <- sapply(contexts, function(ctx) {
        parts <- strsplit(ctx, "_")[[1]]
        doc <- parts[1]
        sent <- as.integer(parts[2])
        sent_tokens <- all_tokens[all_tokens$doc_id == doc & all_tokens$sentence_id == sent, ]
        paste(sent_tokens$token, collapse = " ")
      })

      paste(sentences, collapse = " | ")
    }

    freq_table$context_examples <- sapply(freq_table$phrase, get_context)
  } else {
    freq_table$context_examples <- ""
  }

  # ============================================
  # Add user review columns
  # ============================================
  freq_table$keep <- ""
  freq_table$final_label <- ""

  # Reorder columns
  freq_table <- freq_table[, c("phrase", "frequency", "pos_tags", "suggested_label",
                                "context_examples", "keep", "final_label")]

  # Reset row names
  rownames(freq_table) <- NULL

  # ============================================
  # Export to CSV if requested
  # ============================================
  if(!is.null(export_path)) {
    utils::write.csv(freq_table, export_path, row.names = FALSE)
    message(paste0("Exported ", nrow(freq_table), " suggestions to: ", export_path))
  }

  return(freq_table)
}
