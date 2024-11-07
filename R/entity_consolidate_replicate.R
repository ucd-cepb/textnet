# Exported functions 
# entity_consolidate_replicate 

#' Modified version of spacyr::entity_consolidate.
#' spacyr::entity_consolidate returns a data.frame that no longer has head_token_id and dep_rel data.
#' Instead, this function basically breaks off a feature of the original function and adds a new column to the original spacyr data.frame that is the concatenated entity.
#' This feature is inefficient in that the concatenated entity is then replicated multiple times, but this does seem to be the easiest way to preserve the other data.
#'
#' @param x parsed spacy document
#' @param concatenator A character that separates string segments when they are collapsed into a single entity. Defaults to "_"
#' @param remove regex formatted strings to remove as entity components (like "the" in "the Seattle Supersonics")
#' @return original data frame with added column for concatenated entity
#' 
#' @import data.table
#' @import stringr
#'

entity_consolidate_replicate <- function(x, concatenator = "_",remove = NULL) {
  #Remove tokens that have no alphabet characters
  
  spacy_result <- as.data.table(x)
  if(!is.null(remove)){
    index <- which(grepl(paste(remove,collapse = '|'),spacy_result$token,perl = T)&spacy_result$entity!="")
    spacy_result$token[index] <- str_remove_all(spacy_result$token[index],paste(remove,collapse = '|'))
    spacy_result$entity[index] <- ""
  }
  
  entity <- entity_type <- entity_count <- iob <- entity_id <- .N <- .SD <-
    `:=` <- token <- lemma <- pos <- tag <- new_token_id <- token_id <-
    sentence_id <- doc_id <- NULL
  if (!"entity" %in% names(spacy_result)) {
    stop("no entities in parsed object: rerun spacy_parse() with entity = TRUE")
  }
  spacy_result[, entity_type := sub("_.+", "", entity)]
  spacy_result[, iob := sub(".+_", "", entity)]
  extended_list <- c("DATE", "TIME", "PERCENT", "MONEY", "QUANTITY",
                     "ORDINAL", "CARDINAL")
  # if (type == 'extended'){
  #     spacy_result[entity_type != ""  & !(entity_type %in% extended_list),
  #                  c("entity_type", "iob") := ""]
  # } else if (type == 'named') {
  #     spacy_result[entity_type != ""  & (entity_type %in% extended_list),
  #                  c("entity_type", "iob") := ""]
  # }
  spacy_result[, entity_count := ifelse(iob == "B" | iob == "", 1, 0)]
  spacy_result[, entity_id := cumsum(entity_count), by = c("doc_id", "sentence_id")]
  #added source_or_target to by = c(...) so that appositives do not get concatenated together with the main entity
  spacy_result[, entity_name := paste(token, collapse = concatenator),by = c("doc_id", "sentence_id", "entity_id", "source_or_target")]
  spacy_result$entity_name[spacy_result$entity==''] <- ''
  spacy_result$entity_id[spacy_result$entity==''] <- -1
  ret <- as.data.table(spacy_result)
  class(ret) <- c("spacyr_parsed", class(ret))
  ret
}
