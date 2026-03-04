# Shared helpers for sentence-level tests

#' Parse "Source -verb-> Target" (with optional AND clauses) into a list of edge specs
parse_expected_output <- function(str) {
  parts <- strsplit(str, " AND ")[[1]]
  lapply(parts, function(p) {
    p <- trimws(p)
    # Match: source -verb-> target
    m <- regmatches(p, regexec("^(.+?) -(.+?)-> (.+)$", p))[[1]]
    if (length(m) < 4) stop("Cannot parse expected output: ", p)
    list(source = m[2], verb = m[3], target = m[4])
  })
}

#' Build display verb from an edgelist row (helper + head, or head + xcomp, else head)
.display_verb <- function(helper_token, head_verb_name, xcomp_verb) {
  helper <- unlist(helper_token)
  xcomp  <- unlist(xcomp_verb)
  if (length(helper) > 0) {
    paste(c(helper, head_verb_name), collapse = " ")
  } else if (length(xcomp) > 0) {
    paste(c(head_verb_name, xcomp), collapse = " ")
  } else {
    head_verb_name
  }
}

#' Check whether an edgelist contains a specific (source, verb, target) edge
has_edge <- function(edgelist, source, verb, target) {
  if (is.null(edgelist) || nrow(edgelist) == 0) return(FALSE)
  for (i in seq_len(nrow(edgelist))) {
    dv <- .display_verb(
      edgelist$helper_token[[i]],
      edgelist$head_verb_name[[i]],
      edgelist$xcomp_verb[[i]]
    )
    if (identical(edgelist$source[[i]], source) &&
        identical(dv, verb) &&
        identical(edgelist$target[[i]], target)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Run parse_text + textnet_extract on a single sentence
run_sentence <- function(sentence, python_path, custom_entities = NULL) {
  text_list <- list(doc = sentence)
  ce <- if (!is.null(custom_entities)) list(CUSTOM = custom_entities) else NULL
  parsed <- suppressWarnings(textNet::parse_text(
    python_path,
    text_list          = text_list,
    parsed_filenames   = tempfile(),
    custom_entities    = ce,
    progress           = FALSE
  ))
  textNet::textnet_extract(
    parsed[[1]],
    concatenator         = "_",
    keep_entities        = c("ORG", "GPE", "PERSON", "NORP", "CUSTOM", "QUANTITY",
                             "EVENT", "DATE", "LOC", "PRODUCT"),
    keep_incomplete_edges = TRUE,
    cl                   = 1,
    progress             = FALSE
  )
}
