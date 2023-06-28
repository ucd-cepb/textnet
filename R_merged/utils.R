# Exported functions 
# create_empty_sentence_parse_list
# generate_proper_name

#' Create an list to iterate over a sentence and store items
#'
#' Takes an integer value equivalent to token length of sentence and generates named list
#' @param N integer value that sets vector length
#' @return list with named empty vectors
#' @export
create_empty_sentence_parse_list <- function(N){
  raw_empty = vector(mode = "character", N)
  parse_list = list(source_or_target = raw_empty,
                     head_verb_id = raw_empty,
                     head_verb_name = raw_empty,
                     head_verb_lemma = raw_empty,
                     head_verb_tense = raw_empty,
                     parent_verb_id = raw_empty)
  return(parse_list)
}