# Exported functions 
# filter_sentences

#' Take a vector and clean the strings using regex. 
#' Uppercase or lowercase math font is converted to uppercase or lowercase letters, respectively.
#' Any trailing "'s" at the end of the entity name is removed. 
#' All non-word characters are removed.
#' Consecutive underscores are collapsed to a single underscore.
#' Leading and trailing underscores are removed.
#' Entities that have no letters are removed, if remove_nums is set to T.
#'
#' @param file Data frame to be cleaned. To ensure file format appropriateness, it should be of the form that results from spacy_parse output
#' @param filter Dictionary on which to filter file for acceptable words. Defaults to SCOWL 2020.12.07, including files with extension <= 60, with filenames that include "variant", "american", "british", canadian", or "australian".
#' @param percent_threshold The required percentage of tokens in a sentence required to be found in the filter to keep the sentence in the dataset. Defaults to 40.
#' @param case_sensitive Whether the token is required to have the specific casing used in the dictionary to count as a match. Defaults to F.
#' @return a cleaned version of 'file', keeping only the sentences that pass the threshold requirement.
#' @importFrom stats aggregate
#' @importFrom dplyr left_join

#' @export
#' 

filter_sentences <- function(file, filter = textNet::eng_words, 
                             percent_threshold = 40, case_sensitive = F){
  # Input validation
  if(!is.data.frame(file)) {
    stop("'file' must be a data frame")
  }
  
  if(!is.character(filter)) {
    stop("'filter' must be a character vector")
  }
  
  if(!is.numeric(percent_threshold) || percent_threshold < 0 || percent_threshold > 100) {
    stop("'percent_threshold' must be a numeric value between 0 and 100")
  }
  
  if(!is.logical(case_sensitive)) {
    stop("'case_sensitive' must be a logical value (TRUE/FALSE)")
  }

  if(!case_sensitive){
    filter <- tolower(filter)
    tokens <- tolower(file$token)
  }else if(case_sensitive){
    tokens <- file$token
  }
   eng <- tokens %in% filter
   percent_pass_filter <- aggregate(eng, 
             by = list(file$sentence_id, file$doc_id), 
             FUN = mean) 
   percent_pass_filter <- percent_pass_filter[
      percent_pass_filter$x >= (percent_threshold/100),]
   percent_pass_filter$x <- NULL
   colnames(percent_pass_filter) <- c("sentence_id", "doc_id")
   
   file <- left_join(percent_pass_filter, file)
   return(file)
}