# Exported functions 
# clean_entities 

#' Take a vector and clean the strings using regex. 
#' Uppercase or lowercase math font is converted to uppercase or lowercase letters, respectively.
#' Any trailing "'s" at the end of the entity name is removed. 
#' All non-word characters are removed.
#' Consecutive underscores are collapsed to a single underscore.
#' Leading and trailing underscores are removed.
#' Entities that have no letters are removed, if remove_nums is set to T.
#'
#' @param v a vector of entity names
#' @param remove_nums A boolean. If T, sets entities that contain no letters to an empty string. If F, sets entities that contain no letters or numbers to an empty string.
#' @param remove_trailing_s A boolean. If T, removes trailing instances of concatenator+s, or apostrophe+s in each element of v.
#' @param concatenator Defaults to an underscore. Use regex notation. The concatenator used in elements of v.
#' @return a cleaned vector of entity names

#' @importFrom stringr str_remove_all
#' @importFrom stringi stri_replace_all_regex
#' @export
#' 

clean_entities <- function(v, remove_nums=T, remove_trailing_s=T, concatenator = "_"){
  # Input validation
  if (!is.vector(v)) {
    stop("'v' must be a vector")
  }
  
  if (!is.logical(remove_nums)) {
    stop("'remove_nums' must be a boolean value (TRUE/FALSE)")
  }
  
  if (!is.logical(remove_trailing_s)) {
    stop("'remove_trailing_s' must be a boolean value (TRUE/FALSE)")
  }
  
  if (!is.character(concatenator) || length(concatenator) != 1) {
    stop("'concatenator' must be a single character string")
  }
  
  #format math font as regular font
  #unicode of math font alphabet
  maths <- c("\\U0001d44e", "\\U0001d44f", "\\U0001d450", "\\U0001d451", "\\U0001d452",
             "\\U0001d453", "\\U0001d454", "\\u210e",     "\\U0001d456", "\\U0001d457",
             "\\U0001d458", "\\U0001d459", "\\U0001d45a", "\\U0001d45b", "\\U0001d45c",
             "\\U0001d45d", "\\U0001d45e", "\\U0001d45f", "\\U0001d460", "\\U0001d461",
             "\\U0001d462", "\\U0001d463", "\\U0001d464", "\\U0001d465", "\\U0001d466",
             "\\U0001d467", "\\U0001d434", "\\U0001d435", "\\U0001d436", "\\U0001d437",
             "\\U0001d438", "\\U0001d439", "\\U0001d43a", "\\U0001d43b", "\\U0001d43c",
             "\\U0001d43d", "\\U0001d43e", "\\U0001d43f", "\\U0001d440", "\\U0001d441",
             "\\U0001d442", "\\U0001d443", "\\U0001d444", "\\U0001d445", "\\U0001d446",
             "\\U0001d447", "\\U0001d448", "\\U0001d449", "\\U0001d44a", "\\U0001d44b",
             "\\U0001d44c", "\\U0001d44d")
  letts <- c(letters,LETTERS)
  
  v <- lapply(1:length(v), function(i){
    stri_replace_all_regex(v[i], pattern = maths,
                           replacement = letts,
                           vectorize= F)
  })
  
  if(remove_trailing_s==T){
    #remove strings with specific placement: trailing "'s"
    index <- which(grepl("'s$",v,perl = T))
    v[index] <- str_remove_all(v[index],"'s$")
    
    #remove strings with specific placement: trailing concatenator + s, e.g. "_s"
    index <- which(grepl(paste0(concatenator,"s$"),v,perl = T))
    v[index] <- str_remove_all(v[index],paste0(concatenator,"s$"))
    
  }
  
  #next, remove all non-word characters
  remove <- c("\\W")
  index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
  v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
  
  #remove consecutive underscores that may have arisen due to previous cleaning step
  v <- gsub(paste0('(',concatenator,')\\1+'), '\\1', v)
  
  #remove leading or trailing underscores that may have arisen due to previous cleaning steps
  remove <- c(paste0("^",concatenator), paste0(concatenator, "$"))
  index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
  v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
  
  #remove entities that have no letters (or numbers, if remove_nums == F)
  if(remove_nums){
    index <- which(!grepl("[a-zA-Z]", v))
    v[index] <- ""
  }else{
    index <- which(!grepl("[a-zA-Z0-9]", v))
    v[index] <- ""
  }
  
  return(v)
}