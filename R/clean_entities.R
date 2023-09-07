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
#' @return a cleaned vector of entity names
#' @import pbapply
#' @import stringi
#' @import stringr

#' @export
#' 

clean_entities <- function(v, remove_nums=T){
  
  #format math font as regular font
  maths <- c("𝑎","𝑏","𝑐","𝑑","𝑒","𝑓","𝑔","ℎ","𝑖","𝑗","𝑘","𝑙","𝑚",
             "𝑛","𝑜","𝑝","𝑞","𝑟","𝑠","𝑡","𝑢","𝑣","𝑤","𝑥","𝑦","𝑧",
             "𝐴","𝐵","𝐶","𝐷","𝐸","𝐹","𝐺","𝐻","𝐼","𝐽","𝐾","𝐿","𝑀",
             "𝑁","𝑂","𝑃","𝑄","𝑅","𝑆",
             "𝑇","𝑈","𝑉","𝑊","𝑋","𝑌","𝑍")
  letts <- c(letters,LETTERS)
  
  v <- pblapply(1:length(v), function(i){
    stri_replace_all_regex(v[i], pattern = maths,
                           replacement = letts,
                           vectorize= F)
  })
  
  #remove strings with specific placement: trailing "'s"
  index <- which(grepl("'s$",v,perl = T))
  v[index] <- str_remove_all(v[index],"'s$")
  
  #next, remove all non-word characters
  remove <- c("\\W")
  index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
  v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
  
  #remove consecutive underscores that may have arisen due to previous cleaning step
  v <- gsub('(_)\\1+', '\\1', v)
  
  #remove leading or trailing underscores that may have arisen due to previous cleaning steps
  remove <- c("^_", "_$")
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