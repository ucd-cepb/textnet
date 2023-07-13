# Exported functions 
# clean_entities 

#' Take a vector and clean the strings using regex
#'
#' @param v a vector of entity names
#' @return a cleaned vector of entity names
#' 
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
  
  #remove strings with specific placement: leading "the" and trailing "'s"
  remove <- c("^_*The_","^_*the_","^_*THE_","^_*The$","^_*the$","^_*THE$","'s$")
  index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
  v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
  
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
  
  #remove entities that have no letters
  if(remove_nums){
    index <- which(!grepl("[[:alpha:]]", v))
    v[index] <- ""
  }
  
  return(v)
}