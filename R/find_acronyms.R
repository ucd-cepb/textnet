# Exported function
# pdf_clean

#' Take a character vector and detect all parenthetical statements in which an acronym is defined within the character vector. 
#'
#' @param str A character vector
#' 
#' @importFrom data.table setDT setcolorder rbind
#' @importFrom stringr str_split str_remove_all str_replace_all
#' @importFrom stringi stri_match_last stri_match_all

#' @return a data table with a "name" column and an "acronym" column representing its associated acronym.
#' Each row corresponds to a unique match in the document.
#' 
#' @export
#' 

find_acronyms <- function(str){
  # Input validation
  if(!is.character(str)) {
    stop("'str' must be a character vector")
  }

  paren_splits <- str_split(str, pattern = "\\)")
  paren_splits2 <- lapply(paren_splits, function (k) k[nchar(k)>0])
  paren_splits3 <- lapply(paren_splits2, function(m) stringr::str_split(m, pattern = "\\("))
  paren_splits4 <- lapply(paren_splits3, function (j) lapply(j, function(m) m[length(m)==2]))
  paren_splits4 <- unlist(paren_splits4, recursive=F)
  paren_splits5 <- do.call(rbind, paren_splits4)
  paren_splits$acr1 <- stri_match_last(str = paren_splits5[,1], regex ="\\b[A-Z]+\\b")
  paren_splits$acr2 <- stri_match_all(str = paren_splits5[,2], regex ="\\b[A-Z]+\\b")
  paren_splits$abb1 <- str_remove_all(paren_splits5[,1],"[^A-Z]")
  paren_splits$abb1 <- sapply(1:length(paren_splits$acr2), 
                                  function(s) sapply(1:length(paren_splits$acr2[[s]]), function (m){
                                    stri_match_last(str=paren_splits$abb1[s], 
                                        regex = paren_splits$acr2[[s]][m])
                                  }))
  sp_lower <- "[\\s|a-z]+"
  paren_splits$name1 <- sapply(1:length(paren_splits$acr2), 
                               function(s) sapply(1:length(paren_splits$acr2[[s]]), function (m){
                                 stri_match_last(str=paren_splits5[s,1], regex = paste0(paste0(stringr::str_split(paren_splits$acr2[[s]][m],pattern="")[[1]],collapse=sp_lower),"[a-z]+"))
                               }))
  paren_splits$abb2 <- str_remove_all(paren_splits5[,2],"[^A-Z]")
  paren_splits$name2 <- sapply(1:length(paren_splits$abb2), 
                               function(s) 
                                 stri_match_last(str=paren_splits5[s,2], regex = paste0(paste0(stringr::str_split(paren_splits$acr1[s],pattern="")[[1]],collapse=sp_lower),"[a-z]+"))
                               )
  
  paren_splits$acr1 <- as.vector(paren_splits$acr1)
  paren_splits$name2
  
  paren_splits$acr2 <- unlist(paren_splits$acr2)
  paren_splits$name1 <- unlist(paren_splits$name1)
  
  acronym_matches <- setDT(list("name" = paren_splits$name2,"acronym" = paren_splits$acr1))
  acronym_matches2 <- setDT(list("name" = paren_splits$name1, "acronym" = paren_splits$acr2))
  
  acronym_matches <- rbind(acronym_matches, acronym_matches2)
  acronym_matches <- acronym_matches[!is.na(acronym) & !is.na(name) &nchar(acronym)>1,]
  
  acronym_matches$name <- str_replace_all(acronym_matches$name,"-|\\s+","_")
    #change hyphens and spaces to underscores, since in spacyparse they are treated as separate tokens

  
  #sort from shortest to longest acronym so the replacement happens in the right order
  acronym_matches <- acronym_matches[order(nchar(acronym_matches$acronym)),]
  acronym_matches <- unique(acronym_matches)
  #don't include non-unique acronyms
  acronym_matches <- acronym_matches[,c(.SD,.N),by=acronym]
  acronym_matches <- acronym_matches[N==1,]
  acronym_matches <- acronym_matches[,N:=NULL]
  #reorder cols
  setcolorder(x=acronym_matches,neworder=c("name", "acronym"))
  return(acronym_matches)
}