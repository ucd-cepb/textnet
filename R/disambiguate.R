#behavior: if there is a vector of entities in a "to" cell, it duplicates the relevant
#rows such that there is an edge for each of the entities in the "to" cell

disambiguate <- function(from, to, match_partial_entity=rep(F, length(from)), textnet_extract, prefixes){
  library(igraph)
  library(ggraph)
  library(sna)
  library(stringr)
  library(dplyr)
  
  remove_prefixes <- function(prefixes){
    remove <- prefixes[[m]]
    index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
    v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
    return(v)
  }
  
  
  clean_entities(from)
  clean_entities(to)
  
  
  abbr <- function(strng){
    
    if (!identical(grep(paste0("\b",strng,"\b"),govscitbl$Abbr,useBytes = F), integer(0))){
      return(govscitbl$Agency[grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F)] )
    }
    else
      return(strng)
  }
  
  

  
  #temp rows because changing character column into list column
  edgelist$sourcetemp <- lapply(edgelist$source, function(strng) agency_disambig(strng,m))
  edgelist$targettemp <- lapply(edgelist$target, function(strng) agency_disambig(strng,m))
  edgelist$length_source <- sapply(edgelist$sourcetemp, length)
  edgelist$length_target <- sapply(edgelist$targettemp, length)
  rows <- edgelist[rep(seq(1, nrow(edgelist)), edgelist$length_source)]
  rows$source <- unlist(edgelist$sourcetemp)
  rowstarget <- rows[rep(seq(1, nrow(rows)), rows$length_target)]
  rowstarget$target <- unlist(rows$targettemp)
  
  rowstarget$sourcetemp <- rowstarget$targettemp <- rowstarget$length_source <- rowstarget$length_target<- NULL
  edgelist <- rowstarget
  
  nodelist$entity_cattemp <- lapply(nodelist$entity_cat, function(strng) agency_disambig(strng,m))
  nodelist$length_entitycat <- sapply(nodelist$entity_cattemp, length)
  rows <- nodelist[rep(seq(1, nrow(nodelist)), nodelist$length_entitycat)]
  rows$entity_cat <- unlist(nodelist$entity_cattemp)
  rows$entity_cattemp <- rows$length_entitycat <- NULL
  nodelist <- rows
  
  
  
  
}