# Exported functions 
# disambiguate 

#' Renames entities in a textnet_extract that are supplied in 'from' with those supplied in 'to'.
#'
#' @param from A list of character vectors representing terms to look for, the same length as 'to'. 
#' If a term in this list is found, this function replaces it with its corresponding match in 'to'. 
#' The "acronyms" column from the find_acronyms output could be used here. 
#' @param to A list of character vectors representing replacement terms, the same length as 'from'. 
#' If a term in the 'from' list is found, it is replaced with the corresponding term in this list. 
#' The "names" column in the find_acronyms output could be used here. If an element consists of multiple entities in a vector, 
#' the relevant edges are duplicated in the edgelist, with one edge for each entity in the vector. 
#' These vector entities are disambiguated first, followed by the rest of the strings in order of appearance in 'from' and 'to'.
#rows such that there is an edge for each of the entities in the "to" cell. 
#' @param match_partial_entity A logical vector of the same length as 'from'. 
#' If match_partial_entity is T for an element, it can match on the 'from' term separated by concatenator. 
#' Otherwise, the 'from' term must match the whole entity name to be accepted. Defaults to "F" for all elements.
#' @param textnet_extract An output of the function textnet_extract
#' @param try_drop A regex expression representing one or more terms to try dropping. 
#' The usual case for this is country or state names, such as "^US_". 
#' If try_drop is supplied, the function first tries to determine whether each element in the textnet_extract matches a term in 'from.'
#' If not, it determines whether removing try_drop from the remaining elements enables them to match a term in 'from'. 
#' If so, the matching textnet_extract elements are replaced with the corresponding element in 'to.' 
#' The non-matches remains unchanged.
#' @param recursive A logical value, defaulting to T. If recursive is T, the function runs multiple times. 
#' The number of times the function is run is determined by the longest "chain" in which a value in 'to' is found in 'from', 
#' which may in turn correspond to a 'to' value that is found in 'from', and so on. 
#' @param concatenator The word boundary to look for when match_partial_entity is true. Defaults to "_". 
#' @return a cleaned textnet extract
#' 
#' @import data.table
#' @import igraph
#' @import ggraph
#' @import sna
#' @import stringr
#' @import dplyr
#' @export
#'

#if recursive is true, runs it multiple times to reach the end of the chain.

disambiguate <- function(from, to, match_partial_entity=rep(F, length(from)), textnet_extract, try_drop=NULL, recursive=T, concatenator="_"){
  library(igraph)
  library(ggraph)
  library(sna)
  library(stringr)
  library(dplyr)
  #Data formatting checks####
  to_from_same <- sapply(1:length(to), function (j) sum(from[[j]]!=to[[j]])==0)
  if(sum(to_from_same)>0){
    warning("Removing ",sum(to_from_same)," rows in which to and from are identical.")
    from <- from[!(to_from_same)]
    to <- to[!(to_from_same)]
    match_partial_entity <- match_partial_entity[!(to_from_same)]
    to_from_same <- sapply(1:length(to), function (j) sum(from[[j]]!=to[[j]])==0)
  }
  if(length(from)!=length(to)){
    stop("From and To must be the same length.")
  }
  if(sum(duplicated(from))>0){
    stop(paste0("The 'from' list contains duplicates. ",
    "Each term in the 'from' list must match to a unique term in the 'to' list. ",
    "Please resolve the duplicated 'from' terms: ", paste0(from[duplicated(from)],collapse=", ")))
  }
  
  #Section0: Determine num of recursive ####
  vectto <- unlist(to)
  vectfrom <- unlist(from)
  step0 <- which(vectto %in% vectfrom)
  to0 <- vectto[step0]
  carryovers <- to0[which(to0 %in% vectfrom)]
  times_to_repeat <- 1
  is_inf_loop <- F
  to_nmin1 <- to0
  while(length(carryovers)>0 & !is_inf_loop){
    from_n <- vectfrom[which(vectfrom %in% carryovers)]
    to_n <- unlist(to[
      sapply(from, function(s) sum(s %in% from_n)>0)])
    carryovers <- to_n[which(to_n %in% vectfrom)]
    is_inf_loop <- !(length(to_n) <length(to_nmin1)) & length(carryovers)>0
    to_nmin1 <- to_n
    times_to_repeat <- times_to_repeat + 1
  }
  b <- 1
  removedelements <- NULL
  viewedelements <- NULL
  while(b <= length(carryovers)){
    ndx <- which(sapply(from, function(s) carryovers[b] %in% s))
    start <- from[[ndx]]
    if(!(start %in% viewedelements)){
      viewedelements <- append(viewedelements, start)
      
      #remove any multi-entries that are in an inf loop
      if(length(to[[ndx]])>1){
        from <- from[-ndx]
        to <- to[-ndx]
        match_partial_entity <- match_partial_entity[-ndx]
        removedelements <- append(removedelements,start)
      }else{
        #remove the end of the chain  
        y=1
        nxtto <- to[[ndx]]
        currentndx <- which(from==nxtto)
        nxtfrom <- from[[which(from == nxtto)]]
        viewedelements <- append(viewedelements, nxtfrom)
        while(y <= length(carryovers) & nxtfrom!=start){
          nxtto <- to[[which(sapply(from, function(s) nxtfrom %in% s))]]
          currentndx <- which(from==nxtto)
          nxtfrom <- from[[which(from == nxtto)]]
          viewedelements <- append(viewedelements, nxtfrom)
          y<-y+1
        }
        if(nxtfrom==start){
          from <- from[-currentndx]
          to <- to[-currentndx]
          match_partial_entity <- match_partial_entity[-currentndx]
          removedelements <- append(removedelements, nxtfrom)
        }
      }
    }
    b <- b+1
  }
  if(is_inf_loop==T){
    warning(paste0("to/from terms ", paste0(carryovers, collapse = ", "), " were in an infinite loop. ",
                   "Resolved by removing 'from' elements ",paste0(removedelements, collapse=", ")))
    #rerun the bit of code that finds the number of times to repeat now that the loop is resolved  
    vectto <- unlist(to)
    vectfrom <- unlist(from)
    step0 <- which(vectto %in% vectfrom)
    to0 <- vectto[step0]
    carryovers <- to0[which(to0 %in% vectfrom)]
    times_to_repeat <- 1
    is_inf_loop <- F
    to_nmin1 <- to0
    while(length(carryovers)>0 & !is_inf_loop){
      from_n <- vectfrom[which(vectfrom %in% carryovers)]
      to_n <- unlist(to[
        sapply(from, function(s) sum(s %in% from_n)>0)])
      carryovers <- to_n[which(to_n %in% vectfrom)]
      is_inf_loop <- !(length(to_n) <length(to_nmin1)) & length(carryovers)>0
      to_nmin1 <- to_n
      times_to_repeat <- times_to_repeat + 1
    }
  }
  
  #Section 1: Drop "The"####
  #drop "^the" from the custom list automatically
  #the textnet extraction process already drops "^the"
  remove <- c("^_*The_","^_*the_","^_*THE_",
              "^_*The$","^_*the$","^_*THE$")
 
  index <- which(grepl(paste(remove,collapse='|'),from,perl=T))
  from[index] <- str_remove_all(from[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),to,perl=T))
  to[index] <- str_remove_all(to[index],paste(remove,collapse= '|'))
  
  #Section 2: Start Recursive Disambiguation ####
  for(z in 1:times_to_repeat){
    #Subsection 1: The lists ####
    
    sub_try_drop_forlists <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the from.
      #then return only the terms that actually changed
      #coerces tempv list into atomic vector of same length, which gives a warning unless suppressed.
      infrom <- which(suppressWarnings(str_detect(tempv,paste(fromregex,collapse='|'))))
      terms[infrom] <- tempv[infrom]
      
      return(terms)
    }
    
    multi_to <- sapply(1:length(to), function(w) length(to[[w]]) > 1)
    multi_from <- sapply(1:length(from), function(w) length(from[[w]]) > 1)
    
    if(sum(multi_from)>0){
      stop("Elements in 'from' should not be lists. Please change elements ", 
           paste0(which(multi_to), collapse = ", ") ," to a single character vector.")
    }
    if(sum(multi_to==T & match_partial_entity==T)>0){
      stop("Elements for which 'to' is a list may not be matched on a partial string. Please set match_partial_entity to F for these elements.")
    }
    
    #setting up regex of froms
    
    #only if there is only one entity in the cell.
    frompartial <- unlist(from[match_partial_entity])
    topartial <- unlist(to[match_partial_entity])
    fromfull <- unlist(from[!match_partial_entity & !multi_to])
    tofull <- unlist(to[!match_partial_entity & !multi_to])
    
    if(length(frompartial)>0){
      #beginning of word
      begf <- paste0("^",frompartial,concatenator)
      begt <- paste0(topartial,concatenator)
      #middle or end of word
      midf <- paste0(concatenator,frompartial)
      midt <- paste0(concatenator,topartial)
      #entire word
      wholef <- paste0("^",frompartial,"$")
      wholet <- topartial
      
      frompartial <- c(begf, midf, wholef)
      topartial <- c(begt,midt,wholet)
    }
    #entire word
    if(length(fromfull)>0){
      fromfull <- paste0("^",fromfull,"$")
      tofull <- tofull
    }

    
    fromregex <- c(frompartial, fromfull)
    toregex <- c(topartial, tofull)
    
    namedvect <- toregex
    names(namedvect) <- fromregex
    
    #if the extract matches something that's supposed to turn into a list, send it to a temp column
    if(sum(multi_to==T)>0){
      froms_of_multi_to <- from[multi_to]
      tos_of_multi_to <- to[multi_to]
      
      textnet_extract$edgelist$sourcetemp <- textnet_extract$edgelist$source
      textnet_extract$edgelist$targettemp <- textnet_extract$edgelist$target
      textnet_extract$nodelist$entity_cattemp <- textnet_extract$nodelist$entity_cat
      
      #Sub-subsection 1: Sourcetemp####
      index <- rep(F, length=length(textnet_extract$edgelist$sourcetemp))
      for(q in 1:length(froms_of_multi_to)){
        index <- ifelse(is.na(textnet_extract$edgelist$sourcetemp),
                        F, ifelse(textnet_extract$edgelist$sourcetemp == froms_of_multi_to[[q]], 
                                  T, index))
        
        textnet_extract$edgelist$sourcetemp <- ifelse(is.na(textnet_extract$edgelist$sourcetemp),
                           NA, ifelse(textnet_extract$edgelist$sourcetemp == froms_of_multi_to[[q]], 
                           tos_of_multi_to[q], textnet_extract$edgelist$sourcetemp))
        
      }
      notindex <- !index
      if(!is.null(try_drop)){
        textnet_extract$edgelist$sourcetemp <- sub_try_drop_forlists(try_drop, textnet_extract$edgelist$sourcetemp, notindex)
      }
      
      #Sub-subsection 2: Targettemp####
      index <- rep(F, length=length(textnet_extract$edgelist$targettemp))
      for(q in 1:length(froms_of_multi_to)){
        #progressively adds all of the multi-length entities to index as cycles through vals of q
        index <- ifelse(is.na(textnet_extract$edgelist$targettemp),
                        F, ifelse(textnet_extract$edgelist$targettemp == froms_of_multi_to[[q]], 
                                  T, index))
        
        textnet_extract$edgelist$targettemp <- ifelse(is.na(textnet_extract$edgelist$targettemp),
                                                      NA, ifelse(textnet_extract$edgelist$targettemp == froms_of_multi_to[[q]], 
                                                                 tos_of_multi_to[q], textnet_extract$edgelist$targettemp))
        
      }
      #everything in notindex is a single-length entry
      notindex <- !index
      if(!is.null(try_drop)){
        textnet_extract$edgelist$targettemp <- sub_try_drop_forlists(try_drop, textnet_extract$edgelist$targettemp, notindex)
      }
      
      #Sub-subsection 3: Entity_cattemp####
      index <- rep(F, length=length(textnet_extract$nodelist$entity_cattemp))
      for(q in 1:length(froms_of_multi_to)){
        index <- ifelse(is.na(textnet_extract$nodelist$entity_cattemp),
                        F, ifelse(textnet_extract$nodelist$entity_cattemp == froms_of_multi_to[[q]], 
                                  T, index))
        
        textnet_extract$nodelist$entity_cattemp <- ifelse(is.na(textnet_extract$nodelist$entity_cattemp),
                                                          NA, ifelse(textnet_extract$nodelist$entity_cattemp == froms_of_multi_to[[q]], 
                                                                     tos_of_multi_to[q], textnet_extract$nodelist$entity_cattemp))
        
      }
      notindex <- !index
      if(!is.null(try_drop)){
        textnet_extract$nodelist$entity_cattemp <- sub_try_drop_forlists(try_drop, textnet_extract$nodelist$entity_cattemp, notindex)
      }
      
      #Sub-subsection 4: Generating edges for each element in the multi-entries####
      
      textnet_extract$edgelist$length_source <- sapply(textnet_extract$edgelist$sourcetemp, length)
      textnet_extract$edgelist$length_target <- sapply(textnet_extract$edgelist$targettemp, length)
      rows <- textnet_extract$edgelist[rep(seq(1, nrow(textnet_extract$edgelist)), textnet_extract$edgelist$length_source)]
      rows$source <- unlist(textnet_extract$edgelist$sourcetemp)
      rowstarget <- rows[rep(seq(1, nrow(rows)), rows$length_target)]
      rowstarget$target <- unlist(rows$targettemp)
      
      rowstarget$sourcetemp <- rowstarget$targettemp <- rowstarget$length_source <- rowstarget$length_target<- NULL
      textnet_extract$edgelist <- rowstarget
      
      textnet_extract$nodelist$length_entitycat <- sapply(textnet_extract$nodelist$entity_cattemp, length)
      rows <- textnet_extract$nodelist[rep(seq(1, nrow(textnet_extract$nodelist)), textnet_extract$nodelist$length_entitycat)]
      rows$entity_cat <- unlist(textnet_extract$nodelist$entity_cattemp)
      rows$entity_cattemp <- rows$length_entitycat <- NULL
      textnet_extract$nodelist <- rows
      
    }
    
    #Subsection 2: Match Partial and Full Words (Non-List), and TryDrop####
    #if doesn't match, remove try_drop from the edgelist and nodelist and see if it matches
    sub_try_drop <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the to.
      #then return only the terms that actually changed
      
      infrom <- which(str_detect(tempv,paste(fromregex,collapse='|')))
      terms[infrom] <- str_replace_all(tempv[infrom],
                                       namedvect)
      
      #if removing the try_drop causes the entity to match the entire entry of a "to" column
      #make it that entity
      towhole <- paste0("^",to[!multi_to],"$")
      into <- which(str_detect(tempv,paste(towhole, collapse='|')))
      terms[into] <- tempv[into]
      
      return(terms)
    }
    
    index <- which(str_detect(textnet_extract$edgelist$source,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$edgelist$source) %in% index
    textnet_extract$edgelist$source[index] <- str_replace_all(textnet_extract$edgelist$source[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$source <- sub_try_drop(try_drop, textnet_extract$edgelist$source, notindex)
    }
    
    index <- which(str_detect(textnet_extract$edgelist$target,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$edgelist$target) %in% index
    textnet_extract$edgelist$target[index] <- str_replace_all(textnet_extract$edgelist$target[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$target <- sub_try_drop(try_drop, textnet_extract$edgelist$target, notindex)
    }
    
    index <- which(str_detect(textnet_extract$nodelist$entity_cat,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$nodelist$entity_cat) %in% index
    textnet_extract$nodelist$entity_cat <- str_replace_all(textnet_extract$nodelist$entity_cat,
                                                                  namedvect)
    if(!is.null(try_drop)){
      textnet_extract$nodelist$entity_cat <- sub_try_drop(try_drop, textnet_extract$nodelist$entity_cat, notindex)
    }
    
    
  }
  #Section 3: Clean-Up####

  colnames(textnet_extract$nodelist)[3] <- "num_appearances"
  #consolidates upper and lower case spellings 
  #(this is done after the above cleaning because acronyms and 
  #abbreviations can be case-sensitive)
  textnet_extract$nodelist$entity_cat <- tolower(textnet_extract$nodelist$entity_cat)
  
  #removes the again
  remove <- c("^_*the_","^_*the$")
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$nodelist$entity_cat,perl=T))
  textnet_extract$nodelist$entity_cat[index] <- str_remove_all(
    textnet_extract$nodelist$entity_cat[index],paste(remove,collapse= '|'))
  
  #redoes count of num_appearances, prioritizes most common entity_type by using desc()
  textnet_extract$nodelist <- textnet_extract$nodelist[,c(.SD,sum(num_appearances)),by=entity_cat]
  textnet_extract$nodelist <-arrange(textnet_extract$nodelist, desc(num_appearances))
  textnet_extract$nodelist <- textnet_extract$nodelist[!duplicated(entity_cat),]
  textnet_extract$nodelist$num_appearances <- NULL
  colnames(textnet_extract$nodelist)[3] <- "num_appearances"
  
  #send edgelist tolower
  textnet_extract$edgelist$source <- tolower(textnet_extract$edgelist$source)
  textnet_extract$edgelist$target <- tolower(textnet_extract$edgelist$target)
  
  #removes "the" again
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$source,perl=T))
  textnet_extract$edgelist$source[index] <- str_remove_all(
    textnet_extract$edgelist$source[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$target,perl=T))
  textnet_extract$edgelist$target[index] <- str_remove_all(
    textnet_extract$edgelist$target[index],paste(remove,collapse= '|'))
  
  #remove empty strings 
  textnet_extract$edgelist$source <- ifelse(is.na(textnet_extract$edgelist$source),
                                            textnet_extract$edgelist$source,
                                            ifelse(nchar(textnet_extract$edgelist$source)==0,NA,
                                                   textnet_extract$edgelist$source))
  textnet_extract$edgelist$target <- ifelse(is.na(textnet_extract$edgelist$target),
                                            textnet_extract$edgelist$target,
                                            ifelse(nchar(textnet_extract$edgelist$target)==0,NA,
                                                   textnet_extract$edgelist$target))
  textnet_extract$nodelist <- textnet_extract$nodelist[nchar(textnet_extract$nodelist$entity_cat)>0]
  
  #remove any incomplete edges that may have resulted from the disambiguation process
  #this function should not cause any additions to the existing incomplete edges in a usual case
  textnet_extract$edgelist$edgeiscomplete <- !is.na(textnet_extract$edgelist$source) & !is.na(textnet_extract$edgelist$target)
  textnet_extract$edgelist[, `:=`(hascompleteedge, any(edgeiscomplete==T)), by = c("doc_sent_verb")]
  textnet_extract$edgelist <- textnet_extract$edgelist %>% filter((hascompleteedge==T & edgeiscomplete==T) | hascompleteedge==F)
  textnet_extract$edgelist$hascompleteedge <- NULL
  #Section 4: Return####
  return(textnet_extract)
}

