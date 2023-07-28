#behavior: if there is a vector of entities in a "to" cell, it duplicates the relevant
#rows such that there is an edge for each of the entities in the "to" cell
#vector entities are disambiguated first.
#afterward, it substitutes strings in order of appearance in from and to. 

#drops leading "the" to help facilitate more matches
#if match_partial_entity is T for that element, can match on just a word boundary
#from could be "acronyms" from find_acronyms output
#to could be "names" from find_acronyms output

#textnet_extract should be the result of custom_entity_extract

#if try_drop is supplied, it tries to find a match in "from" among the edgelists and nodelists first.
#if it can't find a match, it drops try_drop from the nonmatching edgelist and nodelist elements and tries again

#if recursive is true, runs it multiple times to reach the end of the chain.

#returns cleaned textnet_extract


disambiguate <- function(from, to, match_partial_entity=rep(F, length(from)), textnet_extract, try_drop=NULL, recursive){
  library(igraph)
  library(ggraph)
  library(sna)
  library(stringr)
  library(dplyr)
  
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
  #drop "^the" from both the nodelist/edgelist and the custom list automatically
  remove <- c("^_*The_","^_*the_","^_*THE","^_*The$","_*the$","^_*THE$")
 
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$source,perl=T))
  textnet_extract$edgelist$source[index] <- str_remove_all(textnet_extract$edgelist$source[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$target,perl=T))
  textnet_extract$edgelist$target[index] <- str_remove_all(textnet_extract$edgelist$target[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$nodelist$entity_cat,perl=T))
  textnet_extract$nodelist$entity_cat[index] <- str_remove_all(textnet_extract$nodelist$entity_cat[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),from,perl=T))
  from[index] <- str_remove_all(from[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),to,perl=T))
  to[index] <- str_remove_all(to[index],paste(remove,collapse= '|'))
  
  #Section 2: Start Recursive Disambiguation ####
  for(z in 1:times_to_repeat){
    #Subsection 1: The lists ####
    
    multi_to <- sapply(1:length(to), function(w) length(to[[w]]) > 1)
    multi_from <- sapply(1:length(from), function(w) length(from[[w]]) > 1)
    
    if(sum(multi_from)>0){
      stop("Elements in 'from' may not be lists. Please change elements ", 
           paste0(which(multi_to), collapse = ", ") ," to a single character vector.")
    }
    if(sum(multi_to==T & match_partial_entity==T)>0){
      stop("Elements for which 'to' is a list may not be matched on a partial string. Please set match_partial_entity to F for these elements.")
    }
    
    #if the extract matches something that's supposed to turn into a list, send it to a temp column
    
    
    sub_try_drop_forlists <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the from.
      #then return only the terms that actually changed
      
      infrom <- which(grepl(paste(fromregex,collapse='|'),tempv,perl=T))
      terms[infrom] <- tempv[infrom]
      
      
      return(terms)
    }
    
    froms_of_multi_to <- from[multi_to]
    tos_of_multi_to <- to[multi_to]
    
    textnet_extract$edgelist$sourcetemp <- textnet_extract$edgelist$source
    textnet_extract$edgelist$targettemp <- textnet_extract$edgelist$target
    textnet_extract$nodelist$entity_cattemp <- textnet_extract$nodelist$entity_cat
    
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
    
    index <- rep(F, length=length(textnet_extract$edgelist$targettemp))
    for(q in 1:length(froms_of_multi_to)){
      index <- ifelse(is.na(textnet_extract$edgelist$targettemp),
                      F, ifelse(textnet_extract$edgelist$targettemp == froms_of_multi_to[[q]], 
                                T, index))
      
      textnet_extract$edgelist$targettemp <- ifelse(is.na(textnet_extract$edgelist$targettemp),
                                                    NA, ifelse(textnet_extract$edgelist$targettemp == froms_of_multi_to[[q]], 
                                                               tos_of_multi_to[q], textnet_extract$edgelist$targettemp))
      
    }
    notindex <- !index
    if(!is.null(try_drop)){
      textnet_extract$edgelist$targettemp <- sub_try_drop_forlists(try_drop, textnet_extract$edgelist$targettemp, notindex)
    }

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
    
    #Subsection 2: Match Partial and Full Words (Non-List), and TryDrop####
    #if doesn't match, remove try_drop from the edgelist and nodelist and see if it matches
    sub_try_drop <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the to.
      #then return only the terms that actually changed
      
      infrom <- which(grepl(paste(fromregex,collapse='|'),tempv,perl=T))
      terms[infrom] <- str_replace_all(tempv[infrom],
                                       namedvect)
      
      #if removing the try_drop causes the entity to match the entire entry of a "to" column
      #make it that entity
      towhole <- paste0("^",to[!multi_to],"$")
      into <- which(grepl(paste(towhole, collapse='|'),tempv,perl=T))
      terms[into] <- tempv[into]
      
      return(terms)
    }
    
    #only if there is only one entity in the cell.
    frompartial <- from[match_partial_entity]
    topartial <- to[match_partial_entity]
    fromfull <- from[!match_partial_entity & !multi_to]
    tofull <- to[!match_partial_entity & !multi_to]
    
    if(length(frompartial)>0){
      #beginning of word
      begf <- paste0("^",frompartial,"_")
      begt <- paste0(topartial,"_")
      #middle or end of word
      midf <- paste0("_",frompartial)
      midt <- paste0("_",topartial)
      #entire word
      wholef <- paste0("^",frompartial,"$")
      wholet <- topartial
      
      frompartial <- c(begf, midf, wholef)
      topartial <- c(begt,midt,wholet)
    }
    #entire word
    fromfull <- paste0("^",fromfull,"$")
    tofull <- tofull
    
    fromregex <- c(frompartial, fromfull)
    toregex <- c(topartial, tofull)
    
    namedvect <- toregex
    names(namedvect) <- fromregex
    
    index <- which(grepl(paste(fromregex,collapse='|'),textnet_extract$edgelist$source,perl=T))
    notindex <- which(!grepl(paste(fromregex,collapse='|'),textnet_extract$edgelist$source,perl=T))
    textnet_extract$edgelist$source[index] <- str_replace_all(textnet_extract$edgelist$source[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$source <- sub_try_drop(try_drop, textnet_extract$edgelist$source, notindex)
    }
    index <- which(grepl(paste(fromregex,collapse='|'),textnet_extract$edgelist$target,perl=T))
    notindex <- which(!grepl(paste(fromregex,collapse='|'),textnet_extract$edgelist$target,perl=T))
    textnet_extract$edgelist$target[index] <- str_replace_all(textnet_extract$edgelist$target[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$target <- sub_try_drop(try_drop, textnet_extract$edgelist$target, notindex)
    }
    index <- which(grepl(paste(fromregex,collapse='|'),textnet_extract$nodelist$entity_cat,perl=T))
    notindex <- which(!grepl(paste(fromregex,collapse='|'),textnet_extract$nodelist$entity_cat,perl=T))
    textnet_extract$nodelist$entity_cat[index] <- str_replace_all(textnet_extract$nodelist$entity_cat[index],
                                                                  namedvect)
    if(!is.null(try_drop)){
      textnet_extract$nodelist$entity_cat <- sub_try_drop(try_drop, textnet_extract$nodelist$entity_cat, notindex)
    }
    
    
  }

  #Section 3: Clean-Up####
  clean_entities(from)
  clean_entities(to)
  #TODO remove empty entities from edgelist and nodelist. consolidate duplicates in nodelist.
  #colnames(nodelist)[3] <- "num_appearances"
  #nodelist <- nodelist %>% arrange(desc(num_appearances))
  
  #get rid of duplicates
  #nodelist <- nodelist %>%
   # group_by(entity_cat) %>%
    #arrange(desc(num_appearances)) %>%
    #filter(row_number()==1)
  
}