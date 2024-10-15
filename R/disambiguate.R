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
#' @importFrom magrittr %>%
#' @export
#'

#if recursive is true, runs it multiple times to reach the end of the chain.

disambiguate <- function(textnet_extract, from, to, match_partial_entity=rep(F, length(from)), try_drop=NULL, recursive=T, concatenator="_"){
  options(warn=1)
  #Data formatting checks####
  multi_to <- sapply(1:length(to), function(w) length(to[[w]]) > 1)
  multi_from <- sapply(1:length(from), function(w) length(from[[w]]) > 1)
  
  if(sum(multi_from)>0){
    stop("Elements in 'from' should not be lists of length greater than 1. Please change elements ", 
         paste0(which(multi_to), collapse = ", ") ," to a single character vector.")
  }
  if(length(to) != length(match_partial_entity) | 
     length(from) != length(match_partial_entity) |
     length(to) != length(from)){
    stop("The arguments to, from, and match_partial_entity must all be the same length.")
  }
  if(sum(multi_to==T & match_partial_entity==T)>0){
    stop("Elements for which 'to' is a list of length greater than 1 may not be matched on a partial string. Please set match_partial_entity to F for these elements.")
  }
  
  which_froms_are_subsets_of_tos <- which(sapply(seq_along(to), function (x) sum(
    stringr::str_detect(string = to[[x]], pattern = from[[x]])))==1)
  if(length(which_froms_are_subsets_of_tos)>0 & sum(match_partial_entity[which_froms_are_subsets_of_tos])>0){
    match_partial_entity[which_froms_are_subsets_of_tos] <- F
    warning("Some elements in 'from' are substrings of the corresponding elements in 'to.' The match_partial_entity value has been automatically changed to F for these 'from' elements to avoid unexpected behavior: ",
     paste0(from[which_froms_are_subsets_of_tos], collapse = ", ")
            )
  }
  
  
  
  to_from_same <- sapply(1:length(to), function (j) sum(from[[j]]!=to[[j]])==0)
  if(sum(to_from_same)>0){
    warning("Removing ",sum(to_from_same)," rows in which to and from are identical: ", paste0(from[to_from_same], collapse = ", "))
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
  previous_carryover_length <- length(carryovers)
  times_to_repeat <- 1
  is_inf_loop <- F
  to_nmin1 <- to0
  while(length(carryovers)>0 & !is_inf_loop){
    from_n <- vectfrom[which(vectfrom %in% carryovers)]
    to_n <- unlist(to[
      sapply(from, function(s) sum(s %in% from_n)>0)])
    carryovers <- to_n[which(to_n %in% vectfrom)]
    is_inf_loop <- !(length(to_n) <length(to_nmin1)) & length(carryovers)>0 &
      length(carryovers) == previous_carryover_length
    to_nmin1 <- to_n
    times_to_repeat <- times_to_repeat + 1
    previous_carryover_length <- length(carryovers)
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
    warning(paste0("The following to/from terms were in an infinite loop: ", paste0(carryovers, collapse = ", "), ". ",
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
  
  #Section0.5: Resolve partial match infinite loops ####
  #if from_n matches 
  vectto <- unlist(to)
  vectfrom <- unlist(from)
  #does not include completely matched froms, since those are already resolved:
  whichpartiallymatchedfroms <- which(sapply(seq_along(vectfrom), function (x) sum(
    stringr::str_detect(string = vectto, pattern = vectfrom[[x]])))>0)
  fromsthatallowpartialmatching <- vectfrom[whichpartiallymatchedfroms[match_partial_entity[
    whichpartiallymatchedfroms]]]
  
  step0 <- which(sapply(lapply(seq_along(vectto),
            function(x) stringr::str_detect(vectto[x], 
                  fromsthatallowpartialmatching)), function(x) sum(x))>0)
  to0 <- vectto[step0]
  carryovers <- to0[which(sapply(lapply(seq_along(to0),
                  function(x) stringr::str_detect(to0[x], 
                         fromsthatallowpartialmatching)), function(x) sum(x))>0)]
  is_inf_loop <- F
  to_nmin1 <- to0
  while(length(carryovers)>0 & !is_inf_loop){
    from_n <- vectfrom[which(sapply(lapply(seq_along(vectfrom),
               function(x) stringr::str_detect(carryovers, vectfrom[x])), function(x) sum(x))>0)]
    to_n <- unlist(to[
      sapply(from, function(s) sum(s %in% from_n)>0)])
    carryovers <- to_n[which(sapply(lapply(seq_along(to_n),
                        function(x) stringr::str_detect(to_n[x], 
                              fromsthatallowpartialmatching)), function(x) sum(x))>0)]
    is_inf_loop <- !(length(to_n) <length(to_nmin1)) & length(carryovers)>0
    to_nmin1 <- to_n
  }
  problematicfroms <- which(sapply(lapply(seq_along(vectfrom),
                      function(x) stringr::str_detect(carryovers, vectfrom[x])), function(x) sum(x))>0)
  match_partial_entity[problematicfroms] <- F
  
  
  if(is_inf_loop==T){
    warning(paste0("The following to/from terms were in an infinite loop because of partial matching allowments: ", paste0(carryovers, collapse = ", "), ". ",
                   "Resolved by automatically setting match_partial_entity to F for elements ", paste0(problematicfroms, collapse = ", ")))
    
  }
  
  
  #Section 1: Drop "The"####
  #drop "^the" from the custom list and entity list automatically
  remove <- c(paste0("^",concatenator,"*The",concatenator),
              paste0("^",concatenator,"*the",concatenator),
              paste0("^",concatenator,"*THE",concatenator),
              paste0("^",concatenator,"*The$"),
              paste0("^",concatenator,"*the$"),
              paste0("^",concatenator,"*THE$"))
 
  index <- which(grepl(paste(remove,collapse='|'),from,perl=T))
  from[index] <- stringr::str_remove_all(from[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),to,perl=T))
  to[index] <- stringr::str_remove_all(to[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'), textnet_extract$edgelist$source,perl=T))
  textnet_extract$edgelist$source[index] <- stringr::str_remove_all(textnet_extract$edgelist$source[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'), textnet_extract$edgelist$target,perl=T))
  textnet_extract$edgelist$target[index] <- stringr::str_remove_all(textnet_extract$edgelist$target[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'), textnet_extract$nodelist$entity_name,perl=T))
  textnet_extract$nodelist$entity_name[index] <- stringr::str_remove_all(textnet_extract$nodelist$entity_name[index],paste(remove,collapse= '|'))
  
  #Section 2: Start Recursive Disambiguation ####
  for(z in 1:times_to_repeat){
    #Subsection 1: The lists ####
    
    sub_try_drop_forlists <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- stringr::str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the from.
      #then return only the terms that actually changed
      #coerces tempv list into atomic vector of same length, which gives a warning unless suppressed.
      infrom <- which(suppressWarnings(stringr::str_detect(tempv,paste(fromregex,collapse='|'))))
      terms[infrom] <- tempv[infrom]
      
      return(terms)
    }
    #defines this again after initial cleaning of to and from columns
    multi_to <- sapply(1:length(to), function(w) length(to[[w]]) > 1)
    multi_from <- sapply(1:length(from), function(w) length(from[[w]]) > 1)
    
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
      textnet_extract$nodelist$entity_nametemp <- textnet_extract$nodelist$entity_name
      
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
      
      #Sub-subsection 3: entity_nametemp####
      index <- rep(F, length=length(textnet_extract$nodelist$entity_nametemp))
      for(q in 1:length(froms_of_multi_to)){
        index <- ifelse(is.na(textnet_extract$nodelist$entity_nametemp),
                        F, ifelse(textnet_extract$nodelist$entity_nametemp == froms_of_multi_to[[q]], 
                                  T, index))
        
        textnet_extract$nodelist$entity_nametemp <- ifelse(is.na(textnet_extract$nodelist$entity_nametemp),
                                                          NA, ifelse(textnet_extract$nodelist$entity_nametemp == froms_of_multi_to[[q]], 
                                                                     tos_of_multi_to[q], textnet_extract$nodelist$entity_nametemp))
        
      }
      notindex <- !index
      if(!is.null(try_drop)){
        textnet_extract$nodelist$entity_nametemp <- sub_try_drop_forlists(try_drop, textnet_extract$nodelist$entity_nametemp, notindex)
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
      
      textnet_extract$nodelist$length_entitycat <- sapply(textnet_extract$nodelist$entity_nametemp, length)
      rows <- textnet_extract$nodelist[rep(seq(1, nrow(textnet_extract$nodelist)), textnet_extract$nodelist$length_entitycat)]
      rows$entity_name <- unlist(textnet_extract$nodelist$entity_nametemp)
      rows$entity_nametemp <- rows$length_entitycat <- NULL
      textnet_extract$nodelist <- rows
      
    }
    
    #Subsection 2: Match Partial and Full Words (Non-List), and TryDrop####
    #if doesn't match, remove try_drop from the edgelist and nodelist and see if it matches
    sub_try_drop <- function(remove, terms, didntmatch){
      tempv <- terms
      rem <- grepl(paste(remove,collapse = '|'),terms,perl = T)
      tempv[rem ==T & didntmatch==T] <- stringr::str_remove_all(tempv[rem==T& didntmatch==T],paste(remove,collapse = '|'))
      
      #step two, now that try_drop is removed, does it match the from? if so, substitute the to.
      #then return only the terms that actually changed
      
      infrom <- which(stringr::str_detect(tempv,paste(fromregex,collapse='|')))
      terms[infrom] <- stringr::str_replace_all(tempv[infrom],
                                       namedvect)
      
      #if removing the try_drop causes the entity to match the entire entry of a "to" column
      #make it that entity
      towhole <- paste0("^",to[!multi_to],"$")
      into <- which(stringr::str_detect(tempv,paste(towhole, collapse='|')))
      terms[into] <- tempv[into]
      
      return(terms)
    }
    
    index <- which(stringr::str_detect(textnet_extract$edgelist$source,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$edgelist$source) %in% index
    textnet_extract$edgelist$source[index] <- stringr::str_replace_all(textnet_extract$edgelist$source[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$source <- sub_try_drop(try_drop, textnet_extract$edgelist$source, notindex)
    }
    
    index <- which(stringr::str_detect(textnet_extract$edgelist$target,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$edgelist$target) %in% index
    textnet_extract$edgelist$target[index] <- stringr::str_replace_all(textnet_extract$edgelist$target[index],
                                                              namedvect)
    if(!is.null(try_drop)){
      textnet_extract$edgelist$target <- sub_try_drop(try_drop, textnet_extract$edgelist$target, notindex)
    }
    
    index <- which(stringr::str_detect(textnet_extract$nodelist$entity_name,paste(fromregex,collapse='|')))
    notindex <- 1:length(textnet_extract$nodelist$entity_name) %in% index
    textnet_extract$nodelist$entity_name <- stringr::str_replace_all(textnet_extract$nodelist$entity_name,
                                                                  namedvect)
    if(!is.null(try_drop)){
      textnet_extract$nodelist$entity_name <- sub_try_drop(try_drop, textnet_extract$nodelist$entity_name, notindex)
    }
    
    
  }
  #Subsection 2.5: Try-drop node collapsing####
  #this collapses nodes in textnet_extract that would be identical if try_drop were removed
  if(!is.null(try_drop)){
    
    #if source node with try_drop dropped matches a different node in the nodelist,
    #convert it, and remove the node that's identical that contains try_drop
    tempv <- textnet_extract$edgelist$source
    rem <- grepl(paste(try_drop,collapse = '|'),textnet_extract$edgelist$source,perl = T)
    abbrevs <- stringr::str_remove_all(tempv[rem==T],paste(try_drop,collapse = '|'))
    #now that try_drop is removed, does it match another node? if so, substitute the name of that other node.
    dropped_in_terms <- which(abbrevs %in% 
                                textnet_extract$nodelist$entity_name)
    if(length(dropped_in_terms)>0){
      for(i in 1:length(dropped_in_terms)){
        #remove the dangling nodelist node which now has no edgelist referents
        textnet_extract$nodelist$entity_name[
          textnet_extract$nodelist$entity_name==textnet_extract$edgelist$source[
            which(rem)[dropped_in_terms[i]]]] <- 
          abbrevs[dropped_in_terms[i]]
        #then remove its referent in the source list
        textnet_extract$edgelist$source[which(rem)[dropped_in_terms[i]]] <- abbrevs[dropped_in_terms[i]]

      }
    }
    
    #same thing but now with target nodes
    #if target node with try_drop dropped matches a different node in the nodelist,
    #convert it, and remove the node that's identical that contains try_drop
    tempv <- textnet_extract$edgelist$target
    rem <- grepl(paste(try_drop,collapse = '|'),textnet_extract$edgelist$target,perl = T)
    abbrevs <- stringr::str_remove_all(tempv[rem==T],paste(try_drop,collapse = '|'))
    #now that try_drop is removed, does it match another node? if so, substitute the name of that other node.
    dropped_in_terms <- which(abbrevs %in% 
                                textnet_extract$nodelist$entity_name)
    if(length(dropped_in_terms)>0){
      for(i in 1:length(dropped_in_terms)){
        #remove the dangling nodelist node which now has no edgelist referents
        textnet_extract$nodelist$entity_name[
          textnet_extract$nodelist$entity_name==textnet_extract$edgelist$target[
            which(rem)[dropped_in_terms[i]]]] <- 
          abbrevs[dropped_in_terms[i]]
        #then remove its referent in the target list
        textnet_extract$edgelist$target[which(rem)[dropped_in_terms[i]]] <- abbrevs[dropped_in_terms[i]]
        
      }
    }
    
    #same thing but now with nodelist nodes
    #if nodelist node with try_drop dropped matches a different node in the nodelist,
    #convert it, and remove the node that's identical that contains try_drop
    tempv <- textnet_extract$nodelist$entity_name
    rem <- grepl(paste(try_drop,collapse = '|'),textnet_extract$nodelist$entity_name,perl = T)
    abbrevs <- stringr::str_remove_all(tempv[rem==T],paste(try_drop,collapse = '|'))
    #now that try_drop is removed, does it match another node? if so, substitute the name of that other node.
    dropped_in_terms <- which(abbrevs %in% 
                                textnet_extract$nodelist$entity_name)
    if(length(dropped_in_terms)>0){
      for(i in 1:length(dropped_in_terms)){
        #remove its referent in the target list
        textnet_extract$nodelist$entity_name[which(rem)[dropped_in_terms[i]]] <- abbrevs[dropped_in_terms[i]]
        
      }
    }
    
  }
 
    
    
    
  
  #Section 3: Clean-Up####

  
  #consolidates upper and lower case spellings 
  #(this is done after the above cleaning because acronyms and 
  #abbreviations can be case-sensitive)
  textnet_extract$nodelist$entity_name <- tolower(textnet_extract$nodelist$entity_name)
  
  #removes the again
  remove <- c(paste0("^",concatenator,"*the_"),paste0("^",concatenator,"*the$"))
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$nodelist$entity_name,perl=T))
  textnet_extract$nodelist$entity_name[index] <- stringr::str_remove_all(
    textnet_extract$nodelist$entity_name[index],paste(remove,collapse= '|'))
  
  #redoes count of num_appearances, prioritizes most common entity_type by using desc()
  textnet_extract$nodelist <- textnet_extract$nodelist[,c(.SD,"new_appr" = sum(num_appearances)),by=entity_name]
  textnet_extract$nodelist <- dplyr::arrange(textnet_extract$nodelist, dplyr::desc(num_appearances))
  
  #if any other node attribute columns, prioritizes the most common entity_type that is not NA
  for(attr in colnames(textnet_extract$nodelist)[
    ! colnames(textnet_extract$nodelist) %in% c(
      "entity_name","entity_type","num_appearances","new_appr")]){
    #take a subset of the dataframe where attr is not na, then take the first row by entity_name group and
    #set the attr to that for all instances of the entity_name
    attrnotna <- textnet_extract$nodelist[as.vector(!is.na(textnet_extract$nodelist[,..attr])),]
    attrnotna <- attrnotna[!base::duplicated(entity_name),]
    for(rw in 1:nrow(textnet_extract$nodelist)){
      set(textnet_extract$nodelist, rw, attr, unlist(ifelse(
        textnet_extract$nodelist[rw,"entity_name"] %in% attrnotna$entity_name,
        attrnotna[as.vector(attrnotna$entity_name)==as.vector(textnet_extract$nodelist[rw,"entity_name"]),..attr],
        textnet_extract$nodelist[rw,..attr])))
    }
  }
  
  textnet_extract$nodelist <- textnet_extract$nodelist[!base::duplicated(entity_name),]
  textnet_extract$nodelist$num_appearances <- NULL
  colnames(textnet_extract$nodelist)[colnames(textnet_extract$nodelist)=="new_appr"] <- "num_appearances"
  
  
  
  #send edgelist tolower
  textnet_extract$edgelist$source <- tolower(textnet_extract$edgelist$source)
  textnet_extract$edgelist$target <- tolower(textnet_extract$edgelist$target)
  
  #removes "the" again
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$source,perl=T))
  textnet_extract$edgelist$source[index] <- stringr::str_remove_all(
    textnet_extract$edgelist$source[index],paste(remove,collapse= '|'))
  
  index <- which(grepl(paste(remove,collapse='|'),textnet_extract$edgelist$target,perl=T))
  textnet_extract$edgelist$target[index] <- stringr::str_remove_all(
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
  textnet_extract$nodelist <- textnet_extract$nodelist[nchar(textnet_extract$nodelist$entity_name)>0]
  
  #remove any incomplete edges that may have resulted from the disambiguation process
  #this function should not cause any additions to the existing incomplete edges in a usual case
  textnet_extract$edgelist$edgeiscomplete <- !is.na(textnet_extract$edgelist$source) & !is.na(textnet_extract$edgelist$target)
  textnet_extract$edgelist[, `:=`(hascompleteedge, any(edgeiscomplete==T)), by = c("doc_sent_verb")]
  textnet_extract$edgelist <- textnet_extract$edgelist %>% dplyr::filter((hascompleteedge==T & edgeiscomplete==T) | hascompleteedge==F)
  textnet_extract$edgelist$hascompleteedge <- NULL
  #Section 4: Return####
  return(textnet_extract)
}

