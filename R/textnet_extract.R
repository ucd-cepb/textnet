# Exported function
# textnet_extract 

#' Takes a parsed spacy document and uses dependency parsing to generate an edgelist, nodelist, verblist, and apposititve list
#'
#' @param x parsed spacy document
#' @param concatenator how entity parts are concatenated (defaults to "_")
#' @param file location where an list object with an edgelist, nodelist, verblist, and appositivelist should be saved as .RDS file
#' @param cl number of cores to crawl sentences in parallel (defaults to 1)
#' @param keep_entities character vector of spacy entity types to retain, defaults to people (PERSON), organizations (ORG), and geographic entities (GPE)
#' @param return_to_memory boolean for whether function should return final result as workspace object
#' @param keep_incomplete_edges Boolean. If T, keeps edges with only a source or target but not both
#' @return data frame with original parsed sentence + added dependency parsing
#' 
#' @import data.table
#' @importFrom magrittr %>%
#' @export
#'

textnet_extract <- function (x, concatenator = "_",file = NULL,cl = 1,
                                    keep_entities = c('ORG','GPE','PERSON'),
                                    return_to_memory = T,keep_incomplete_edges=F) {

  ### note this should be an error 
  if(is.null(file) && return_to_memory == F){stop("function not set to save output OR return object to memory")}
  x <- data.table::as.data.table(x)

  #removes invalid sentences with no verb
  #only keeps sentences that have a subject and object
  #does not keep sentences with compound subject and no object
  #list of subj and obj tags from https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
  dep_rels_subj_keep <- c('nsubj','nsubjpass','csubj','csubjpass','agent','expl')        
  dep_rels_obj_keep <- c('pobj','iobj','dative','attr','dobj','oprd','ccomp','xcomp','acomp','pcom') 

  x <- x[,keep:=any(dep_rel %in% dep_rels_subj_keep) & any(dep_rel %in% dep_rels_obj_keep) & any(pos=='VERB'),by=.(doc_id,sentence_id)]
  x <- x[keep==T,]
  x$doc_sent <- paste0(x$doc_id, "_", x$sentence_id)


  #entity_type <- entity <- iob <- entity_id <- .SD <- `:=` <- sentence_id <- doc_id <- NULL
  if (!"entity" %in% names(x)) {
    stop("no entities in parsed object: rerun spacy_parse() with entity = TRUE")
  }
  
  sentence_splits <- split(x,x$doc_sent)
  print(paste0('crawling ',length(sentence_splits),' sentences'))
  parse_list <- pbapply::pblapply(sentence_splits,function(y) data.table::as.data.table(crawl_sentence(y)),cl = cl)
  x <- data.table::rbindlist(mapply(function(x,y) cbind(x,y),x = sentence_splits,y = parse_list,SIMPLIFY = F))
  
  #"remove" is null because we don't want to remove anything token-wise, but only concatenated-entity-wise
  x <- entity_consolidate_replicate(x,concatenator, remove=NULL)
  
  remove_nums <- ifelse("DATE" %in% keep_entities | "CARDINAL" %in% keep_entities |
                          "QUANTITY" %in% keep_entities | "TIME" %in% keep_entities |
                          "MONEY" %in% keep_entities | "PERCENT" %in% keep_entities, F, T)
  x$entity_name <- clean_entities(x$entity_name,remove_nums)
  
  #by entity, check which of the head_token_ids in the group is NOT in the list of token_ids in the group.
  #then note the doc_sent of the group. Then within the doc_sent, find the row corresponding to the 
  #token that is first (made a choice here to match only the first; probably there is only one anyway) 
  #in the non-matching tokens list and grab its entity_name; which is the full name of the appos. the 
  #abbrev is the entity_name of the appos group
  xappos <- x[x$source_or_target=="appos" & nchar(x$entity_name)>0 & x$entity_type%in%keep_entities,]
  cat_splits <- split(xappos,list(xappos$doc_sent, xappos$entity_id),drop=T)
  apposlist <- pbapply::pblapply(cat_splits, function(z) {
      anchor <- which(!(z$head_token_id %in% z$token_id))[1]
      if(x[x$doc_sent==z$doc_sent[1] & x$token_id==z$head_token_id[anchor],"entity_type"]%in%keep_entities){
        print(x[x$doc_sent==z$doc_sent[1] & x$token_id==z$head_token_id[anchor],])
        as.data.table(cbind("abbrev" = z[1,"entity_name"],
                            "fullname" = x[x$doc_sent==z$doc_sent[1] & x$token_id==z$head_token_id[anchor],"entity_name"]))
        
      }
  }, cl=cl)
  apposlist <- data.table::rbindlist(apposlist)
  apposlist <- base::unique(apposlist[nchar(apposlist$fullname)>0,])
    
  #remove aux helpers functioning as aux; xcomp verbs; and appositives
  x <- x[!((pos=="AUX" & dep_rel %in% c("aux","auxpass")) |
             (source_or_target=="appos") | (pos=="VERB" & dep_rel =="xcomp")),]
  
  #make unique head_verb_id and head_token_id identifier for each doc_sent
  x[, `:=`(doc_sent_verb, paste0(doc_sent, "_", head_verb_id))]
  x[, `:=`(doc_sent_parent, paste0(doc_sent, "_", parent_verb_id))]
  x[, `:=`(doc_sent_head_tok, paste0(doc_sent, "_", head_token_id))]
    
  nodelist <- x[nchar(x$entity_name)>0,]
    
  #tag each head_tok that has a negation child
  x[, `:=`(neg, any(dep_rel=="neg")), by = doc_sent_verb]
  
  #data table of verb properties
  verb_dt <- x[pos %in% c("AUX","VERB"),]
  
  verb_dt <- verb_dt[,list(doc_sent_verb, head_verb_id, head_verb_name, head_verb_lemma, head_verb_tense, parent_verb_id, helper_lemma, helper_token, xcomp_verb, xcomp_helper_lemma, xcomp_helper_token, doc_sent_parent, neg)]

    
  #remove verbs with neg
  verb_dt <- verb_dt[neg==F]
    
  #does the verb have any sources? do this before removing non-entities
  x[, `:=`(has_sources, any(source_or_target=="source")), by = doc_sent_verb]
  
  ### there are duplicates here because there can be multiple tokens (each of which has its own row) associated with one verb ###
  
  source_target_list <- x[,.(entity_name, entity_id, entity_type, source_or_target, doc_sent_verb, doc_sent_parent, has_sources)]
  source_target_list <- source_target_list[!duplicated(source_target_list),]
  ## dt of verbs with without source
  unsourced_verbs <- source_target_list[has_sources==F,]
  ## dt of verbs with source that are also a source
  
  sources <- source_target_list[has_sources==T & source_or_target=='source',]
    
  ## find sources associated with the unsourced verbs' parent verbs
  adopted_source_ids <- lapply(seq_along(unsourced_verbs$doc_sent_parent), function(i)
           grep(unsourced_verbs$doc_sent_parent[i],sources$doc_sent_verb, value=F))
  
  #create a copy of the adopted sources, where the child doc_sent_verb 
  #overwrites the original source's doc_sent_verb
  adopted_sources <- lapply(seq_along(adopted_source_ids), function(i){
      if(length(adopted_source_ids[[i]])>0){
          tempsources <- sources[adopted_source_ids[[i]],]
          tempsources$doc_sent_verb <- unsourced_verbs$doc_sent_verb[i]
      }else{
        tempsources <- NULL
      }
      tempsources
  })
    
  adopted_sources_df <- data.table::rbindlist(adopted_sources, use.names=T,fill=T)
  
  #append it to the dataframe

  source_target_list <- rbind(source_target_list,adopted_sources_df,use.names = T)

  #only keep actual entities from our desired categories and
  #remove entities that are only empty strings due to the cleaning steps above
  source_target_list <- source_target_list[nchar(source_target_list$entity_name)>0 & source_target_list$entity_type%in%keep_entities,]
  
  ind <- match(c("entity_name","entity_type","source_or_target","doc_sent_verb","doc_sent_parent"),colnames(source_target_list))

  for(j in ind) set(source_target_list, j =j ,value = as.character(source_target_list[[j]]))
  
  ind <- match(c("entity_id"),colnames(source_target_list))
  
  for(j in ind) data.table::set(source_target_list, j =j ,value = as.numeric(source_target_list[[j]]))
 
  #there are duplicates here because multiple unsourced tokens may point to the same source

  source_target_list <- source_target_list[!duplicated(source_target_list),]
  source_target_list[,row_id := 1:.N]
  #create all combos of source and target by doc_sent_verb
  st_pivot <- data.table::dcast(source_target_list,doc_sent_verb+row_id~source_or_target, value.var= "entity_name")
  
  st_pivot <- data.table::as.data.table(st_pivot %>% dplyr::group_by(doc_sent_verb) %>% tidyr::expand(source, target))

  edgelist <- data.table::merge.data.table(st_pivot, verb_dt, by = c("doc_sent_verb"),all.x=F, all.y=F)
  
  if(keep_incomplete_edges==T){
    #preserves verbs with only sources or targets
    #if there is
    edgelist$edgeiscomplete <- !is.na(edgelist$source) & !is.na(edgelist$target)
    edgelist[, `:=`(hascompleteedge, any(edgeiscomplete==T)), by = c("doc_sent_verb")]
    edgelist <- edgelist %>% dplyr::filter((hascompleteedge==T & edgeiscomplete==T) | hascompleteedge==F)
    edgelist$hascompleteedge <- NULL
  }else{
    edgelist <- edgelist %>% dplyr::filter(!is.na(source) & !is.na(target))
  }
  
  hedging_helpers <- c("may","might","can","could")
  hedging_verbs <- c("seem","appear","suggest","tend","assume","indicate","estimate","doubt","believe")
  has_hedging_verb <- sapply(1:nrow(edgelist), function(z) (sum(match(hedging_verbs, 
                                        edgelist$head_verb_lemma[[z]]), na.rm=T)>0 | 
           sum(match(hedging_verbs, edgelist$xcomp_verb[[z]]), na.rm=T)>0))
 
  has_hedging_helper <- sapply(1:nrow(edgelist), function(z) (sum(match(hedging_helpers, 
                                        edgelist$helper_lemma[[z]]), na.rm=T)>0 | 
           sum(match(hedging_helpers, edgelist$xcomp_helper_lemma[[z]]), na.rm=T)>0)) #although xcomp_helpers shouldn't have any hedges
  
  future_helpers <- c("shall","will","wo","'ll")
  has_future_helper <- sapply(1:nrow(edgelist), function(z) (sum(match(future_helpers, 
                          edgelist$helper_lemma[[z]]), na.rm=T)>0 | 
                   sum(match(future_helpers, edgelist$xcomp_helper_lemma[[z]]), na.rm=T)>0))#although xcomps shouldn't have any future helpers

  be_tokens <- c("am","is","are","'m","'s","'re")
  has_future_going <- sapply(1:nrow(edgelist), function(z){(
    !is.null(edgelist$xcomp_verb[[z]]) && edgelist$head_verb_name[[z]]=="going" &&
    (sum(match(be_tokens, edgelist$helper_lemma[[z]]), na.rm=T)>0))
  })
  
  edgelist$has_hedge <- has_hedging_verb | has_hedging_helper
  edgelist$is_future <- has_future_helper | has_future_going
  
  #remove duplicates that arose from concatenating entity names
  nodelist <- nodelist[,.(entity_id, entity_name, entity_type, doc_sent_verb)]
  nodelist <- nodelist[!duplicated(nodelist),]

  nodelist <- nodelist[,.(entity_type, entity_name)]
  #create catalog of unique entity names and entity types, arranged by num mentions
  nodelist <- nodelist[,.N,by=.(entity_type,entity_name)][order(-N),]
  setnames(nodelist,'N','num_mentions')
  nodelist <- nodelist[nodelist$entity_type%in%keep_entities,]
  nodelist <- nodelist[order(entity_name,-num_mentions),]
  #include most common of the the entity types listed for each entity
  new_type <- nodelist[!duplicated(nodelist$entity_name),]
  new_type[,num_mentions:=NULL]
  
  nodelist <- nodelist[,sum(num_mentions),by=.(entity_name)]

  nodelist <- merge.data.table(new_type,nodelist,all = T)

  nodelist <- nodelist[nchar(nodelist$entity_name)>0,]
  colnames(nodelist)[3] <- "num_appearances"
  
  unique_lemmas <- data.table("head_verb_lemma" = base::unique(edgelist$head_verb_lemma))
  
  #appending verb classification
  #this currently only captures single-word verbs
  utils::data(verb_classifications)
  verblist <- data.table::merge.data.table(unique_lemmas,verb_classifications,by.x="head_verb_lemma",by.y="verb", all.x=T, all.y=F)
  
  #putting source and target first
  data.table::setcolorder(edgelist, c(2:ncol(edgelist),1))
  
  if(!is.null(file)){
    saveRDS(list('nodelist' = nodelist,'edgelist' = edgelist, 'verblist' = verblist, 'appositivelist' = apposlist),file)
    return(paste0("Nodelist, edgelist, verblist, and appositive list for doc id ",file," written to local drive"))
    
  }
    if(return_to_memory){return(list('nodelist' = nodelist,'edgelist' = edgelist,'verblist'=verblist,'appositivelist'=apposlist))}
  
  #TODO value adds:
  #TODO import the object that "which" refers to
  #TODO other non-entities we care about? eg "stakeholders"?
}



