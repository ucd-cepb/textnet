# Exported functions 
# custom_entity_extract2 

#' Takes a parsed spacy document and performs dependency parsing
#'
#' @param x parsed spacy document
#' @param concatenator how entity parts are concatenated (defaults to "_")
#' @param file location where an list object with an edgelist and nodelist should be saved as .RDS file
#' @param cl number of cores to crawl sentences in parallel (defaults to 1)
#' @param keep_entities character vector of spacy entity types to retain, defaults to people (PERSON), organizations (ORG), and geographic entities (GPE)
#' @param return_to_memory boolean for whether function should return final result as workspace object
#' @return data frame with original parsed sentence + added dependency parsing
#' 
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove_all
#' @importFrom dplyr inner_join filter group_by arrange summarize select full_join left_join
#' @importFrom tidyr pivot_wider expand
#' @importFrom pbapply pblapply pbsapply
#' @export
#'

custom_entity_extract2 <- function (x, concatenator = "_",file = NULL,cl = 1,
                                    keep_entities = c('ORG','GPE','PERSON'),
                                    return_to_memory = T) {

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
parse_list <- pblapply(sentence_splits,function(y) as.data.table(crawl_sentence(y)),cl = cl)
x <- rbindlist(mapply(function(x,y) cbind(x,y),x = sentence_splits,y = parse_list,SIMPLIFY = F))

#remove aux helpers functioning as aux; xcomp verbs; and appositives
#TODO if appos are to be used to generate dictionary, that would have to happen before this step, and they would have to be cleaned
x <- x[!((pos=="AUX" & dep_rel %in% c("aux","auxpass")) |(source_or_target=="appos") | (pos=="VERB" & dep_rel =="xcomp")),]

#"remove" is null because we don't want to remove anything token-wise, but only concatenated-entity-wise
x <- entity_consolidate_replicate(x,concatenator, remove=NULL)

remove <- c("^_", "_$", "^The_","^the_","^THE_","\\s")
index <- which(grepl(paste(remove,collapse = '|'),x$entity_cat,perl = T))
x$entity_cat[index] <- str_remove_all(x$entity_cat[index],paste(remove,collapse = '|'))
#remove consecutive underscores that may have arisen due to previous cleaning step
x$entity_cat[index] <- gsub('(_)\\1+', '\\1', x$entity_cat[index])

#TODO inspect the alnum behavior. option: remove anything that doesn't have alnum and also remove anything that's only a number

  #make unique head_verb_id and head_token_id identifier for each doc_sent
  x[, `:=`(doc_sent_verb, paste0(doc_sent, "_", head_verb_id))]
  x[, `:=`(doc_sent_parent, paste0(doc_sent, "_", parent_verb_id))]
  x[, `:=`(doc_sent_head_tok, paste0(doc_sent, "_", head_token_id))]
  
  nodelist <- x[nchar(x$entity_type)>0,]
  
  #tag each head_tok that has a negation child
        x[, `:=`(neg, any(dep_rel=="neg")), by = doc_sent_verb]
        #entities_collapsed[, `:=`(neg, any(dep_rel=="neg")), by = doc_sent_head_tok]

  #data table of verb properties
  verb_dt <- x[,list(doc_sent_verb, head_verb_id, head_verb_name, head_verb_lemma, head_verb_tense, parent_verb_id, helper_lemma, helper_token, xcomp_verb, xcomp_helper_lemma, xcomp_helper_token, doc_sent_parent, neg)]
  
  verb_dt <- verb_dt[!duplicated(verb_dt),]
  
  #remove verbs with neg
  verb_dt <- verb_dt[neg==F]
  
  #does the verb have any sources? do this before removing non-entities
  x[, `:=`(has_sources, any(source_or_target=="source")), by = doc_sent_verb]

  ### there are duplicates here because there can be multiple tokens (each of which has its own row) associated with one verb ###

  source_target_list <- x[,.(entity_cat, entity_id, entity_type, source_or_target, doc_sent_verb, doc_sent_parent, has_sources)]
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
  
  adopted_sources_df <- rbindlist(adopted_sources, use.names=T,fill=T)
  
  #append it to the dataframe

  source_target_list <- rbind(source_target_list,adopted_sources_df,use.names = T)

  #only keep actual entities
  source_target_list <- source_target_list[entity_id>0]
  
  ind <- match(c("entity_cat","entity_type","source_or_target","doc_sent_verb","doc_sent_parent"),colnames(source_target_list))

  for(j in ind) set(source_target_list, j =j ,value = as.character(source_target_list[[j]]))
  
  ind <- match(c("entity_id"),colnames(source_target_list))
  
  for(j in ind) set(source_target_list, j =j ,value = as.numeric(source_target_list[[j]]))
 
  #there are duplicates here because multiple unsourced tokens may point to the same source

  source_target_list <- source_target_list[!duplicated(source_target_list),]
  
  #create all combos of source and target by doc_sent_verb
  st_pivot <- pivot_wider(source_target_list, names_from = source_or_target, values_from = entity_cat)
  
  st_pivot <- st_pivot %>% group_by(doc_sent_verb) %>% expand(source, target)

  edgelist <- inner_join(st_pivot, verb_dt, by = c("doc_sent_verb"))
  
  #TODO if we want to preserve verbs with only sources or targets this would happen here
  edgelist <- edgelist %>% filter(!is.na(source) & !is.na(target))
  
  #remove duplicates that arose from concatenating entity names
  nodelist <- nodelist[,.(entity_id, entity_cat, entity_type, doc_sent_verb)]
  nodelist <- nodelist[!duplicated(nodelist),]

  nodelist <- nodelist[,.(entity_type, entity_cat)]
  #create catalog of unique entity names and entity types, arranged by num mentions
  nodelist <- nodelist[,.N,by=.(entity_type,entity_cat)][order(-N),]
  setnames(nodelist,'N','num_mentions')
  #as_tibble(nodelist) %>% group_by(entity_type, entity_name) %>% 
  #summarize(num_mentions = n()) %>% arrange(desc(num_mentions))
  nodelist <- nodelist[nodelist$entity_type%in%keep_entities,]
  nodelist <- nodelist[order(entity_cat,-num_mentions),]
  #include most common of the three entity types listed for each entity
  new_type <- nodelist[!duplicated(nodelist$entity_cat),]
  new_type[,num_mentions:=NULL]
  
  nodelist <- nodelist[,sum(num_mentions),by=.(entity_cat)]

  nodelist <- merge.data.table(new_type,nodelist,all = T)

  nodelist <- nodelist[nchar(nodelist$entity_cat)>0,]
  
  unique_lemmas <- data.table("head_verb_lemma" = unique(edgelist$head_verb_lemma))
  
  #appending verb classification
  #this currently only captures single-word verbs
  verblist <- merge.data.table(unique_lemmas,verb_pivot,by.x="head_verb_lemma",by.y="verb", all.x=T, all.y=F)
  
  if(!is.null(file)){
    saveRDS(list('nodelist' = nodelist,'edgelist' = edgelist, 'verblist' = verblist),file)
    return(paste0("Nodelist, edgelist, and verblist for doc id ",file," written to local drive"))
    
  }
    if(return_to_memory){return(list('nodelist' = nodelist,'edgelist' = edgelist))}
}


#TODO value adds:
#TODO import the object that "which" refers to
#TODO other non-entities we care about? eg "stakeholders"?

