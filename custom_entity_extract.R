#sample parsedtxt
#parsedtxt <- spacy_parse(c("Meg Johnson and Evan McGee sit and discuss the Department of Transportation while the Department of Energy does nothing." ,
#                           "Frances Potter and Beatrix Lovegood sit and discuss poetry with Jim Robinson.", 
#                           "What she said makes sense", "What she said was well received", 
#                           "To hike in the mountains is to experience the best of nature.", 
#                           "For us to not attempt to solve the problem is for us to acknowledge defeat."),
#                         pos=T, tag=T, lemma=T, entity=T, dependency=T, nounphrase=T)

#entities <- entity_extract(parsedtxt,type="all") %>% mutate(doc_sent = paste0(doc_id, "_", sentence_id))


#x = parsedtxt
#concatenator= "_"

#getAnywhere(methods("entity_extract"))

custom_entity_extract <- function (x, concatenator = "_",file_ext) {
  x <- x %>% mutate(doc_sent = paste0(doc_id, "_", sentence_id))
  
  #removes invalid sentences with no verb
  #only keeps sentences that have a subject and object
  #does not keep sentences with compound subject and no object
  #list of subj and obj tags from https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
  x <- x %>% group_by(doc_sent) %>% 
    filter(any(pos == "VERB") &
             any(dep_rel == "nsubj" | dep_rel == "nsubjpass" | dep_rel == "csubj" | dep_rel == "csubjpass" | dep_rel == "agent" | dep_rel == "expl") & 
             any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "attr" | dep_rel == "dobj" | dep_rel == "oprd" | 
                   dep_rel == "ccomp" | dep_rel == "xcomp" | dep_rel == "acomp" | dep_rel == "pcomp")) %>% 
    mutate(doc_sent = paste0(doc_id, "_", sentence_id))
  #spacy_data %>% group_by(sentence_id, doc_id) %>% summarize(tally(pos==nsubj>0),tally(pos==nobj>0))
  
  spacy_result <- data.table::as.data.table(x)
  #entity_type <- entity <- iob <- entity_id <- .SD <- `:=` <- sentence_id <- doc_id <- NULL
  
  if (!"entity" %in% names(spacy_result)) {
    stop("no entities in parsed object: rerun spacy_parse() with entity = TRUE")
  }
  #commenting this out to preserve non-entities
  #spacy_result <- spacy_result[nchar(spacy_result$entity) > 
  #                               0]
  #spacy_result[, `:=`(doc_sent, paste0(doc_id, "_", sentence_id))]
  spacy_result[, `:=`(entity_type, sub("_.+", "", entity))]
  spacy_result[, `:=`(iob, sub(".+_", "", entity))]
  spacy_result[, `:=`(entity_id, ifelse(nchar(x$entity) > 0,cumsum(iob == "B"),NA))]
  
  #source_or_target_by_word <- 
    
  doc_sent_list <- unique(spacy_result[,doc_sent]) 

  source_or_target <- vector(mode = "list", length = length(doc_sent_list))
  head_verb_id <- vector(mode = "list", length = length(doc_sent_list))
  head_verb_name <- vector(mode = "list", length = length(doc_sent_list))
  head_verb_lemma <- vector(mode = "list", length = length(doc_sent_list))
  head_verb_tense <- vector(mode = "list", length = length(doc_sent_list))
  parent_verb_id <- vector(mode = "list", length = length(doc_sent_list))
  
  for(doc_sent_num in 1:length(doc_sent_list)){
    print(paste0("beginning doc_sent ",doc_sent_list[doc_sent_num]))
    source_or_target[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    head_verb_id[[doc_sent_num]] <- vector(mode = "numeric", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    head_verb_name[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    head_verb_lemma[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    head_verb_tense[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    parent_verb_id[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_list[doc_sent_num], doc_sent]))
    
    for(tok_num in 1:length(spacy_result[doc_sent==doc_sent_list[doc_sent_num],doc_sent])){
      #generates array designating source or target for each word, by doc_sent
      initial_token_id <- tok_num
      current_token_id <- initial_token_id
      head_tok_id <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][tok_num,head_token_id]
      
      source_or_target[[doc_sent_num]][tok_num] <- NA
      break_while_counter <- 0
      #while loop
      #this categorizes each word as either source or target and 
      #saves it as a new column. Identifies head_verb_id and saves it as a new column
      while(is.na(source_or_target[[doc_sent_num]][tok_num]) & break_while_counter < 15){
        
        source_or_target[[doc_sent_num]][tok_num] <- case_when(
          
          #if head_token_id trail traces back to an appositive before hitting anything else --> NA
          #if you find an appositive, stop -- it's a duplicate and should not be counted
          spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,dep_rel] %in% c("appos") ~ "appos",
          
          #If head_token_id trail traces back to a verb before hitting a subject, --> target
          spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,pos] %in% c("VERB","AUX") ~ "target",
          
          #if you find a subject, stop
          #If head_token_id trail traces back to a subject before hitting a verb, --> source
          spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,dep_rel] %in% c("nsubj","nsubjpass","csubj","csubjpass","agent","expl") ~ "source",
          
          #If head_token_id trail traces back to root that is not a verb, --> "root_not_verb"
          spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,dep_rel] == "ROOT" ~ "root_not_verb",
          
          #If head_token_id trail leads to the current cursor, --> "broken_dep_rel"
          head_tok_id == current_token_id ~ "broken_dep_rel",
          
          #If head_token_id trail leads to the beginning, --> "inf_loop"
          head_tok_id == initial_token_id ~ "inf_loop",
          
          TRUE ~ as.character(NA)
        )
        if(is.na(source_or_target[[doc_sent_num]][tok_num])){
          current_token_id <- head_tok_id
          head_tok_id <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,head_token_id]
        }
        break_while_counter <- break_while_counter + 1
      }#end of while
      
      if(!is.na(source_or_target[[doc_sent_num]][tok_num]) && source_or_target[[doc_sent_num]][tok_num]=="target"){
        #if source_or_target == target, set head_verb_id as first verb it hits
        head_verb_id[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token_id]
        head_verb_name[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token]
        head_verb_lemma[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,lemma]
        head_verb_tense[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,tag]
        parent_verb_id[[doc_sent_num]][tok_num] <- head_tok_id

      }else if(!is.na(source_or_target[[doc_sent_num]][tok_num]) && source_or_target[[doc_sent_num]][tok_num]=="source"){
        #TODO what to call "sources" that point to roots that aren't verbs?
        current_token_is_verb <- F
        source_while_counter <- 0
        while(!current_token_is_verb & source_while_counter < 10){
          current_token_is_verb <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,pos]%in%c("VERB","AUX")
          if(!current_token_is_verb){
            current_token_id <- head_tok_id
            head_tok_id <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,head_token_id]
          }
          source_while_counter <- source_while_counter + 1
        }
        #if source_or_target == source , set head_verb_id as first verb it hits
        if(current_token_is_verb){
          head_verb_id[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token_id]
          head_verb_name[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token]
          head_verb_lemma[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,lemma]
          head_verb_tense[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,tag]
          parent_verb_id[[doc_sent_num]][tok_num] <- head_tok_id
        }else{
          head_verb_id[[doc_sent_num]][tok_num] <- NA
          head_verb_name[[doc_sent_num]][tok_num] <- NA
          head_verb_lemma[[doc_sent_num]][tok_num] <- NA
          head_verb_tense[[doc_sent_num]][tok_num] <- NA
          parent_verb_id[[doc_sent_num]][tok_num] <- NA
        }
      }
      #removed for speed
      #else if(!is.na(source_or_target[[doc_sent_num]][tok_num])){
       #   print(paste0("Anomaly ",source_or_target[[doc_sent_num]][tok_num],
       #                " found at doc_sent ", doc_sent_list[doc_sent_num], ", tok_num ", tok_num))
      #}
    }#end of for tok_num
  }#end of for doc_sent_num
  
  spacy_result <- cbind(spacy_result, 
                        source_or_target = unlist(source_or_target), 
                        head_verb_id = unlist(head_verb_id),
                        head_verb_name = unlist(head_verb_name),
                        head_verb_lemma = unlist(head_verb_lemma),
                        head_verb_tense = unlist(head_verb_tense),
                        parent_verb_id = unlist(parent_verb_id))
  
  rm(source_or_target)
  rm(head_verb_id)
  rm(head_verb_name)
  rm(head_verb_lemma)
  rm(head_verb_tense)
  rm(parent_verb_id)
  #TODO someday: if verb has no object, check if (it's a verb that requires an object & there's another verb attached with an object) then
  #adopt the other verb's object
  #to distinguish "eat, drink, and be merry" from "bring and read books"
  
  #add temporary entity_ids so the other rows don't get collapsed
  temp_entity_ids <- seq(-sum(is.na(spacy_result[,entity_id])),-1,length.out = sum(is.na(spacy_result[,entity_id])))
  spacy_result[is.na(entity_id), `:=`(entity_id, temp_entity_ids)]
  
  #collapse entity rows 
  entities_collapsed <- spacy_result[,lapply(spacy_result, function(x) tapply(x, spacy_result$entity_id, function(j){as.vector(j)}))]
  entity_reorder <- match(sort(unique(spacy_result$entity_id)),unique(spacy_result$entity_id))
  entities_collapsed <- cbind(entities_collapsed, entity_id = sort(unique(spacy_result$entity_id)))
  
  #concatenate words in entity name
  #entity_name <- spacy_result[,lapply(.SD,function(x) paste(x, collapse=concatenator)),by=entity_id, .SDcols=c("token")]$token[entity_reorder]
  entities_collapsed[, `:=`(entity_name, spacy_result[, lapply(.SD, function(x) paste(x, 
                                                                       collapse = concatenator)), by = entity_id, .SDcols = c("token")]$token[entity_reorder])]
  
  entities_collapsed$entity_name <- entities_collapsed$entity_name %>% str_remove_all("[^[:alnum:]]") %>% 
    str_remove_all("^the_") %>% str_remove_all("^The_") %>% str_remove_all("^California_") %>% str_remove_all("^US_") %>% tolower()
  
  entities_collapsed <- setDT(lapply(entities_collapsed, function(y) lapply(y, function(x) if(length(unique(x))==1) as.vector(unique(x)) else as.vector(x))))
  
  nodelist <- entities_collapsed[nchar(entities_collapsed$entity_type)>0]
  #make unique head_verb_id and head_token_id identifier for each doc_sent
  entities_collapsed[, `:=`(doc_sent_verb, paste0(doc_sent, "_", head_verb_id))]
  entities_collapsed[, `:=`(doc_sent_parent, paste0(doc_sent, "_", parent_verb_id))]
  entities_collapsed[, `:=`(doc_sent_head_tok, paste0(doc_sent, "_", head_token_id))]
  
  #tag each head_tok that has a negation child
        entities_collapsed[, `:=`(neg, any(dep_rel=="neg")), by = doc_sent_verb]
        #entities_collapsed[, `:=`(neg, any(dep_rel=="neg")), by = doc_sent_head_tok]
  #TODO fix the negation algorithm
  #is the head_tok a verb?
  #setDT(entities_collapsed)[,if(any(dep_rel=="neg")) .SD, by = doc_sent_verb]
  #as.numeric(str_extract(entities_collapsed$doc_sent_head_tok, "(\\d+)$"))
  #entities_collapsed[unlist(entities_collapsed$doc_sent)
  #entities_collapsed$pos[)]
  
  #str_extract(entities_collapsed$doc_sent_head_tok, "(\\d+)$")
  
  #entities are the only ones with multiple values per element, and they are not going to be neg, so it's ok to use only the first value
  #View(lapply(entities_collapsed[,head_token_id], function(x) entities_collapsed[x]))
  #sapply(entities_collapsed[,head_token_id],"[[",1)
  #unlist(entities_collapsed[,doc_sent])
  
  #entities_collapsed[sapply(entities_collapsed[,head_token_id],"[[",1), pos] %in% c("VERB", "AUX")
  #transform spacy_result into edgelist, with columns source, target, verb, lemma, verb tense, and neg Y/N
  
  #start with verb properties: head_verb_id, head_verb_name, head_verb_lemma, head_verb_tense, neg, and parent_verb_id
  verb_dt <- entities_collapsed[,list(doc_sent_verb, head_verb_id, head_verb_name, head_verb_lemma, head_verb_tense, parent_verb_id, doc_sent_parent, neg)]
  
  verb_dt <- distinct(verb_dt)
  
  #remove verbs with neg
  verb_dt <- verb_dt[neg==F]
  
  #ind <- match(c("head_verb_id"),colnames(verb_dt))
  #set(verb_dt, j = ind ,value = as.numeric(verb_dt[[ind]]))
  
  
  #setDT(entities_collapsed)[,if(any(dep_rel=="neg")) .SD, by = doc_sent_verb]
  
  #verbs_that_have_neg <- entities_collapsed[,if(any(dep_rel=="neg")) .SD, by=doc_sent_verb]
 
  
  
  #does the verb have any sources?
  entities_collapsed[, `:=`(has_sources, any(source_or_target=="source")), by = doc_sent_verb]
  
  source_target_list <- entities_collapsed[,.(entity_name, entity_id, entity_type, source_or_target, doc_sent_verb, doc_sent_parent, has_sources)]
  
  #If the verb doesn't have any sources, 
  #this traces to the verb ID that this verb points to and selects the sources for that verb.
  #TODO create while loop that searches all parent verbs in the sentence for sources, not just the immediate one 
  verbs_without_sources <- sapply(which(entities_collapsed$has_sources==F),function(x)source_target_list[x]$doc_sent_verb)
  parents <- sapply(which(entities_collapsed$has_sources==F),function(x)source_target_list[x]$doc_sent_parent)
  sources_have_been_adopted <- vector(length = length(verbs_without_sources))
  for(p in 1:length(parents)){
    if(!sources_have_been_adopted[p]){
      adopted_dt <- source_target_list[doc_sent_verb==parents[p] & source_or_target == "source"]
      adopted_dt[, `:=`(doc_sent_verb, verbs_without_sources[p])]
      source_target_list <- rbindlist(list(source_target_list, adopted_dt))
    }
    
    sources_have_been_adopted[which(verbs_without_sources == verbs_without_sources[p])] <- T
    
  }
  
  
  #only keep actual entities
  source_target_list <- source_target_list[entity_id>0]
  
  ind <- match(c("entity_name","entity_type","source_or_target","doc_sent_verb","doc_sent_parent"),colnames(source_target_list))

  for(j in ind) set(source_target_list, j =j ,value = as.character(source_target_list[[j]]))
  
  ind <- match(c("entity_id"),colnames(source_target_list))
  
  for(j in ind) set(source_target_list, j =j ,value = as.numeric(source_target_list[[j]]))
  
  source_target_list <- tibble(source_target_list)
  
  
  
  #create all combos of source and target by doc_sent_verb
  st_pivot <- pivot_wider(source_target_list, names_from = source_or_target, values_from = entity_name)
  st_pivot <- st_pivot %>% group_by(doc_sent_verb) %>% expand(source, target)

  edgelist <- inner_join(st_pivot, verb_dt, by = c("doc_sent_verb"))
  
  edgelist <- edgelist %>% filter(!is.na(source) & !is.na(target))
  
  nodelist <- nodelist[,.(entity_type, entity_name)]
  #create catalog of unique entity names and entity types, arranged by num mentions
  nodelist <- as_tibble(nodelist) %>% group_by(entity_type, entity_name) %>% 
    summarize(num_mentions = n()) %>% arrange(desc(num_mentions))
    

  nodelist_OrgGpePerson <- nodelist[nodelist$entity_type=="ORG" | 
                                      nodelist$entity_type=="GPE" | 
                                      nodelist$entity_type=="PERSON",]%>% 
                                  group_by(entity_name, entity_type) %>% 
                                  summarize(num_mentions) %>% arrange(desc(num_mentions))
  nodelist <- nodelist_OrgGpePerson %>% group_by(entity_name, entity_type, num_mentions) %>% 
    group_keys %>% arrange(entity_name, desc(num_mentions)) 
  
  #include most common of the three entity types listed for each entity
  new_type <- nodelist[!duplicated(nodelist$entity_name),] %>% select(!num_mentions)
  nodelist <- nodelist %>% group_by(entity_name) %>% summarize(new_sum = sum(num_mentions))
  nodelist <- full_join(new_type, nodelist) %>% filter(nchar(entity_name)>0)
  
  saveRDS(nodelist, paste0("data/nodelist/",file_ext))
  saveRDS(edgelist, paste0("data/edgelist/",file_ext))
  
  return(paste0("Nodelist and edgelist for doc id ",file_ext," written to local drive"))
}




























#TODO value adds:
#TODO don't import subjects unless there's a cc; otherwise, import the object??
#TODO import the object that "which" refers to
#TODO other non-entities we care about? eg "stakeholders"?


#type = "all"
#type <- match.arg(type)
#TODO edit this list of named vs extended
#extended_list <- c("DATE", "TIME", "PERCENT", "MONEY", "QUANTITY", 
#                   "ORDINAL", "CARDINAL")
#if (type == "extended") {
#  entities <- entities[entity_type %in% extended_list]
#}
#else if (type == "named") {
#  entities <- entities[!entity_type %in% extended_list]
#}
#as.data.frame(entities[, list(doc_id, sentence_id, entity, 
#                              entity_type)])
