#sample parsedtxt
parsedtxt <- spacy_parse(c("Meg Johnson and Evan McGee sit and discuss the Department of Transportation while the Department of Energy does nothing." ,
                           "Frances Potter and Beatrix Lovegood sit and discuss poetry with Jim Robinson.", 
                           "What she said makes sense", "What she said was well received", 
                           "To hike in the mountains is to experience the best of nature.", 
                           "For us to not attempt to solve the problem is for us to acknowledge defeat."),
                         pos=T, tag=T, lemma=T, entity=T, dependency=T, nounphrase=T)

#entities <- entity_extract(parsedtxt,type="all") %>% mutate(doc_sent = paste0(doc_id, "_", sentence_id))

type = "all"
x = parsedtxt
concatenator= "_"

getAnywhere(methods("entity_extract"))

function (x, type = c("named", "extended", "all"), concatenator = "_") 
{
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
  #type <- match.arg(type)
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
  
  for(doc_sent_num in 1:length(doc_sent_list)){
    source_or_target[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_num, doc_sent]))
    head_verb_id[[doc_sent_num]] <- vector(mode = "numeric", length = length(spacy_result[doc_sent==doc_sent_num, doc_sent]))
    head_verb_name[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_num, doc_sent]))
    head_verb_lemma[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_num, doc_sent]))
    head_verb_tense[[doc_sent_num]] <- vector(mode = "character", length = length(spacy_result[doc_sent==doc_sent_num, doc_sent]))
    
    for(tok_num in 1:length(spacy_result[doc_sent==doc_sent_list[doc_sent_num],doc_sent])){
      #generates array designating source or target for each word, by doc_sent
      initial_token_id <- tok_num
      current_token_id <- initial_token_id
      head_tok_id <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][tok_num,head_token_id]
      
      source_or_target[[doc_sent_num]][tok_num] <- NA
      #while loop
      #this categorizes each word as either source or target and 
      #saves it as a new column. Identifies head_verb_id and saves it as a new column
      while(is.na(source_or_target[[doc_sent_num]][tok_num])){
        
        source_or_target[[doc_sent_num]][tok_num] <- case_when(
          
          #if head_token_id trail traces back to an appositive before hitting anything else --> NA
          #if you find an appositive, stop -- it's a duplicate and should not be counted
          spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,dep_rel] %in% c("appos") ~ "remove",
          
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
        
      }#end of while
      
      if(source_or_target[[doc_sent_num]][tok_num]=="target"){
        #if source_or_target == target, set head_verb_id as first verb it hits
        head_verb_id[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token_id]
        head_verb_name[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token]
        head_verb_lemma[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,lemma]
        head_verb_tense[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,tag]

      }else if(source_or_target[[doc_sent_num]][tok_num]=="source"){
        current_token_is_verb <- F
        while(!current_token_is_verb){
          current_token_is_verb <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,pos]%in%c("VERB","AUX")
          if(!current_token_is_verb){
            current_token_id <- head_tok_id
            head_tok_id <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,head_token_id]
          }
        }
        #if source_or_target == source , set head_verb_id as first verb it hits
        head_verb_id[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token_id]
        head_verb_name[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,token]
        head_verb_lemma[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,lemma]
        head_verb_tense[[doc_sent_num]][tok_num] <- spacy_result[doc_sent==doc_sent_list[doc_sent_num],][current_token_id,tag]
        
        
      }else if(!is.na(source_or_target[[doc_sent_num]][tok_num])){
          print(paste0("Anomaly ",source_or_target[[doc_sent_num]][tok_num],
                       " found at doc_sent_num ", doc_sent_num, ", tok_num ", tok_num))
        }
    }#end of for tok_num
  }#end of for doc_sent_num
  
  spacy_result <- cbind(spacy_result, 
                        source_or_target = unlist(source_or_target), 
                        head_verb_id = unlist(head_verb_id),
                        head_verb_name = unlist(head_verb_name),
                        head_verb_lemma = unlist(head_verb_lemma),
                        head_verb_tense = unlist(head_verb_tense))
  
  #TODO someday: if verb has no object, check if (it's a verb that requires an object & there's another verb attached with an object) then
  #adopt the other verb's object
  #to distinguish "eat, drink, and be merry" from "bring and read books"
  
  #add temporary entity_ids so the other rows don't get collapsed
  temp_entity_ids <- seq(-1,-sum(is.na(spacy_result[,entity_id])),length.out = sum(is.na(spacy_result[,entity_id])))
  spacy_result[is.na(entity_id), `:=`(entity_id, temp_entity_ids)]
  
  #collapse entity rows 
  entities <- spacy_result[, lapply(.SD, function(x) ifelse(length(unique(x))>1,x,unique(x))), 
                           by = entity_id, .SDcols = c("doc_sent", 
                                                       "token_id",
                                                       #"head_token_id", 
                                                       #"tag",
                                                       # "primary_dep_rel", 
                                                       "entity_type")]
  #concatenate words in entity name
  entities[, `:=`(entity, spacy_result[, lapply(.SD, function(x) paste(x, 
                                                                       collapse = concatenator)), by = entity_id, .SDcols = c("token")]$token)]
  
  
  
  
  #transform spacy_result into edgelist, with columns source, target, verb, lemma, verb tense, and neg Y/N
  #for each verb:
  #make list of all sources. If the verb doesn't have any sources, 
  #trace to the verb ID that this verb points to and selects the sources for that verb.
  #   End loop if self-reference, revisits an index, or if ROOT
  #then subset these sources by which ones are entities and list them
  #make list of targets that are entities
  #mark the name of the verb
  #mark the lemma of the verb
  #mark the verb tense
  #mark whether the verb has a neg
  
  
  
  #TODO edit this list of named vs extended
  extended_list <- c("DATE", "TIME", "PERCENT", "MONEY", "QUANTITY", 
                     "ORDINAL", "CARDINAL")
  if (type == "extended") {
    entities <- entities[entity_type %in% extended_list]
  }
  else if (type == "named") {
    entities <- entities[!entity_type %in% extended_list]
  }
  as.data.frame(entities[, list(doc_id, sentence_id, entity, 
                                entity_type)])
}
