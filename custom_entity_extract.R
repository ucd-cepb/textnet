entities <- entity_extract(parsedtxt,type="all") %>% mutate(doc_sent = paste0(doc_id, "_", sentence_id))

type = "all"
x = parsedtxt
concatenator= "_"

getAnywhere(methods("entity_extract"))

function (x, type = c("named", "extended", "all"), concatenator = "_") 
{
  
  
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
  entity_type <- entity <- iob <- entity_id <- .SD <- `:=` <- sentence_id <- doc_id <- NULL
  type <- match.arg(type)
  if (!"entity" %in% names(spacy_result)) {
    stop("no entities in parsed object: rerun spacy_parse() with entity = TRUE")
  }
  #spacy_result <- spacy_result[nchar(spacy_result$entity) > 
  #                               0]
  spacy_result[, `:=`(doc_sent, paste0(doc_id, "_", sentence_id))]
  spacy_result[, `:=`(entity_type, sub("_.+", "", entity))]
  spacy_result[, `:=`(iob, sub(".+_", "", entity))]
  spacy_result[, `:=`(entity_id, ifelse(nchar(x$entity) > 0,cumsum(iob == "B"),NA))]
  
  
  
  #collapse entity rows 
  #TODO adjust so this doesn't get rid of other rows
  entities <- spacy_result[, lapply(.SD, function(x) x[1]), 
                           by = entity_id, .SDcols = c("doc_id", "sentence_id", "doc_sent", 
                                                       # "token_id",
                                                       #"head_token_id", 
                                                       #"tag",
                                                       # "primary_dep_rel", 
                                                       "entity_type")]
  #concatenate words in entity name
  entities[, `:=`(entity, spacy_result[, lapply(.SD, function(x) paste(x, 
                                                                       collapse = concatenator)), by = entity_id, .SDcols = c("token")]$token)]
  
  
  
  
  
  #while loop
  #this categorizes each word as either source or target and saves it as a new column. Identifies head_verb_id and saves it as a new column
  #if head_token_id trail traces back to an appositive before hitting anything else --> NA
  #If head_token_id trail traces back to a subject before hitting a verb, --> source; set head_verb_id as first verb it hits
  #If head_token_id trail traces back to a verb before hitting a subject, --> target; set head_verb_id as first verb it hits
  #If head_token_id trail leads to the beginning or to the current cursor, --> NA
  #If head_token_id trail traces back to root that is not a verb, --> NA
  
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
