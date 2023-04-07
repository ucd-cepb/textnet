generate_phrases <- FALSE
test_data <- FALSE
draw_edges <- T
#FALSE

library(reticulate)
library(spacyr)
library(magrittr)
library(dplyr)
library(tidytext)
library(quanteda)
library(pbapply)
library(stringr)
library(network)
library(pbapply)
library(data.table)
source('R/generate_proper_names.R')
#imported file from open source package govscienceuseR
govsci_agencies <- readRDS("data/govscienceuseR_agencies.RDS")
#editing DWR cell as a test
#govsci_agencies$Abbr[grep("California Department of Water Resources",govsci_agencies$Agency)] <- "\bDWR\b"


#prerequisites: step 1, install python
#step 2, install miniconda from https://conda.io/miniconda.html
#step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
Sys.setenv(RETICULATE_PYTHON="/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python")
py_config()
#spacy_install()
#spacy_download_langmodel(model = 'en_core_web_lg')
spacy_initialize(model = "en_core_web_lg")
pr_names_grouped <- generate_proper_names(underscore = T,to_lower=F)

if(generate_phrases){
  pr_names_sep <- generate_proper_names(underscore = F,to_lower=F)
  gsp_text_with_meta <- readRDS("data/gsp_docs_w_meta")
  gsp_text_with_meta$text <- pblapply(1:length(gsp_text_with_meta$text), function(i){
    stri_replace_all_regex(gsp_text_with_meta$text[i], pattern = pr_names_sep,
                           replacement = pr_names_grouped,
                           vectorize= F)
  })
  saveRDS(gsp_text_with_meta, "data/prepped_for_sna")
}

if(test_data==TRUE){
  test_collabs <- ("Invalid_sentence. Only one organization exists. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board. The CDC will collaborate with NASA on the project. NRA's partners will include the FBI and several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence. If they partner together, the GDE plan must be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC.")
  test_deprel <- ("Invalid_sentence. Only one organization exists. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board. The CDC will collaborate with NASA on the project. NRA's partners will include the FBI and several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence. If they partner together, the GDE plan must be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC.")
  
  test_parse <- spacy_parse(test_collabs,
                            pos = T,
                            tag = T,
                            lemma = T,
                            entity = T,
                            dependency = T,
                            nounphrase = T)
  valid_test_sentences <- test_parse  %>% group_by(doc_id, sentence_id) %>% 
    filter(any(dep_rel == "nsubj" | dep_rel == "nsubjpass") & 
             any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "dobj"))
  
}


gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
gsp_planonly <- gsp_text_with_meta[is_comment==FALSE & is_reference==FALSE]
rm(gsp_text_with_meta)


for (m in 1:length(gspids)){
  
  if(draw_edges==TRUE){
      if(!file.exists(paste0("data/parsed_",gspids[m]))){
        single_plan_text <- unlist(gsp_planonly[gsp_id==gspids[m]]$text)
        parsedtxt <- spacy_parse(single_plan_text,
                                 pos = T,
                                 tag = T,
                                 lemma = T,
                                 entity = T,
                                 dependency = T,
                                 nounphrase = T)
        saveRDS(parsedtxt, paste0("data/parsed_",gspids[m]))
      }
      print(paste0("parsing complete: ",gspids[m]))
      
      parsedtxt <- readRDS(paste0("data/parsed_",gspids[m]))
      #part 1: POS tagging with spacy
      
      #part 2: identify entities
      entities <- entity_extract(parsedtxt,type="all")
      
      roots <- parsedtxt %>% filter(dep_rel == "ROOT")
      #add dependency parsing tags to entities like this:
      #break apart entities at underlines to form tok
      #find all section of rows in parsedtxt where doc id and sentence id are correct and the section of 
      #tokens is equivalent to tok
      #create new cols of entity_extract: is_obj and is_subj
      #if at least one of the rows involved is dobj, pobj, or any other obj, is_obj = true
      #same for subj
      #filter for !empty is_obj or is_subj
      
      
      
      #TODO step 2 (opt) dependency parsing (disambiguating pronouns)      
      #TODO step 3 types of verbs -- analyzing types, groupings
      #Verb Group 13: Change of Possession
      #Verb Group 15: Hold and Keep
      #Verb Group 26: Verbs of Creation and Transformation
      #Verb Group 55: Aspectual Verbs
      #Verb Group 63: Enforce Verbs
      #Verb Group 68: Pay Verbs
      #Verb Group 70: Rely Verbs
      #Verb Group 71: Conspire Verbs
      #Verb Group 72: Help Verbs
      #Verb Group 73: Cooperate Verbs
      #Verb Group 86: Correlating and Relating Verbs
      #Verb Group 93: Adopt Verbs
      #Verb Group 102: Promote Verbs
      #Verb Group 104: Spending Time Verbs
      #Verb Group 105: Use Verbs
      #Verb Group 107: Involve Verbs
      #Support? Teach? Educate?
      #TODO step 4 = Uses Verbnet database from Univ of Colorado, nature of verbs, categorization. match with verb cats from verbnet
      #verbnet <- xmlToList("data_raw/verbnet3.3/cooperate-73.1.xml")
      #verbnet2 <- read_xml("data_raw/verbnet3.3/cooperate-73.1.xml")
      #xml_find_all(verbnet2, "/pathway//entry")
      #TODO step 5 (opt) lemmatization - how do we chop the verbs? do we use lemma or not
      #TODO step 6 (opt) hedging and polarity on verbs (should be able to rely on POS tagging) (Bethard and Martin, 2006)
      
      #1 million char max
      #could call pdftotext, then run cleaning, then collapse into giant string, then tokenizer
      #deal with generic nouns when we scale up eg county that refer to different counties
      
      
      #removes invalid sentences with no verb
      #only keeps sentences that have a subject and object
      #does not keep sentences with compound subject and no object
      valid_sentences <- parsedtxt %>% group_by(doc_id, sentence_id) %>% 
        filter(any(pos == "VERB") &
                 any(dep_rel == "nsubj" | dep_rel == "nsubjpass") & 
                 any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "dobj"))
      #spacy_data %>% group_by(sentence_id, doc_id) %>% summarize(tally(pos==nsubj>0),tally(pos==nobj>0))
      
      entities_valid <- entities %>% 
        filter(doc_id %in% valid_sentences$doc_id & sentence_id %in% valid_sentences$sentence_id)
      rm(entities)
      
      #combining full and abbreviated agency names
      entities_valid$entity <- entities_valid$entity %>% str_remove_all("[^[:alnum:]]") %>% 
        str_remove_all("^the") %>% str_remove_all("^The") %>% tolower()
      
      govsci_agencies$Agency <- govsci_agencies$Agency %>% 
        str_remove_all("[^[:alnum:]]") %>% 
        str_remove_all("^the") %>% str_remove_all("^The") %>% tolower()
      
      
      abbr <- function(strng){
        
        if (!identical(grep(paste0("\b",strng,"\b"),govsci_agencies$Abbr,useBytes = F), integer(0))){
          return(govsci_agencies$Agency[grep(paste0("\b",strng,"\b"),govsci_agencies$Abbr, useBytes = F)] )
        }
        else
          return(strng)
      }
      
      entities_valid$entity <- sapply(entities_valid$entity, abbr)
      print(paste0("combining agency names complete: ",gspids[m]))
      
      #create catalog of unique entities and orgs, arranged by num mentions
      ordered_rownames <- entities_valid %>% 
        group_by(entity, entity_type) %>% summarize(num_mentions = n()) %>% arrange(desc(num_mentions))
      
      entities_valid <- full_join(ordered_rownames, entities_valid)
    
      #drop unwanted entity types and only keep ORG, GPE, PERSON
      entities_OrgGpePerson <- entities_valid[entities_valid$entity_type=="ORG" | 
                                                entities_valid$entity_type=="GPE" | 
                                                entities_valid$entity_type=="PERSON",]%>% 
            group_by(entity, entity_type) %>% summarize(num_mentions,doc_id,sentence_id) %>% arrange(desc(num_mentions))
      entities_unique <- entities_OrgGpePerson %>% group_by(entity, entity_type, num_mentions) %>% 
            group_keys %>% arrange(entity, desc(num_mentions)) 
      rm(entities_valid)
      #include most common of the three entity types listed for each entity
      new_type <- entities_unique[!duplicated(entities_unique$entity),] %>% select(!num_mentions)
      new_num_mentions <- entities_unique %>% group_by(entity) %>% summarize(new_sum = sum(num_mentions))
      entities_unique_new <- full_join(new_type, new_num_mentions) %>% filter(nchar(entity)>0)
      #passes identical test
      #identical(entities_unique[!duplicated(entities_unique$entity),]$entity,entities_unique_new$entity)
      rm(entities_unique)
      
      all_ents <- entities_unique_new$entity
      adj_mat <- matrix(0, nrow = length(all_ents), 
                        ncol = length(all_ents),
                        dimnames = list(all_ents,all_ents)
      )
      
      #adjacency matrix structure
      rownames(adj_mat) <- all_ents
      colnames(adj_mat) <- all_ents
      
      #formatting list of entities for creating combos
      entities_OrgGpePerson_no_type <- entities_OrgGpePerson %>% ungroup() %>% select(!c(entity_type, num_mentions))
      entities_grouped <- left_join(entities_unique_new, entities_OrgGpePerson_no_type, by = "entity") %>% group_by(doc_id, sentence_id)
      saveRDS(entities_grouped, paste0("data/entities_grouped_",gspids[m]))
      rm(entities_OrgGpePerson)
      rm(entities_OrgGpePerson_no_type)
      #then increment edge between entities in adjacency matrix by 1
      combos <- group_map(entities_grouped, ~ if(length(.x$entity)>1)combn(.x$entity,2))
      rm(entities_grouped) #to save memory temporarily
      
      for(s in 1:length(combos)){
        if(!is.null(combos[[s]])){
          for(i in 1:ncol(combos[[s]])){
            if(combos[[s]][1,i]!=combos[[s]][2,i]){
              adj_mat[combos[[s]][1,i],combos[[s]][2,i]] = adj_mat[combos[[s]][1,i],combos[[s]][2,i]] + 1
              adj_mat[combos[[s]][2,i],combos[[s]][1,i]] = adj_mat[combos[[s]][2,i],combos[[s]][1,i]] + 1
              
            }
          }
        }
      }
      
      print(paste0("raw adjmat complete: ",gspids[m]))
      #preserving original adj_mat
      saveRDS(adj_mat, paste0("data/adjmat_orig_",gspids[m]))
      
      #adj_mat <- readRDS(paste0("data/adjmat_orig_",gspids[m]))
      entities_grouped <- readRDS(paste0("data/entities_grouped_",gspids[m]))
      
      
  
      #removing node clusters who are only connected to other nodes in their same sentence
      #since this likely either a parsing mistake or a restating of the entity name in a different way
      
      #split each sentence into a different df
      sent_by_grp <- split(entities_grouped, list(entities_grouped$doc_id, entities_grouped$sentence_id))
      print(paste0("sentence group splitting complete: ",gspids[m]))
      #start list of entities to remove
      names_to_remove <- character(0)
      for (sent_num in 1:length(sent_by_grp)){
        num_entities <- nrow(sent_by_grp[[sent_num]])
        if(num_entities > 0){
          #initialize empty (FALSE) vector stating whether entities are exclusive
          is_exclusive <- logical(num_entities)
          #check if all entities in that sentence are exclusive to that sentence
          for(entity_num in 1:num_entities){
            #check if each entity is in multiple sentences 
            if(!(nrow(distinct(entities_grouped[entities_grouped$entity == sent_by_grp[[sent_num]]$entity[entity_num],],
                               doc_id, sentence_id,.keep_all=T))>1)){
              is_exclusive[entity_num] <- T
            }
          }#TODO - should the following if go above this brace?
          #if there are no entities in that sentence that exist outside of that sentence,
          if(sum(is_exclusive)==num_entities){
            #add the entities in that sentence to a "no list" to remove later 
            names_to_remove <- append(names_to_remove, sent_by_grp[[sent_num]]$entity)
          }
        }
        
      }
      print(paste0("identifying isolated sentences complete: ",gspids[m]))
      
      #get rid of rows and columns for all entities on the "no list"
      #TODO decide whether to use sparseMatrix as follows: adj_mat <- as(adj_mat, "sparseMatrix")
      adj_mat <- adj_mat[!(rownames(adj_mat)%in%names_to_remove),!(colnames(adj_mat)%in%names_to_remove)]
      
      print(paste0("removing isolated sentences complete: ",gspids[m]))
      
      #removing isolates
      adj_mat_subset <- adj_mat[rowSums(adj_mat)>0,]
      adj_mat_subset <- adj_mat_subset[,colSums(adj_mat_subset)>0]
      print(paste0("removing isolated nodes complete: ",gspids[m]))
      
      agency_net <- network::network(adj_mat_subset, matrix.type = "adjacency", 
                                     directed = FALSE, ignore.eval = F, names.eval = "weights")
      
      name <- network::get.vertex.attribute(agency_net, "vertex.names")
      #setting node attribute of entity type
      
      network_entities <- entities_unique_new[match(name, entities_unique_new$entity),]
      #order equivalence check
      print(paste0("identical check: ", identical(network_entities$entity, name)))
      network::set.vertex.attribute(agency_net, "type", network_entities$entity_type)
      saveRDS(agency_net, paste0("data/network_maincomponents_",gspids[m]))
        

  } 

  
  
  
  
  
  #can also use gplot instead of ggnet2, but ggnet2 is pretty cool
  #detach(package:GGally, unload = TRUE)
  #ggraph is for igraph in ggplot
  
  
  #igr <- graph_from_adjacency_matrix(adj_mat_subset, mode = "undirected", 
  #                                   weighted = TRUE, diag = FALSE)

  
  #take a couple steps to remove noise in orgs
  #dig into the probability under each org (keep orgs with > 0.75 prob)
  
  
  #goal of four-ish layers with different types of connections, then count within each category (could use verbnet here)
  #don't worry about tense
  #look at how the cues we know (like "might") is classified in spacyr parse
  
  
  
  
  #part 6: build network based on identified events
  #part 7: Use SNA to identify:
  #                 central actors
  
  
  
  
  #part 8: make plot of how these factors differ for GSPs
  #part 9: make a regression about how each of those four 
  #        factors is related to topic prevalence
  #part 10: make a regression about how each of those four
  #         factors is related to topic correlation network connectivity/centralization
}

spacy_finalize()






