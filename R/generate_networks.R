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
    if(!file.exists(paste0("data/network_maincomponents_",gspids[m]))){
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
      
      #removes invalid sentences with no root
      #only keeps sentences that have a subject and object
      #does not keep sentences with compound subject and no object
      valid_sentences <- parsedtxt %>% group_by(doc_id, sentence_id) %>% 
        filter(any(dep_rel == "ROOT") &
                 any(dep_rel == "nsubj" | dep_rel == "nsubjpass") & 
                 any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "dobj"))
      
      
      #spacy_data %>% group_by(sentence_id, doc_id) %>% summarize(tally(pos==nsubj>0),tally(pos==nobj>0))
      
      #step 2 (opt) dependency parsing (disambiguating pronouns)
      
      #step 3 types of verbs -- analyzing types, groupings
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
      
      
      #step 4 = Uses Verbnet database from Univ of Colorado, nature of verbs, categorization. match with verb cats from verbnet
      #verbnet <- xmlToList("data_raw/verbnet3.3/cooperate-73.1.xml")
      #verbnet2 <- read_xml("data_raw/verbnet3.3/cooperate-73.1.xml")
      #xml_find_all(verbnet2, "/pathway//entry")
      
      #step 5 (opt) lemmatization - how do we chop the verbs? do we use lemma or not
      #step 6 (opt) hedging and polarity (should be able to rely on POS tagging)
      
      #step 7 cross-reference database of agency names
      
      
      
      #create catalog of unique entities and orgs, arranged by num mentions
      #include most common entity type listed for each entity
      entities_cut <- entities[entities$entity_type=="ORG" | 
                                 entities$entity_type=="GPE" | 
                                 entities$entity_type=="PERSON",
      ][,c('entity', 'entity_type')]%>% 
        group_by(entity, entity_type) %>% summarize(n()) %>% arrange(desc(`n()`))
      
      entities_subset <- entities_cut[!duplicated(entities_cut[ , "entity"]), ]
      rm(entities_cut)
      
      
      #later add:     conduct hedging detection on verbs
      #later add:     conduct polarity detection on verbs
      #part 5: use event extraction (define event) based on 
      #later add        non-hedging, positive verb relationships between entities (Bethard and Martin, 2006)
      #verb net lexicon -- dictionary of verbs
      #focus on one or two plans
      #1 million char max
      #could call pdftotext, then run cleaning, then collapse into giant string, then tokenizer
      #deal with generic nouns when we scale up eg county that refer to different counties
      
      
      all_ents <- unique(c(entities_subset$entity, pr_names_grouped))
      adj_mat <- matrix(0, nrow = length(all_ents), 
                        ncol = length(all_ents),
                        dimnames = list(all_ents,all_ents)
      )
      #if sentence contains 2+
      #"ORG" "GPE" "PERSON" or matches agency database
      #connected_entities <- valid_sentences  %>% filter(sum(
      #              entity=="ORG_I" | 
      #             entity=="ORG_B" | 
      #            entity=="GPE_I" | 
      #           entity=="GPE_B" |
      #          entity=="PERSON_I" |
      #         entity=="PERSON_B" |
      #        token %in% pr_names_grouped)>1)
      entities_mini_df <- entities[entities$entity_type=="ORG" | 
                                     entities$entity_type=="GPE" | 
                                     entities$entity_type=="PERSON"|
                                     entities$entity %in% pr_names_grouped,]
      
      entities_tiny_df <- entities_mini_df %>% 
        filter(doc_id %in% valid_sentences$doc_id & sentence_id %in% valid_sentences$sentence_id)
      
      #only_entities <- connected_entities %>% filter(entity=="ORG_I" | 
      #                                                 entity=="ORG_B" | 
      #                                                entity=="GPE_I" | 
      #                                               entity=="GPE_B" |
      #                                              entity=="PERSON_I" |
      #                                             entity=="PERSON_B" |
      #                                            token %in% pr_names_grouped) 
      
      entities_grouped <- entities_tiny_df %>% group_by(doc_id, sentence_id)
      rm(entities_tiny_df)
      #TODO unique of entities_grouped so as to not have loops
      
      #then increment edge between entities in adjacency matrix by 1
      combos <- group_map(entities_grouped, ~ if(length(.x$entity)>1)combn(.x$entity,2))
      rm(entities_grouped)
      
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
      saveRDS(entities_subset, paste0("data/entities_subset_",gspids[m]))
  
      
      adj_mat <- readRDS(paste0("data/adjmat_orig_",gspids[m]))
      entities_subset <- readRDS(paste0("data/entities_subset_",gspids[m]))
      
      
      #combining full and abbreviated agency names
       cleaned_names <- rownames(adj_mat) %>% str_remove_all("[^[:alnum:]]") %>% 
           str_remove_all("^the") %>% str_remove_all("^The")
      
      govsci_agencies$Agency <- govsci_agencies$Agency %>% 
         str_remove_all("[^[:alnum:]]")
     
      
      abbr <- function(strng){
        
         if (!identical(grep(paste0("\b",strng,"\b"),govsci_agencies$Abbr,useBytes = T), integer(0))){
           return(govsci_agencies$Agency[grep(paste0("\b",strng,"\b"),govsci_agencies$Abbr, useBytes = T)] )
         }
        else
          return(strng)
      }
       
      abbr_names <- sapply(cleaned_names, abbr)
       
      rownames(adj_mat) <- abbr_names
      colnames(adj_mat) <- abbr_names
      
      ordered_adj_mat <- adj_mat[order(rownames(adj_mat)),]
      ordered_adj_mat <- ordered_adj_mat[,order(colnames(adj_mat))]
      
      dupl_combined_adj_mat <- rowsum(ordered_adj_mat, group = rownames(ordered_adj_mat), na.rm=T)
      
      dupl_combined_adj_mat <- t(rowsum(t(dupl_combined_adj_mat), group = colnames(dupl_combined_adj_mat), na.rm = T))
      diag(dupl_combined_adj_mat) <- 0
      
      dupl_combined_adj_mat[dupl_combined_adj_mat > 0] <- 1
    
      
      print(paste0("combining agency names complete: ",gspids[m]))
  
      #removing node clusters who are only connected to other nodes in their same sentence
      #since this likely either a parsing mistake or a restating of the entity name in a different way
      
      #split each sentence into a different df
      sent_by_grp <- split(entities, list(entities$doc_id, entities$sentence_id))
      #start list of entities to remove
      names_to_remove <- character(0)
      for (sent_num in 1:length(sent_by_grp)){
        num_entities <- nrow(sent_by_grp[[sent_num]])
        #initialize empty (FALSE) vector stating whether entities are exclusive
        is_exclusive <- logical(num_entities)
        #check if all entities in that sentence are exclusive to that sentence
        for(entity_num in 1:num_entities){
          #check if each entity is in multiple sentences 
          if(!(nrow(distinct(entities[entities$entity == sent_by_grp[[sent_num]]$entity[entity_num],],
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
      #get rid of rows and columns for all entities on the "no list"
      #TODO decide whether to use sparseMatrix as follows: adj_mat <- as(adj_mat, "sparseMatrix")
      adj_mat <- adj_mat[!(rownames(adj_mat)%in%names_to_remove),!(colnames(adj_mat)%in%names_to_remove)]
      
      print(paste0("removing isolated sentences complete: ",gspids[m]))
  #     old cleaning method that only searches for adjacent dyads. doesn't work now that
  #     the data is organized by number of mentions, rather than sentence appearance order
  #     for(i in 1:nrow(adj_mat)){
  #      if(sum(adj_mat[i,])==1 & (ncol(adj_mat)>i && adj_mat[i,i+1]==1)){
  #        adj_mat[i,i+1] <- 0
  #        adj_mat[i+1,i] <- 0
  #      }
   #     if(sum(adj_mat[i,])==1 & (i > 1 && adj_mat[i,i-1]==1)){
  #        adj_mat[i,i-1] <- 0
  #        adj_mat[i-1,i] <- 0
  #      }
  #    }
      
  #    #testing to see if it worked. Should only print organizations who were in the middle of a cluster of three
  #    #for(i in 1:nrow(adj_mat)){
  #    #   if(sum(adj_mat[i,])==1 & ((ncol(adj_mat)>i && adj_mat[i,i+1]==1) | (i > 1 && adj_mat[i,i-1]==1))){
  #    #      print(i)
  #    #   }
  #    #}
      
      #removing isolates
      adj_mat_subset <- adj_mat[rowSums(adj_mat)>0,]
      adj_mat_subset <- adj_mat_subset[,colSums(adj_mat_subset)>0]
      print(paste0("removing isolated nodes complete: ",gspids[m]))
      
      agency_net <- network::network(adj_mat_subset, matrix.type = "adjacency", 
                                     directed = FALSE, ignore.eval = F, names.eval = "weights")
      
      name <- network::get.vertex.attribute(agency_net, "vertex.names")
      
      #setting node attribute of entity type
      entities_mini_df <- entities_mini_df %>% 
        group_by(entity, entity_type) %>% summarize(n()) %>% arrange(desc(`n()`))
      network_entities <- entities_mini_df[!duplicated(entities_mini_df[ , "entity"]), ]
      network_entities <- network_entities[network_entities$entity %in% name,]
      network_entities <- network_entities[match(name, network_entities$entity),]
      #order equivalence check
      print(paste0("identical check: ", identical(network_entities$entity, name)))
      network::set.vertex.attribute(agency_net, "type", network_entities$entity_type)
      saveRDS(agency_net, paste0("data/network_maincomponents_",gspids[m]))
    }    
    else{
      agency_net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
    }
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






