# Exported functions 
# crawl_sentence 

#' Take a sentence and follow the dependencies
#'
#' @param s a data.frame containing the results of one (1) parsed spacy sentence
#' @return data frame with original parsed sentence + added dependency parsing
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when
#' @export
#' 

crawl_sentence <- function(s){ 
sentence <- s
empty_list <- create_empty_sentence_parse_list(nrow(sentence))
#tag auxes acting as auxes (eg "will" in "will attempt") for removal
empty_list$helper_lemma <- ifelse(sentence$pos=="AUX" & sentence$dep_rel %in% c("aux","auxpass"),"aux",empty_list$helper_lemma)
empty_list$helper_token <- ifelse(sentence$pos=="AUX" & sentence$dep_rel %in% c("aux","auxpass"),"aux",empty_list$helper_token)
empty_list$xcomp_verb <- ifelse(sentence$pos=="VERB" & sentence$dep_rel =="xcomp","xcomp",empty_list$helper_token)

  for(tok_num in 1:nrow(sentence)) {
    if(empty_list$helper_lemma[tok_num]=="aux"){
      #this isn't the main verb, so it should be appended to the main verb as a helper column
      empty_list$helper_lemma[[sentence$head_token_id[tok_num]]] <- 
        append(empty_list$helper_lemma[[sentence$head_token_id[tok_num]]], sentence$lemma[tok_num])
      empty_list$helper_token[[sentence$head_token_id[tok_num]]] <- 
        append(empty_list$helper_token[[sentence$head_token_id[tok_num]]], sentence$token[tok_num])
    }
    #this isn't the main verb, so it should be appended to the main verb as an xcomp_verb column
    if(empty_list$xcomp_verb[tok_num]=="xcomp"){
      empty_list$xcomp_verb[sentence$head_token_id[tok_num]] <- 
        append(empty_list$xcomp_verb[[sentence$head_token_id[tok_num]]], sentence$lemma[tok_num])
      
      
    }
    #print(tok_num)
    initial_token_id <- tok_num
    current_token_id <- initial_token_id
    sentence$head_token_id
    head_tok_id <- sentence[tok_num,head_token_id]
    empty_list$source_or_target[tok_num] <- NA
    break_while_counter <- 0
    #while loop
    #this categorizes each word as either source or target and 
    #saves it as a new column. Identifies head_verb_id and saves it as a new column
    while(is.na(empty_list$source_or_target[tok_num]) & break_while_counter < 15){
      empty_list$source_or_target[tok_num] <- case_when(
        #if head_token_id trail traces back to an appositive before hitting anything else --> NA
        #if you find an appositive, stop -- it's a duplicate and should not be counted
        sentence[current_token_id,dep_rel] %in% c("appos") ~ "appos",
        
        #If head_token_id trail traces back to a verb before hitting a subject, --> target
        sentence[current_token_id,pos] %in% c("VERB","AUX") ~ "target",
        
        #if you find a subject, stop
        #If head_token_id trail traces back to a subject before hitting a verb, --> source
        sentence[current_token_id,dep_rel] %in% c("nsubj","nsubjpass","csubj","csubjpass","agent","expl") ~ "source",
        
        #If head_token_id trail traces back to root that is not a verb, --> "root_not_verb"
        sentence[current_token_id,dep_rel] == "ROOT" ~ "root_not_verb",
        
        #If head_token_id trail leads to the current cursor, --> "broken_dep_rel"
        head_tok_id == current_token_id ~ "broken_dep_rel",
        
        #If head_token_id trail leads to the beginning, --> "inf_loop"
        head_tok_id == initial_token_id ~ "inf_loop",
        
        TRUE ~ as.character(NA)
      )
      if(is.na( empty_list$source_or_target[tok_num])){
        current_token_id <- head_tok_id
        head_tok_id <- sentence[current_token_id,head_token_id]
      }
      break_while_counter <- break_while_counter + 1
    }#end of while
    
    
    if(!is.na(empty_list$source_or_target[tok_num]) && empty_list$source_or_target[tok_num]=="target"){
      #if source_or_target == target, set head_verb_id as first verb it hits
      empty_list$head_verb_id[tok_num] <- sentence[current_token_id,token_id]
      empty_list$head_verb_name[tok_num] <- sentence[current_token_id,token]
      empty_list$head_verb_lemma[tok_num] <- sentence[current_token_id,lemma]
      empty_list$head_verb_tense[tok_num] <- sentence[current_token_id,tag]
      empty_list$parent_verb_id[tok_num] <- head_tok_id
      
    }else if(!is.na(empty_list$source_or_target[tok_num]) && empty_list$source_or_target[tok_num]=="source"){
      #TODO what to call "sources" that point to roots that aren't verbs?
      current_token_is_verb <- F
      source_while_counter <- 0
      while(!current_token_is_verb & source_while_counter < 10){
        current_token_is_verb <- sentence[current_token_id,pos]%in%c("VERB","AUX")
        if(!current_token_is_verb){
          current_token_id <- head_tok_id
          head_tok_id <- sentence[current_token_id,head_token_id]
        }
        source_while_counter <- source_while_counter + 1
      }
      #if source_or_target == source , set head_verb_id as first verb it hits
      if(current_token_is_verb){
        empty_list$head_verb_id[tok_num] <- sentence[current_token_id,token_id]
        empty_list$head_verb_name[tok_num] <-  sentence[current_token_id,token]
        empty_list$head_verb_lemma[tok_num] <-  sentence[current_token_id,lemma]
        empty_list$head_verb_tense[tok_num] <-  sentence[current_token_id,tag]
        empty_list$parent_verb_id[tok_num] <- head_tok_id
      }else{
        empty_list$head_verb_id[tok_num] <- NA
        empty_list$head_verb_name[tok_num] <- NA
        empty_list$head_verb_lemma[tok_num] <- NA
        empty_list$head_verb_tense[tok_num] <- NA
        empty_list$parent_verb_id[tok_num] <- NA
      }
    }
    #removed for speed
    #else if(!is.na(source_or_target[[doc_sent_num]][tok_num])){
    #   print(paste0("Anomaly ",source_or_target[[doc_sent_num]][tok_num],
    #                " found at doc_sent ", doc_sent_list[doc_sent_num], ", tok_num ", tok_num))
    #}
    #end of for tok_num
  }


#for each of the xcomp tokens, move their helper_lemmas and helper_tokens into their head verb's xcomp_helper_lemma and comp_helper_token rows

comp_rows <- which(empty_list$xcomp_verb=="xcomp")
headverb_rows <- sentence$head_token_id[comp_rows]

empty_list$xcomp_helper_lemma[headverb_rows] <- lapply(seq_along(headverb_rows), function (j){
  append(empty_list$xcomp_helper_lemma[[headverb_rows[j]]][
    !is.null(empty_list$xcomp_helper_lemma[[headverb_rows[j]]])], 
    empty_list$helper_lemma[[comp_rows[j]]][
      !is.null(empty_list$helper_lemma[[comp_rows[j]]])
    ])
})
empty_list$xcomp_helper_token[headverb_rows] <- lapply(seq_along(headverb_rows), function (j){
  append(empty_list$xcomp_helper_token[[headverb_rows[j]]][
    !is.null(empty_list$xcomp_helper_token[[headverb_rows[j]]])], 
    empty_list$helper_token[[comp_rows[j]]][
      !is.null(empty_list$helper_token[[comp_rows[j]]])
    ])
})


return(empty_list)}
