library(igraph)
library(ohenery)
library(tidyverse)
gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

top_lemmas <- vector("list", length = length(gspids))
all_lemmas<- vector("list", length = length(gspids))
top_entities <- vector("list", length = length(gspids))
all_entities <- vector("list", length = length(gspids))
num_top <- 10

for(i in 1:length(gspids)){
  igr <- readRDS(paste0("data/full_directed_graph_",gspids[i]))
  net <- asNetwork(igr)
  #get top 10
  top_entities[[i]] <- head(sort(igraph::degree(igr),decreasing = T), num_top)
  all_entities[[i]] <- sort(igraph::degree(igr),decreasing = T)
  top_lemmas[[i]] <- head(sort(table(igraph::get.edge.attribute(
    igr, "head_verb_lemma")), decreasing = T),num_top)
  all_lemmas[[i]] <- sort(table(igraph::get.edge.attribute(
    igr, "head_verb_lemma")), decreasing = T)
}

top_entities_normalized <- lapply(top_entities, function(x) ohenery::normalize(x))
all_entities_normalized <- lapply(all_entities, function(x) ohenery::normalize(x))
top_lemmas_normalized <- lapply(top_lemmas, function(x) ohenery::normalize(x))
all_lemmas_normalized <- lapply(all_lemmas, function(x) ohenery::normalize(x))

top_entities_normalized <- unlist(top_entities_normalized)
all_entities_normalized <- unlist(all_entities_normalized)
top_lemmas_normalized <- unlist(top_lemmas_normalized)
all_lemmas_normalized <- unlist(all_lemmas_normalized)

top_entities_df <- tibble("names" = names(top_entities_normalized), 
                          "percent"= unname(top_entities_normalized))
all_entities_df <- tibble("names" = names(all_entities_normalized), 
                          "percent"= unname(all_entities_normalized))
top_lemmas_df <- tibble("names" =names(top_lemmas_normalized),
                        "percent"=unname(top_lemmas_normalized))
all_lemmas_df <- tibble("names" =names(all_lemmas_normalized),
                        "percent"=unname(all_lemmas_normalized))

top_entity_percents <- top_entities_df %>% group_by(names) %>% 
  summarize("total_percent" = sum(percent)) %>% arrange(desc(total_percent))
all_entity_percents <- all_entities_df %>% group_by(names) %>% 
  summarize("total_percent" = sum(percent)) %>% arrange(desc(total_percent))
top_lemma_percents <- top_lemmas_df %>% group_by(names) %>% 
  summarize("total_percent" = sum(percent)) %>% arrange(desc(total_percent))
all_lemma_percents <- all_lemmas_df %>% group_by(names) %>% 
  summarize("total_percent" = sum(percent)) %>% arrange(desc(total_percent))

#only keep entities that have letters
top_entity_percents <- top_entity_percents[grepl("[A-Za-z]", top_entity_percents$names),]
all_entity_percents <- all_entity_percents[grepl("[A-Za-z]", all_entity_percents$names),]
top_entity_percents
all_entity_percents
top_lemma_percents
all_lemma_percents 

saveRDS(all_entity_percents, "data/entity_prevalence_in_corpus_normalized")
saveRDS(all_lemma_percents, "data/lemma_prevalence_in_corpus_normalized")
