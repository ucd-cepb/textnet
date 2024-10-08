# Exported functions
# top_features

#' Finds the most common entities and lemmas in the corpus
#' 
#' @param files vector of filepaths to igraph objects or list of igraph objects
#' @param from_file boolean whether files represent filepaths (T) or igraph objects (F)
#' 
#' @return list of all entities and lemmas in the corpus, along with their average normalized prevalence as a fraction of a plan
#' @importFrom magrittr %>%
#' 
#' @export

top_features <- function(files, from_file=F){
  all_lemmas<- vector("list", length = length(files))
  all_entities <- vector("list", length = length(files))
  
  for(i in 1:length(files)){
    
    if(from_file==T){
      igr <- readRDS(files[i])
    }else{
      igr <- files[[i]]
    }
    igr_df <- igraph::get.data.frame(igr, what = "both")
    
    net <- network::network(x=igr_df$edges[,1:2], directed = T,
                          hyper = F, loops = T, multiple = T, 
                          bipartiate = F, vertices = igr_df$vertices,
                          matrix.type = "edgelist")
    
    
    all_entities[[i]] <- sort(igraph::degree(igr),decreasing = T)
    all_lemmas[[i]] <- sort(table(igraph::get.edge.attribute(
      igr, "head_verb_lemma")), decreasing = T)
  }
  
  all_entities_normalized <- lapply(all_entities, function(x) ohenery::normalize(x))
  all_lemmas_normalized <- lapply(all_lemmas, function(x) ohenery::normalize(x))
  
  all_entities_normalized <- unlist(all_entities_normalized)
  all_lemmas_normalized <- unlist(all_lemmas_normalized)
  
  all_entities_df <- tidyr::tibble("names" = names(all_entities_normalized), 
                            "fraction_of_doc"= unname(all_entities_normalized))
  all_lemmas_df <- tidyr::tibble("names" =names(all_lemmas_normalized),
                          "fraction_of_doc"=unname(all_lemmas_normalized))
  #prevalence over entire corpus as avg fraction of a plan
  all_entity_percents <- all_entities_df %>% dplyr::group_by(names) %>% 
    dplyr::summarize("avg_fract_of_a_doc" = sum(fraction_of_doc)/length(files)) %>% dplyr::arrange(dplyr::desc(avg_fract_of_a_doc))
  all_lemma_percents <- all_lemmas_df %>% dplyr::group_by(names) %>% 
    dplyr::summarize("avg_fract_of_a_doc" = sum(fraction_of_doc)/length(files)) %>% dplyr::arrange(dplyr::desc(avg_fract_of_a_doc))
  
  #only keep entities that have letters
  all_entity_percents <- all_entity_percents[grepl("[A-Za-z]", all_entity_percents$names),]
  
  return(list(entities = all_entity_percents, lemmas = all_lemma_percents))
  
}

