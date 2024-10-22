# Exported functions
# combine_networks

#' Superimposes multiple textNet igraphs into a single igraph. 
#' 
#' @param textnet_igraphs List of textnet igraphs, the outputs of export_to_network(export_format = "igraph)
#' @param mode Either "multiplex" or "weighted" depending on the desired exported network format. If "weighted", collapses multiplex edges into a single weighted edge
#' 
#' @return Single igraph object that consolidates nodes and edges from input graphs. If there are multiple nodes with the same name and different attributes originating from different graphs, this function preserves the node attributes associated with the version that appears most commonly. Adds a node attribute num_graphs_in, which denotes the number of input graphs each node was found in. For a weighted graph, the weight is equal to the original number of edges between the respective source and target nodes. Edge attributes for a multiplex graph are described in the help file of textnet_extract. 
#' @import data.table
#' 
#' @export
#' 

combine_networks <- function(textnet_igraphs, mode){
  if(!is(textnet_igraphs, "list")){
    stop("please format your graphs as elements of a list.")
  }
  num_graphs <- length(textnet_igraphs)
  if(sum(sapply(1:length(textnet_igraphs), function(i) !is(textnet_igraphs[[i]], "igraph")))>0){
    stop("combine_networks only accepts igraph objects.")
  }
  if(!mode %in% c("multiplex","weighted")){
    stop("Mode must be either multiplex or weighted.")
  }
  
  supernodes <- vector(mode = "list", length = length(num_graphs))
  superedges <- vector(mode = "list", length = length(num_graphs))
  for(m in 1:num_graphs){
    single_ig <- textnet_igraphs[[m]]
    sidf <- igraph::get.data.frame(single_ig, what = "both")
    supernodes[[m]] <- sidf$vertices
    superedges[[m]] <- sidf$edges
  }
  
  #getting rid of duplicates and keeping most common node attribute (entity_type) by number of appearances in the original text
  superedgesdt <- data.table::rbindlist(superedges)
  supernodesdt <- data.table::rbindlist(supernodes)
  supernodesdt <- supernodesdt[order(-num_appearances),]
  supernodesdt <- supernodesdt[,num_graphs_in := .N, by="name"]
  supernodesdt <- supernodesdt[!duplicated(supernodesdt, by="name"),]
  
  supernetwork <- igraph::graph_from_data_frame(superedgesdt,
                                                vertices = supernodesdt,
                                                directed = T)
  
  if(mode == "weighted"){
    weighted_graph <- supernetwork
    
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_id")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_tense")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_name")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_lemma")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "parent_verb_id")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "neg")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_verb")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_parent")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_lemma")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_token")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_verb")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_lemma")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_token")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "edgeiscomplete")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "has_hedge")
    weighted_graph <- igraph::delete_edge_attr(weighted_graph, "is_future")
    
    igraph::E(weighted_graph)$weight <- 1
    weighted_graph <- igraph::simplify(weighted_graph, edge.attr.comb=list(weight="sum"), remove.loops = F)
    
    return(weighted_graph)
    
  }else{
    return(supernetwork)
  }
  
}
