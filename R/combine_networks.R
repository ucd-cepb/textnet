# Exported functions
# combine_networks

#' Superimposes multiple textNet igraphs into a single igraph. 
#' 
#' @param textnet_igraphs List of textnet igraphs, the outputs of export_to_network(export_format = "igraph)
#' @param mode Either "multiplex" or "weighted" depending on the desired exported network format. If "weighted", collapses multiplex edges into a single weighted edge
#' 
#' @return Single igraph object that consolidates nodes and edges from input graphs. If there are multiple nodes with the same name and different attributes originating from different graphs, this function preserves the node attributes associated with the version that appears most commonly. Adds a node attribute num_graphs_in, which denotes the number of input graphs each node was found in. For a weighted graph, the weight is equal to the original number of edges between the respective source and target nodes. Edge attributes for a multiplex graph are described in the help file of textnet_extract. 
#' @importFrom data.table rbindlist
#' @importFrom igraph as_data_frame
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph edge_attr_names
#' @importFrom igraph delete_edge_attr
#' @importFrom igraph E
#' @importFrom igraph simplify
#' @importFrom methods is
#' @export
#' 

combine_networks <- function(textnet_igraphs, mode = c('multiplex','weighted')){
  # Check if textnet_igraphs is missing
  if(missing(textnet_igraphs)) {
    stop("Argument 'textnet_igraphs' is missing. Must provide a list of igraph objects.")
  }
  
  # Check if textnet_igraphs is a list
  if(!is.list(textnet_igraphs)){
    stop("Argument 'textnet_igraphs' must be a list of igraph objects.")
  }
  
  # Check if list is empty
  if(length(textnet_igraphs) == 0) {
    stop("Argument 'textnet_igraphs' cannot be an empty list.")
  }
  
  # Check if all elements are igraph objects
  if(sum(sapply(textnet_igraphs, function(x) !is(x, "igraph"))) > 0){
    stop("All elements in 'textnet_igraphs' must be igraph objects.")
  }

  # Check if mode is missing
  if(missing(mode)) {
    stop("Argument 'mode' is missing. Must be either 'multiplex' or 'weighted'.")
  }
  
  # Check if mode is character
  if(!is.character(mode)) {
    stop("Argument 'mode' must be a character string ('multiplex' or 'weighted').")
  }
  
  # Check if mode is length 1
  if(length(mode) != 1) {
    stop("Argument 'mode' must be a single value ('multiplex' or 'weighted').")
  }
  
  # Check if mode has valid value
  if(!mode %in% c("multiplex","weighted")){
    stop("Argument 'mode' must be either 'multiplex' or 'weighted'.")
  }

  num_graphs <- length(textnet_igraphs)
  supernodes <- vector(mode = "list", length = length(num_graphs))
  superedges <- vector(mode = "list", length = length(num_graphs))
  for(m in 1:num_graphs){
    single_ig <- textnet_igraphs[[m]]
    sidf <- igraph::as_data_frame(single_ig, what = "both")
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
    
    for(q in igraph::edge_attr_names(weighted_graph)){
      weighted_graph <- igraph::delete_edge_attr(weighted_graph, q)
    }
    
    igraph::E(weighted_graph)$weight <- 1
    weighted_graph <- igraph::simplify(weighted_graph, edge.attr.comb=list(weight="sum"), remove.loops = F)
    
    return(weighted_graph)
    
  }else{
    return(supernetwork)
  }
  
}
