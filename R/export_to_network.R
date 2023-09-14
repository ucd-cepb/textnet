# Exported function
# export_to_network

#' Takes a textnet_extract output and converts it into an "igraph" or "network" object, then calculates network summary statistics. Note, this function does not preserve incomplete edges. The verblist and appositivelist are not used.
#'
#' @param textnet_extract An object output from textnet_extract
#' @param keep_isolates A boolean, where T means to keep nodes from the nodelist that are not included in the edgelist, and F otherwise.
#' @param export_format A string, either "igraph" for an igraph object, or "network" for a network object
#' @param collapse_edges A boolean, where T removes edge attributes and collapses edges into a single weighted edge, and where F preserves all edges and edge attributes
#' @param self_loops A boolean, where T allows self-loops, and F removes them
#' @return Returns the "igraph" or "network" object as the first element and the network statistics as the second element. 
#' num_nodes -- the number of nodes in the resulting "igraph" or "network" object
#' num_edges -- the number of edges in the resulting "igraph" or "network" object
#' connectedness -- calculates sna::connectedness
#' centralization -- degree centralization
#' transitivity -- directed weak transitivity
#' pct_entitytype_homophily -- percent of all edges whose terminal nodes share an entity type
#' reciprocity -- reciprocity
#' median_in_degree, median_out_degree, mean_in_degree, mean_out_degree -- the median, or mean in-degree or out-degree of all nodes in the network object, respectively
#' 
#' For weighted graphs, if collapse_edges == T:
#' modularity -- calculates igraph::modularity on a weighted, undirected, non-multiplex version of the network.
#' num_communities -- number of communities using louvain cluster algorithm on a weighted, undirected, non-multiplex version of the network
#' density -- density of the graph
#' mean_edge_weight -- mean weight of all edges in the graph
#' mean_in_strength, mean_out_strength, median_in_strength, median_out_strength -- the mean or median strength -- weighted in-degree or weighted out-degree, respectively -- of a node in the network
#' 
#' For multiplex graphs, if collapse_edges == F:
#' modularity -- calculates igraph::modularity on a weighted, undirected, non-multiplex version of the network.
#' num_communities -- number of communities using louvain cluster algorithm on a weighted, undirected, non-multiplex version of the network
#' percent_vbn, percent_vbg, percent_vpb, percent_vbd, percent_vb, percent_vbz -- percent of edges in the graph that are of the respective verb tense
#' 
#' @export
#'

export_to_network <- function(textnet_extract, export_format, keep_isolates=T, collapse_edges, self_loops){
  if(!export_format %in% c("igraph","network")){
    stop("export_format must be either 'igraph' or 'network'")
  }
  if(!keep_isolates %in% c(T,F)){
    stop("keep_isolates must be either T or F.")
  }
  if(!collapse_edges %in% c(T,F)){
    stop("collapse_edges must be either T or F.")
  }
  
  textnet_extract$edgelist <-  dplyr::filter(textnet_extract$edgelist, !is.na(textnet_extract$edgelist$source) & !is.na(textnet_extract$edgelist$target))
  
  
    #make igraph object.
    if(keep_isolates==T & collapse_edges == F){
      #keep all
      igr <- igraph::graph_from_data_frame(d = textnet_extract$edgelist, 
                                           vertices = textnet_extract$nodelist, 
                                           directed = T)
      if(self_loops==F){
        igr <- igraph::delete.edges(igr, igraph::E(igr)[igraph::is.loop(igr)])
        
      }
    }else if(keep_isolates==T & collapse_edges == T){
      #keep isolates but use weighted
      igr <- igraph::graph_from_data_frame(d = textnet_extract$edgelist, 
                                           vertices = textnet_extract$nodelist, 
                                           directed = T)
      
      igr <- igraph::delete_edge_attr(igr, "head_verb_id")
      igr <- igraph::delete_edge_attr(igr, "head_verb_tense")
      igr <- igraph::delete_edge_attr(igr, "head_verb_name")
      igr <- igraph::delete_edge_attr(igr, "head_verb_lemma")
      igr <- igraph::delete_edge_attr(igr, "parent_verb_id")
      igr <- igraph::delete_edge_attr(igr, "neg")
      igr <- igraph::delete_edge_attr(igr, "doc_sent_verb")
      igr <- igraph::delete_edge_attr(igr, "doc_sent_parent")
      igr <- igraph::delete_edge_attr(igr, "helper_lemma")
      igr <- igraph::delete_edge_attr(igr, "helper_token")
      igr <- igraph::delete_edge_attr(igr, "xcomp_verb")
      igr <- igraph::delete_edge_attr(igr, "xcomp_helper_lemma")
      igr <- igraph::delete_edge_attr(igr, "xcomp_helper_token")
      igr <- igraph::delete_edge_attr(igr, "edgeiscomplete")
      igr <- igraph::delete_edge_attr(igr, "has_hedge")
      igr <- igraph::delete_edge_attr(igr, "is_future")
      
      igraph::E(igr)$weight <- 1
      igr <- igraph::simplify(igr, edge.attr.comb=list(weight="sum"), remove.loops = !self_loops)
      
    }else if(keep_isolates==F & collapse_edges == F){
      #remove isolates but use original edges
      igr <- igraph::graph_from_data_frame(d = textnet_extract$edgelist,  
                                           vertices = textnet_extract$nodelist, 
                                           directed = T)
      if(self_loops==F){
        igr <- igraph::delete.edges(igr, igraph::E(igr)[igraph::is.loop(igr)])
        
      }
      igr <- igraph::delete.vertices(igr, igraph::V(igr)[igraph::degree(igr, igraph::V(igr), mode = "all",loops = self_loops)==0])
    }else if(keep_isolates==F & collapse_edges == T){
      #remove isolates and use weighted
      igr <- igraph::graph_from_data_frame(d = textnet_extract$edgelist,  
                                           vertices = textnet_extract$nodelist, 
                                           directed = T)
      
      
      igr <- igraph::delete_edge_attr(igr, "head_verb_id")
      igr <- igraph::delete_edge_attr(igr, "head_verb_tense")
      igr <- igraph::delete_edge_attr(igr, "head_verb_name")
      igr <- igraph::delete_edge_attr(igr, "head_verb_lemma")
      igr <- igraph::delete_edge_attr(igr, "parent_verb_id")
      igr <- igraph::delete_edge_attr(igr, "neg")
      igr <- igraph::delete_edge_attr(igr, "doc_sent_verb")
      igr <- igraph::delete_edge_attr(igr, "doc_sent_parent")
      igr <- igraph::delete_edge_attr(igr, "helper_lemma")
      igr <- igraph::delete_edge_attr(igr, "helper_token")
      igr <- igraph::delete_edge_attr(igr, "xcomp_verb")
      igr <- igraph::delete_edge_attr(igr, "xcomp_helper_lemma")
      igr <- igraph::delete_edge_attr(igr, "xcomp_helper_token")
      igr <- igraph::delete_edge_attr(igr, "edgeiscomplete")
      igr <- igraph::delete_edge_attr(igr, "has_hedge")
      igr <- igraph::delete_edge_attr(igr, "is_future")
      
      igraph::E(igr)$weight <- 1
      igr <- igraph::simplify(igr, edge.attr.comb=list(weight="sum"), remove.loops = !self_loops)
      igr <- igraph::delete.vertices(igr, igraph::V(igr)[igraph::degree(igr, igraph::V(igr), mode = "all",loops = self_loops)==0])
      
    }

  #network object
  agency_df <- igraph::get.data.frame(igr, what = "both")
  if(keep_isolates==T & collapse_edges == F){
    #keep_all
    net <- network::network(x=agency_df$edges, directed = T,
                   hyper = F, loops = self_loops, multiple = T, 
                   bipartiate = F, vertices = agency_df$vertices,
                   matrix.type = "edgelist")
  }else if(keep_isolates==T & collapse_edges == T){
    #TODO keep isolates but use weighted
    net <- network::network(x=agency_df$edges, directed = T,
                   hyper = F, loops = self_loops, multiple = F, 
                   bipartiate = F, vertices = agency_df$vertices,
                   matrix.type = "edgelist")
  }else if(keep_isolates==F & collapse_edges == F){
    #remove isolates but use original edges
    net <- network::network(x=agency_df$edges[,1:2], directed = T,
                   hyper = F, loops = self_loops, multiple = T, 
                   bipartiate = F, 
                   matrix.type = "edgelist")
  }else if(keep_isolates==F & collapse_edges == T){
    #remove isolates and use weighted
    net <- network::network(x=agency_df$edges, directed = T,
                   hyper = F, loops = self_loops, multiple = F,
                   bipartiate = F, 
                   matrix.type = "edgelist")
  }
  
  
  attr_tbl <- data.frame(matrix(NA, nrow = 1, ncol=11))
  names(attr_tbl) <- c("num_nodes","num_edges","connectedness",
                       "centralization","transitivity",
                       "pct_entitytype_homophily","reciprocity",
                       "mean_in_degree","mean_out_degree","median_in_degree",
                       "median_out_degree")
  
  attr_tbl$num_nodes <- network::network.size(net)
  attr_tbl$num_edges <- network::network.edgecount(net)
  attr_tbl$connectedness <- sna::connectedness(net)
  attr_tbl$centralization <- sna::centralization(net,sna::degree, diag=self_loops)
  attr_tbl$transitivity <- sna::gtrans(net, mode = "digraph", measure = "weak", diag=self_loops, use.adjacency=F)
  attr_tbl$pct_entitytype_homophily <- mean(igraph::vertex_attr(igr, "entity_type",igraph::head_of(igr, igraph::E(igr))) == igraph::vertex_attr(igr, "entity_type",igraph::tail_of(igr, igraph::E(igr))))
  attr_tbl$reciprocity <- igraph::reciprocity(igr, ignore.loops = !self_loops, mode = "default")
  attr_tbl$mean_in_degree <- mean(igraph::degree(igr,mode="in", loops=self_loops))
  attr_tbl$mean_out_degree <- mean(igraph::degree(igr,mode="out",loops=self_loops))
  attr_tbl$median_in_degree <- stats::median(igraph::degree(igr,mode="in",loops=self_loops))
  attr_tbl$median_out_degree <- stats::median(igraph::degree(igr,mode="out",loops=self_loops))
  
  if(collapse_edges==T){
    #uses weighted graph to create undirected weighted graph, where new weight=sum of dir edge weights
    undir <- igraph::as.undirected(igr, mode = "collapse")
    lc <- igraph::cluster_louvain(undir, weights = igraph::edge_attr(undir,"weight"))#uses weights of undirected igraph
    attr_tbl$modularity <- igraph::modularity(undir,membership = lc$membership, weights = igraph::edge_attr(undir, "weight"))
    attr_tbl$num_communities <- length(base::unique(lc$membership))
    attr_tbl$density <- network::network.density(net)
    attr_tbl$mean_edge_weight <- mean(igraph::edge_attr(igr, "weight"))
    attr_tbl$mean_in_strength <- mean(igraph::strength(igr, mode="in", loops=self_loops))
    attr_tbl$mean_out_strength <- mean(igraph::strength(igr,mode="out", loops=self_loops))
    attr_tbl$median_in_strength <- stats::median(igraph::strength(igr, mode = "in", loops=self_loops))
    attr_tbl$median_out_strength <- stats::median(igraph::strength(igr, mode="out", loops=self_loops))
  }else{
    
    
    undir <- igraph::delete_edge_attr(igr, "head_verb_id")
    undir <- igraph::delete_edge_attr(undir, "head_verb_tense")
    undir <- igraph::delete_edge_attr(undir, "head_verb_name")
    undir <- igraph::delete_edge_attr(undir, "head_verb_lemma")
    undir <- igraph::delete_edge_attr(undir, "parent_verb_id")
    undir <- igraph::delete_edge_attr(undir, "neg")
    undir <- igraph::delete_edge_attr(undir, "doc_sent_verb")
    undir <- igraph::delete_edge_attr(undir, "doc_sent_parent")
    undir <- igraph::delete_edge_attr(undir, "helper_lemma")
    undir <- igraph::delete_edge_attr(undir, "helper_token")
    undir <- igraph::delete_edge_attr(undir, "xcomp_verb")
    undir <- igraph::delete_edge_attr(undir, "xcomp_helper_lemma")
    undir <- igraph::delete_edge_attr(undir, "xcomp_helper_token")
    undir <- igraph::delete_edge_attr(undir, "edgeiscomplete")
    undir <- igraph::delete_edge_attr(undir, "has_hedge")
    undir <- igraph::delete_edge_attr(undir, "is_future")
    
    igraph::E(undir)$weight <- 1
    undir <- igraph::simplify(undir, edge.attr.comb=list(weight="sum"), remove.loops = !self_loops)
    undir <- igraph::as.undirected(igr, mode = "collapse")
    lc <- igraph::cluster_louvain(undir, weights = igraph::edge_attr(undir,"weight"))#uses weights of undirected igraph
    attr_tbl$modularity <- igraph::modularity(undir,membership = lc$membership, weights = igraph::edge_attr(undir, "weight"))
    attr_tbl$num_communities <- length(base::unique(lc$membership))
    attr_tbl$percent_vbn <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VBN")
    attr_tbl$percent_vbg <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VBG")
    attr_tbl$percent_vbp <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VBP")
    attr_tbl$percent_vbd <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VBD")
    attr_tbl$percent_vb <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VB")
    attr_tbl$percent_vbz <- mean(igraph::edge_attr(igr,"head_verb_tense")=="VBZ")
  }

  if(export_format=="igraph"){
    return(list(igr, attr_tbl)) 
  }else{
    return(list(net, attr_tbl)) 
  }
   

}


