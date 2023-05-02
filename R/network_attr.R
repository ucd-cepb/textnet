library(network)
library(sna)
library(igraph)
library(intergraph)
#igr <- graph_from_adjacency_matrix(adj_mat_subset, mode = "undirected", 
#                                   weighted = TRUE, diag = FALSE)

#CHOOSE ONE
type = "governance"
type = "topic"
type = "governanceORGs"

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=10))
names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                               "connectedness", "centralization", "transitivity",
                               "num_communities", "modularity", 
                               "percent_homophily")

for(m in 1:length(gspids)){
  
  if(type=="governance"){
    net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
    igr <- asIgraph(net)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$type[edgelist$from]==nodelist$type[edgelist$to]) / nrow(edgelist)
    
  }
  if(type=="topic"){
    net <- readRDS(paste0("data/topic_network_",gspids[m]))$posadj
    igr <- igraph::graph.adjacency(net, mode = "undirected",weighted=NULL,diag=F)
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    percent_homophily <- NA
    #topics do not have "types", so percent_homophily is not applicable
  }
  if(type=="governanceORGs"){
    net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
    igr <- asIgraph(net) 
    igr <- delete_vertices(igr, get.vertex.attribute(igr, "type") != "ORG")
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$type[edgelist$from]==nodelist$type[edgelist$to]) / nrow(edgelist)
  }
  
  set.seed(327856)
  lc <- cluster_louvain(igr)
  
  #TODO add num communities
  
  network_properties[m,] <- c(gspids[m], 
                              network::network.size(net), 
                              network::network.edgecount(net),
                              network::network.density(net),
                              sna::connectedness(net),
                              sna::centralization(net,sna::degree),
                              sna::gtrans(net, mode = "graph", use.adjacency=F),
                              length(unique(lc$membership)), 
                              modularity(igr,lc$membership,edgelist$weights),
                              percent_homophily
                              )
}

if(type=="governance"){
  
  saveRDS(network_properties, "data/network_properties")
  
}
if(type=="topic"){
  
  saveRDS(network_properties, "data/topic_network_properties")
  
}
if(type=="governanceORGs"){
  
  saveRDS(network_properties, "data/network_properties_orgs")
  
}


#extremes of connection and centralization
min_connect <- network_properties_df %>% 
  filter(connectedness == min(connectedness))
max_connect <- network_properties_df %>% 
  filter(connectedness == max(connectedness))
min_centr <- network_properties_df %>% 
  filter(centralization == min(centralization))
max_centr <- network_properties_df %>% 
  filter(centralization == max(centralization))
min_comm <- network_properties_df %>% 
  filter(num_communities== min(num_communities))
max_comm <- network_properties_df %>% 
  filter(num_communities== max(num_communities))