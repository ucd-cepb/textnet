library(network)
library(sna)
library(igraph)
library(intergraph)
#igr <- graph_from_adjacency_matrix(adj_mat_subset, mode = "undirected", 
#                                   weighted = TRUE, diag = FALSE)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)

network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=7))
names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                               "connectedness", "centralization", "num_communities")

for(m in 1:length(gspids)){
  agency_net <- readRDS(paste0("data/network_",gspids[m]))
  igr <- asIgraph(agency_net)
  set.seed(327856)
  lc <- cluster_louvain(igr)
  #TODO add num communities
  network_properties[m,] <- c(gspids[m], network::network.size(agency_net), network::network.edgecount(agency_net),
                          network::network.density(agency_net),
                          sna::connectedness(agency_net),sna::centralization(agency_net,sna::degree),
                          length(unique(lc$membership)))
}

saveRDS(network_properties, "data/network_properties")

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

