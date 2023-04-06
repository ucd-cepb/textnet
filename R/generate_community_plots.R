library(network)
library(igraph)
library(intergraph)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)

for(m in 1:length(gspids)){
  agency_net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
  
  igr <- asIgraph(agency_net)
  set.seed(327856)
  lc <- cluster_louvain(igr)
  #V(igr)$realname <- V(igr)$name
  V(igr)$realname <- V(igr)$vertex.names
  
  #V(igr)$name <- 1:length(V(igr)$name)
  V(igr)$vertex.names <- 1:length(V(igr)$vertex.names)
  
  V(igr)$bigname <- ifelse(igraph::degree(igr)>50,V(igr)$realname,"")
  
  png(paste0("figures/igraph_",gspids[m],".png"), 4020, 1890)
  plot(igr, vertex.size = 3, vertex.label = V(igr)$bigname,
       vertex.color=rainbow(num_communities, alpha=0.3)[lc$membership])
  dev.off()
  print(m)
}