library(network)
library(tidyverse)
library(sna)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)

for(m in 1:length(gspids)){
  
  if(!file.exists(paste0("data/node_attr_",gspids[m]))){
    agency_net <- readRDS(paste0("data/network_",gspids[m]))
    closens <- sna::closeness(agency_net, gmode = "graph", cmode="undirected")
    between <- sna::betweenness(agency_net,gmode = "graph",cmode="undirected")
    deg <- sna::degree(agency_net, gmode = "graph", cmode = "undirected")
    eigenvector <- sna::evcent(agency_net, gmode = "graph")
    centr_df <- tibble(name = network::get.vertex.attribute(agency_net, "vertex.names"), 
                       closens, between, deg, eigenvector, 
                       type = network::get.vertex.attribute(agency_net, "type"))
    saveRDS(centr_df, paste0("data/node_attr_",gspids[m]))
  }
}  
  





