library(network)
library(sna)
library(igraph)
library(intergraph)
#igr <- graph_from_adjacency_matrix(adj_mat_subset, mode = "undirected", 
#                                   weighted = TRUE, diag = FALSE)

#CHOOSE ONE
type = "governance_undir"
type = "topic"
type = "governanceORGs_undir"
type = "governance_dir_full"
type = "governance_dir_weighted"

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

if(type=="governance_dir_full"){
  network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=32))
  names(network_properties) <- c("gsp_id", "num_nodes", "num_edges",
                                 "connectedness", "centralization", "transitivity",
                                 "num_communities", "modularity", 
                                 "percent_homophily", "reciprocity", 
                                 "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                 "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                 "percent_gpe_to_org","percent_gpe_to_person","percent_gpe_to_gpe",
                                 "percent_org","percent_persons","percent_gpe", 
                                 "median_out_ties","mean_out_ties",
                                 "median_in_ties","mean_in_ties",
                                 "percent_vbn", "percent_vbg","percent_vbp",
                                 "percent_vbd","percent_vb","percent_vbz")
}else if(type=="governance_dir_weighted"){
  network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=33))
  names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                 "connectedness", "centralization", "transitivity",
                                 "num_communities", "modularity", 
                                 "percent_homophily", "reciprocity", 
                                 "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                 "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                 "percent_gpe_to_org","percent_gpe_to_person","percent_gpe_to_gpe",
                                 "percent_org","percent_persons","percent_gpe", 
                                 "median_num_out_neighbors","mean_num_out_neighbors",
                                 "median_num_in_neighbors","mean_num_in_neighbors",
                                 "percent_vbn", "percent_vbg","percent_vbp",
                                 "percent_vbd","percent_vb","percent_vbz")
}else{
  network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=10))
  names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                 "connectedness", "centralization", "transitivity",
                                 "num_communities", "modularity", 
                                 "percent_homophily")
}


for(m in 1:length(gspids)){
  if(type=="governance_dir_full"){
    igr <- readRDS(paste0("data/full_directed_graph_",gspids[m]))
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
    reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
    
    set.seed(327856)
    lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
    
    network_properties[m,] <- c(gspids[m], 
                                network::network.size(net), 
                                network::network.edgecount(net),
                                #no way to define density for multiplex network
                                sna::connectedness(net),
                                sna::centralization(net,sna::degree),
                                sna::gtrans(net, mode = "graph", use.adjacency=F),
                                length(unique(lc$membership)), 
                                modularity(igr,lc$membership,edgelist$weights),
                                percent_homophily,
                                reciprocated,
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
                                sum(nodelist$entity_type=="GPE")/nrow(nodelist),
                                #degree includes loops by default
                                median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                sum(edgelist$head_verb_tense=="VBN")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBG")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBP")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBD")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VB")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBZ")/nrow(edgelist)
                                
                                )
    
  }
  if(type=="governance_dir_weighted"){
    
    igr <- readRDS(paste0("data/full_directed_graph_",gspids[m]))
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
    reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
    
    set.seed(327856)
    lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
    
    network_properties[m,] <- c(gspids[m], 
                                network::network.size(net), 
                                network::network.edgecount(net),
                                network::network.density(net),
                                sna::connectedness(net),
                                sna::centralization(net,sna::degree),
                                sna::gtrans(net, mode = "graph", use.adjacency=F),
                                length(unique(lc$membership)), 
                                modularity(igr,lc$membership,edgelist$weights),
                                percent_homophily,
                                reciprocated,
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
                                sum(nodelist$entity_type=="GPE")/nrow(nodelist),
                                #degree includes loops by default
                                median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                sum(edgelist$head_verb_tense=="VBN")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBG")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBP")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBD")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VB")/nrow(edgelist),
                                sum(edgelist$head_verb_tense=="VBZ")/nrow(edgelist)
                                
    )
  }
  if(type=="governance_undir"){
    net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
    igr <- asIgraph(net)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$type[edgelist$from]==nodelist$type[edgelist$to]) / nrow(edgelist)
    
    set.seed(327856)
    lc <- cluster_louvain(igr)
    
    network_properties[m,] <- c(gspids[m], 
                                network::network.size(net), 
                                network::network.edgecount(net),
                                network::network.density(net),
                                sna::connectedness(net),
                                sna::centralization(net,sna::degree),
                                sna::gtrans(net, mode = "graph", use.adjacency=F),
                                length(unique(lc$membership)), 
                                modularity(igr,lc$membership,edgelist$weights),
                                percent_homophily)
  }
  if(type=="topic"){
    net <- readRDS(paste0("data/topic_network_",gspids[m]))$posadj
    igr <- igraph::graph.adjacency(net, mode = "undirected",weighted=NULL,diag=F)
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    #topics do not have "types", so percent_homophily is not applicable
    
    set.seed(327856)
    lc <- cluster_louvain(igr)
    
    network_properties[m,] <- c(gspids[m], 
                                network::network.size(net), 
                                network::network.edgecount(net),
                                network::network.density(net),
                                sna::connectedness(net),
                                sna::centralization(net,sna::degree),
                                sna::gtrans(net, mode = "graph", use.adjacency=F),
                                length(unique(lc$membership)), 
                                modularity(igr,lc$membership,edgelist$weights))
  }
  if(type=="governanceORGs_undir"){
    net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
    igr <- asIgraph(net) 
    igr <- delete_vertices(igr, get.vertex.attribute(igr, "type") != "ORG")
    net <- asNetwork(igr)
    edgelist <- get.data.frame(igr, what = "edges")
    nodelist <- get.data.frame(igr, what = "vertices")
    percent_homophily <- sum(nodelist$type[edgelist$from]==nodelist$type[edgelist$to]) / nrow(edgelist)
  
    set.seed(327856)
    lc <- cluster_louvain(igr)
    
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
  
}

if(type=="governance_undir"){
  
  saveRDS(network_properties, "data/network_properties_undir")
  
}
if(type=="topic"){
  
  saveRDS(network_properties, "data/topic_network_properties")
  
}
if(type=="governanceORGs_undir"){
  
  saveRDS(network_properties, "data/network_properties_orgs_undir")
  
}
if(type=="governance_dir_full"){
  
  saveRDS(network_properties, "data/dir_full_network_properties")
  
}
if(type=="governance_dir_weighted"){
  
  saveRDS(network_properties, "data/dir_weighted_network_properties")
  
}

network_properties <- sapply(network_properties, function(x) as.numeric(x))
network_properties_summary_table <- network_properties[,c("num_nodes", "num_edges",
                                                          "connectedness", "centralization", "transitivity",
                                                          "num_communities", "modularity", 
                                                          "percent_homophily", "reciprocity", 
                                                          "percent_org_to_org",
                                                          "percent_org","mean_out_ties","mean_in_ties",
                                                          "percent_vbn", "percent_vbg","percent_vbp",
                                                          "percent_vbd","percent_vb","percent_vbz")]

View(summary(network_properties_summary_table,digits=2))

network_properties_for_pairs <- network_properties[,c("connectedness", "centralization", "transitivity",
                                                                "num_communities", "modularity", 
                                                                "percent_homophily", "reciprocity", 
                                                                "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                                                "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                                                "percent_gpe_to_org","percent_gpe_to_person",
                                                                "mean_out_ties",
                                                                "percent_vbn", "percent_vbg","percent_vbp",
                                                                "percent_vbd","percent_vb","percent_vbz")]

library(GGally) 
library(ggplot2)



verbtensepairs <- ggpairs(as.data.frame(network_properties_for_pairs[,(16:22)]))+theme_bw()
#summarypairs1 did not include column 7
summarypairs2 <- ggpairs(as.data.frame(network_properties_for_pairs[,(c(1:5,7,16))]))+theme_bw()

ggsave(paste0("verb_tense_and_degree.png"), plot = verbtensepairs, device = "png",
       path = "figures", width = 4020, height = 3015, dpi = 300,
       units = "px", bg = "white")

ggsave(paste0("summarypairs2.png"), plot = summarypairs2, device = "png",
       path = "figures", width = 4020, height = 3015, dpi = 300,
       units = "px", bg = "white")

net_with_gsa_attr <- cbind(gsp_mini, network_properties)
net_with_gsa_attr <- net_with_gsa_attr[,c(2,3,4,6:14,30)]
net_with_gsa_attr[,2] <- as.factor(t(net_with_gsa_attr[,2]))
net_with_gsa_attr[,1] <- as.factor(t(net_with_gsa_attr[,1]))
net_with_gsa_attr[,3] <- as.factor(t(net_with_gsa_attr[,3]))
#exploring whether gsa attributes are correlated with any network properties
#does not appear so.
ggpairs(as.data.frame(net_with_gsa_attr)[,c(2,4:10)])

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