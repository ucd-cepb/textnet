
library(textNet)
library(stringr)
library(testthat)

#download sample pdf data from state website
#this is the same as vignettes/old.pdf and vignettes/new.pdf
#that are included in the package, EXCEPT the package versions have pages that are maps or figures removed to shrink file size.

#URL <- "https://sgma.water.ca.gov/portal/service/gspdocument/download/2840"
#download.file(URL, destfile = "vignettes/old.pdf", #method="curl")

#URL <- "https://sgma.water.ca.gov/portal/service/gspdocument/download/9625"
#download.file(URL, destfile = "vignettes/new.pdf", method="curl")
pdfs <- c(system.file("extdata", "old.pdf", package = "textNet"),         
          system.file("extdata", "new.pdf", package = "textNet"))

old_new_text <- textNet::pdf_clean(pdfs, ocr=F, maxchar=10000, 
                                   export_paths=NULL, return_to_memory=T, suppressWarn = F, 
                                   auto_headfoot_remove = T)
names(old_new_text) <- c("old","new")

#we expect one element per pdf
test_that("number of elements equals number of files", {
  expect_equal(length(old_new_text), length(pdfs))
})


### Pre-Processing Step II: Parse Text


library(findpython)
ret_path <- find_python_cmd(required_modules = c('spacy', 'en_core_web_lg'))

water_bodies <- c("surface water", "Surface water", "groundwater", "Groundwater", 
                  "San Joaquin River", "Cottonwood Creek", "Chowchilla Canal Bypass", 
                  "Friant Dam", "Sack Dam", "Friant Canal", "Chowchilla Bypass", 
                  "Fresno River", "Sacramento River", "Merced River","Chowchilla River", 
                  "Bass Lake", "Crane Valley Dam", "Willow Creek", "Millerton Lake", 
                  "Mammoth Pool", "Dam 6 Lake", "Delta","Tulare Lake", 
                  "Madera-Chowchilla canal", "lower aquifer", "upper aquifer", 
                  "upper and lower aquifers", "lower and upper aquifers", 
                  "Lower aquifer", "Upper aquifer", "Upper and lower aquifers", 
                  "Lower and upper aquifers")

if(!requireNamespace("spacyr", quietly = T)){
  stop("Package 'spacyr' must be installed to use this function.",
       call.=F)
}
old_new_parsed <- textNet::parse_text(ret_path, 
                                      keep_hyph_together = F, 
                                      phrases_to_concatenate = water_bodies, 
                                      concatenator = "_", 
                                      text_list = old_new_text,   
                                      parsed_filenames=c("old_parsed","new_parsed"), 
                                      overwrite = T,
                                      custom_entities = list(WATER = water_bodies))

#expect all pages are preserved 
for(k in 1:length(old_new_parsed)){
  maxpage <- max(as.numeric(stringr::str_remove(old_new_parsed[[k]]$doc_id, "text")))
  test_that("all pages are preserved", {
    expect_equal(maxpage, length(old_new_text[[k]]))
  })
  
}





### Extraction Expectations

  

ent_types <- c('ORG','GPE','PERSON','WATER')
extracts <- vector(mode="list",length=length(old_new_parsed))
for(m in 1:length(old_new_parsed)){
  extracts[[m]] <- textnet_extract(old_new_parsed[[m]],cl=2,
                                   keep_entities = ent_types, keep_incomplete_edges=T)
}




#test conditions
for(m in 1:length(old_new_parsed)){
  #checking list of entities
  onp <- old_new_parsed[[m]] |> dplyr::mutate(
    entitynum = cumsum(str_detect(entity, "_B")))
  onp$entitynum <- ifelse(onp$entity == "", NA, onp$entitynum)
  onp <- onp |> dplyr::group_by(entitynum) |> dplyr::mutate(entityconcat = paste(
    token, collapse = "_"))
  onp$entityconcat <- ifelse(str_detect(onp$entity, 
                                        paste0(ent_types, "_B", sep = "", collapse = "|")), onp$entityconcat, NA)
  
  #node entities should be a subset of all entities since
  #sometimes there are improper sentences that cause
  #allentities to not make it to the nodelist
  remove_nums <- ifelse("DATE" %in% ent_types | "CARDINAL" %in% ent_types |
                          "QUANTITY" %in% ent_types | "TIME" %in% ent_types |
                          "MONEY" %in% ent_types | "PERCENT" %in% ent_types, F, T)
  
  allentities <- onp$entityconcat[!is.na(onp$entityconcat)]
  allentities <- clean_entities(allentities, remove_nums)
  allentities <- unique(sort(allentities))
  nodentities <- unique(sort(extracts[[m]]$nodelist$entity_name))
  #sometimes appositives happen in the middle of the entity name, which textNet removes
  nodentities <- nodentities |> str_replace_all("_", "_.*_*")
  
  #this method accounts for the fact that the nodentity might be a substring of the 
  #original entity, since it may have included an appositive
  test_that("each node entity is found in original collection of entities",{
    expect_equal(all(unlist(lapply(nodentities, function(j) any(str_detect(
      string = allentities, pattern = j))))), T)
  })
  
}




### Entity Consolidation Expectations




old_acronyms <- find_acronyms(old_new_text[[1]])
new_acronyms <- find_acronyms(old_new_text[[2]])

print(head(old_acronyms))






tofrom <- data.table::data.table(
  from = c(as.list(old_acronyms$acronym),
           list("Sub_basin",
                "Sub_Basin",
                "upper_and_lower_aquifers",
                "Upper_and_lower_aquifers",
                "Lower_and_upper_aquifers",
                "lower_and_upper_aquifers")), 
  to = c(as.list(old_acronyms$name),
         list("Subbasin",
              "Subbasin",
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"))))

old_extract_clean <- disambiguate(
  textnet_extract = extracts[[1]],
  from = tofrom$from, 
  to = tofrom$to,
  match_partial_entity = c(rep(F,nrow(old_acronyms)),T,T,F,F,F,F))

#we shouldn't have changed the overall structure of the data
test_that("disambiguate process does not change number of files", {
  expect_equal(length(old_extract_clean), length(extracts[[1]]))
})

#we converted from acronyms to full names so should not see any acronyms
test_that("There are no remaining acronyms in the list of entities, since they are now converted to their full name.",{
  expect_equal(any(str_detect(old_extract_clean$nodelist$entity_name,
                              paste0("^", paste0(old_acronyms$acronym, collapse = "$|^"),
                                     "$"))), F)
})

tofrom <- data.table::data.table(
  from = c(as.list(new_acronyms$acronym),
           list("Sub_basin",
                "Sub_Basin",
                "upper_and_lower_aquifers",
                "Upper_and_lower_aquifers",
                "Lower_and_upper_aquifers",
                "lower_and_upper_aquifers")), 
  to = c(as.list(new_acronyms$name),
         list("Subbasin",
              "Subbasin",
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"),
              c("upper_aquifer","lower_aquifer"))))

new_extract_clean <- disambiguate(
  textnet_extract = extracts[[2]], 
  from = tofrom$from, 
  to = tofrom$to,
  match_partial_entity = c(rep(F,nrow(new_acronyms)),T,T,F,F,F,F))



### Network Attribute Expectations



set.seed(50000)
old_extract_net <- export_to_network(old_extract_clean, "igraph", keep_isolates = F, 
                                     collapse_edges = F, self_loops = T)

test_that("The first element of the resulting object is an igraph",{
  expect_equal(igraph::is_igraph(old_extract_net[[1]]), T)
})

set.seed(50000)
new_extract_net <- export_to_network(new_extract_clean, "igraph", keep_isolates = F, 
                                     collapse_edges = F, self_loops = T)

test_that("The first element of this resulting object is also an igraph",{
  expect_equal(igraph::is_igraph(new_extract_net[[1]]), T)
})

table <- t(format(rbind(old_extract_net[[2]], new_extract_net[[2]]), digits = 3, 
                  scientific = F))
colnames(table) <- c("old","new")
print(table)





library(ggraph)
set.seed(50000)
old_extract_plot <- export_to_network(old_extract_clean, "igraph", keep_isolates = F, 
                                      collapse_edges = T, self_loops = T)[[1]]
set.seed(50000)
new_extract_plot <- export_to_network(new_extract_clean, "igraph", keep_isolates = F, 
                                      collapse_edges = T, self_loops = T)[[1]]
#order of these layers matters
set.seed(50000)
ggraph(old_extract_plot, layout = 'fr')+
  geom_edge_fan(aes(alpha = weight),
                end_cap = circle(1,"mm"),
                color = "#000000",
                width = 0.3,
                arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",
                              type = "closed"))+
  #from Paul Tol's bright color scheme
  scale_color_manual(values = c("#4477AA","#228833","#CCBB44","#66CCEE"))+
  geom_node_point(aes(color = entity_type), size = 1,
                  alpha = 0.8)+
  labs(title= "Old Network")+
  theme_void()

#order of these layers matters
set.seed(50000)
ggraph(new_extract_plot, layout = 'fr')+
  geom_edge_fan(aes(alpha = weight),
                end_cap = circle(1,"mm"),
                color = "#000000",
                width = 0.3,
                arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",
                              type = "closed"))+
  #from Paul Tol's bright color scheme
  scale_color_manual(values = c("#4477AA","#228833","#CCBB44","#66CCEE"))+
  geom_node_point(aes(color = entity_type), size = 1,
                  alpha = 0.8)+
  labs(title= "New Network")+
  theme_void()



### Edge Attribute Expectations



top_feats <- top_features(list(old_extract_net[[1]], new_extract_net[[1]]))
head(top_feats[[2]],10)





table(igraph::E(old_extract_net[[1]])$head_verb_tense)


### Composite Network Expectations




composite_net <- combine_networks(list(old_extract_net[[1]], new_extract_net[[1]]), 
                                  mode = "weighted")

#we expect the new nodes to be in the cleaned extracts
expect_contains(c(old_extract_clean$nodelist$entity_name,
                  new_extract_clean$nodelist$entity_name),
                igraph::vertex_attr(composite_net, "name"))
set.seed(50000)
ggraph(composite_net, layout = 'fr')+
  geom_edge_fan(aes(alpha = weight),
                end_cap = circle(1,"mm"),
                color = "#000000",
                width = 0.3,
                arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",
                              type = "closed"))+
  #from Paul Tol's bright color scheme
  scale_color_manual(values = c("#4477AA","#228833","#CCBB44","#66CCEE"))+
  geom_node_point(aes(color = entity_type), size = 1,
                  alpha = 0.8)+
  labs(title= "Composite Network")+
  theme_void()




### Node Attribute Expectations



library(network)
library(igraph)

top_feats <- top_features(list(old_extract_net[[1]], new_extract_net[[1]]))
print(head(top_feats[[1]],10))





composite_tbl <- igraph::as_data_frame(composite_net, what = "vertices")
composite_tbl <- composite_tbl[,c("name","num_graphs_in")]

#prepare data frame version of old network, to add composite_tbl variables
old_tbl <- igraph::as_data_frame(old_extract_net[[1]], what = "both")
#this adds the num_graphs_in variable from composite_tbl
old_tbl$vertices <- dplyr::left_join(old_tbl$vertices, composite_tbl)
#turn back into a network
old_net <- network::network(x=old_tbl$edges[,1:2], directed = T,
                            hyper = F, loops = T, multiple = T, 
                            bipartiate = F, vertices = old_tbl$vertices,
                            matrix.type = "edgelist")
#we need a matrix version for some node statistics
set.seed(50000)
old_mat <- as.matrix(as.matrix(export_to_network(old_extract_clean, "igraph", 
                                                 keep_isolates = F, collapse_edges = T, self_loops = F)[[1]]))

#prepare data frame version of new network, to add composite_tbl variables
new_tbl <- igraph::as_data_frame(new_extract_net[[1]], what = "both")
#this adds the num_graphs_in variable from composite_tbl
new_tbl$vertices <- dplyr::left_join(new_tbl$vertices, composite_tbl)
#turn back into a network
new_net <- network::network(x=new_tbl$edges[,1:2], directed = T,
                            hyper = F, loops = T, multiple = T, 
                            bipartiate = F, vertices = new_tbl$vertices,
                            matrix.type = "edgelist")
#we need a matrix version for some node statistics
set.seed(50000)
new_mat <- as.matrix(as.matrix(export_to_network(new_extract_clean, "igraph", 
                                                 keep_isolates = F, collapse_edges = T, self_loops = F)[[1]]))





paths2 <- diag(old_mat %*% old_mat)
recip <- 2*paths2 / sna::degree(old_net)
totalCC <- as.vector(unname(DirectedClustering::ClustF(old_mat, 
                                                       type = "directed", isolates="zero")$totalCC))
closens <- sna::closeness(old_net, gmode = "graph", cmode="suminvundir")
between <- sna::betweenness(old_net,gmode = "graph",cmode="undirected")
deg <- sna::degree(old_net, gmode = "graph", cmode = "undirected")
old_node_df <- dplyr::tibble(name = network::get.vertex.attribute(old_net, 
                                                                  "vertex.names"), 
                             closens, 
                             between, 
                             deg,
                             recip,
                             totalCC,
                             entity_type = network::get.vertex.attribute(old_net,"entity_type"),
                             num_graphs_in = network::get.vertex.attribute(old_net, "num_graphs_in"))


paths2 <- diag(new_mat %*% new_mat)
recip <- 2*paths2 / sna::degree(new_net)
totalCC <- as.vector(unname(DirectedClustering::ClustF(new_mat, 
                                                       type = "directed", isolates="zero")$totalCC))
closens <- sna::closeness(new_net, gmode = "graph", cmode="suminvundir")
between <- sna::betweenness(new_net,gmode = "graph",cmode="undirected")
deg <- sna::degree(new_net, gmode = "graph", cmode = "undirected")
new_node_df <- dplyr::tibble(name = network::get.vertex.attribute(new_net, 
                                                                  "vertex.names"), 
                             closens, 
                             between, 
                             deg,
                             recip,
                             totalCC,
                             entity_type = network::get.vertex.attribute(new_net,"entity_type"),
                             num_graphs_in = network::get.vertex.attribute(new_net, "num_graphs_in"))

summary(old_node_df)
summary(new_node_df)






old_node_df$plan_version <- "old"
new_node_df$plan_version <- "new"
combineddf <- rbind(old_node_df, new_node_df)
with(combineddf,table(plan_version,num_graphs_in))





library(gridExtra)
library(ggplot2)
b1 <- ggplot(old_node_df, aes(x = entity_type, y = deg)) + geom_boxplot() + 
  theme_bw() + labs(title="Old Network")
b2 <- ggplot(new_node_df, aes(x = entity_type, y = deg)) + geom_boxplot() + 
  theme_bw() + labs(title="New Network")
b3 <- ggplot(old_node_df, aes(x = entity_type, y = log(between+0.01))) + 
  geom_boxplot() + theme_bw() + labs(title="Old Network")
b4 <- ggplot(new_node_df, aes(x = entity_type, y = log(between+0.01))) + 
  geom_boxplot() + theme_bw() + labs(title="New Network")

grid.arrange(b1, b2, b3, b4, ncol=2)



