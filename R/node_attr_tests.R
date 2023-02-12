library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(Matrix)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

#gspids <- c("0124","0061","0047")
list <- c(99,33,45)

#anova: eigenvector centralization ~  org_type 
#for(m in 1:length(gspids)){
for(m in list){
  centr_df <- readRDS(paste0("data/node_attr_",gspids[m]))
 ##SUBSETTING FOR POL 279 ASSIGNMENT 2 
  centr_df <- centr_df %>% filter(type %in% c("ORG","GPE","PERSON"))
  ggplot(centr_df, aes(x=type, y=eigenvector)) + 
    geom_boxplot()
  
  one_way_anova <- aov(eigenvector ~ as.factor(type), data = centr_df)
  summary(one_way_anova)
  one_way_anova$coefficients
  TukeyHSD(one_way_anova)
}

#calculate transitivity
for(m in list){
  print(paste0("GSP ID ",gspids[m]))
  
  adj_mat <- readRDS(paste0("data/adjmat_orig_",gspids[m]))
  adj_mat <- as(adj_mat, "sparseMatrix")
  
  binary_mat <- as.matrix((adj_mat>0)+0)
  binary_mat <- as(binary_mat, "sparseMatrix")

  diag(binary_mat) <- 0
  
  t_binary_mat <- t(binary_mat)
  binary_sq <- binary_mat %*% t_binary_mat
  saveRDS(binary_sq, paste0("data/binarymat_sq_",gspids[m]))
  
  triangles <- binary_mat * binary_sq
  saveRDS(triangles, paste0("data/triangles_",gspids[m]))
  node_degree <- colSums(binary_mat)
  node_triangles <- colSums(triangles)
  temp <- tibble(node_degree, node_triangles)
  temp <- temp %>% mutate(node_transitivity = node_triangles/(node_degree*(node_degree-1)))
  temp$name <- rownames(binary_mat)
  
  centr_df <- readRDS(paste0("data/node_attr_",gspids[m]))
  
  centr_df <- full_join(centr_df, temp, by = c("name"))
  
  saveRDS(centr_df, paste0("data/node_attr_tr_",gspids[m]))
  
}

#boxplot: org_type and transitivity
#correlation: transitivity ~ eig centrality
#correlation: transitivity ~ betw centrality
#anova: transitivity ~ type
for(m in list){
  print(paste0("GSP ID ",gspids[m]))
  
  centr_df <- readRDS(paste0("data/node_attr_tr_",gspids[m]))
  ##SUBSETTING FOR POL 279 ASSIGNMENT 2 
  centr_df <- centr_df %>% filter(type %in% c("ORG","GPE","PERSON"))
  ggplot(centr_df, aes(x=type, y=node_transitivity)) + 
    geom_boxplot()
  
  print(cor.test(centr_df$eigenvector, centr_df$node_transitivity, 
                 method = "pearson"))
  plot(centr_df$eigenvector, centr_df$node_transitivity)
  
  print(cor.test(centr_df$between, centr_df$node_transitivity, 
                 method = "pearson"))
  plot(centr_df$between, centr_df$node_transitivity)
  
  one_way_anova <- aov(node_transitivity ~ as.factor(type), data = centr_df)
  print(summary(one_way_anova))
  print(one_way_anova$coefficients)
  print(TukeyHSD(one_way_anova))
}




