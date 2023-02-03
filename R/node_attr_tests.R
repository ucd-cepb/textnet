library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)

gspids <- c("0124","0061","0047")

for(m in 1:length(gspids)){
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






