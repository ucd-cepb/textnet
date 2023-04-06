library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(GGally)

gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)

for(m in 1:length(gspids)){
  agency_net <- readRDS(paste0("data/network_maincomponents_",gspids[m]))
  
  gov_net <- ggnet2(agency_net, node.size=3, node.color = "type",
                    edge.size = 0.2,
                    singletons=F,
                    edge.color="grey")
  ggsave(paste0("network_maincomponents_",gspids[m],".png"), plot = gov_net, device = "png",
         path = "figures", width = 4020, height = 1890, dpi = 300,
         units = "px", bg = "white")
  
}
