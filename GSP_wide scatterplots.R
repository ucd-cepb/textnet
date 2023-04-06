library(tidyverse)
library(ggplot2)
library(vegan)

gov_net <- as.data.frame(lapply(gov_net, as.numeric))

topic_net <- as.data.frame(lapply(topic_net, as.numeric))


full_net <- full_join(gov_net, topic_net, by = "gsp_id")

#topic_prevalences <- readRDS("data/topic_prevalence")



#x
ggplot(full_net, aes(x = density.x, y = centralization.x)) + geom_point()#pos
ggplot(full_net, aes(x = connectedness.x, y = centralization.x)) + geom_point()#pos
ggplot(full_net, aes(x = transitivity.x, y = centralization.x)) + geom_point()#neg?
ggplot(full_net, aes(x = connectedness.x, y = transitivity.x)) + geom_point()#neg
ggplot(full_net, aes(x = num_communities.x, y = transitivity.x)) + geom_point()#neg?

#xy
ggplot(full_net, aes(x = centralization.x, y = transitivity.y)) + geom_point()#?
ggplot(full_net, aes(x = centralization.x, y = connectedness.y)) + geom_point()#?
ggplot(full_net, aes(x = centralization.x, y = centralization.y)) + geom_point()#?
ggplot(full_net, aes(x = connectedness.x, y = transitivity.y)) + geom_point()#?
ggplot(full_net, aes(x = connectedness.x, y = connectedness.y)) + geom_point()#?
ggplot(full_net, aes(x = connectedness.x, y = centralization.y)) + geom_point()#?
ggplot(full_net, aes(x = transitivity.x, y = transitivity.y)) + geom_point()#?
ggplot(full_net, aes(x = transitivity.x, y = connectedness.y)) + geom_point()#?
ggplot(full_net, aes(x = transitivity.x, y = centralization.y)) + geom_point()#?

#y
ggplot(full_net, aes(x = density.y, y = centralization.y)) + geom_point()#pos?
ggplot(full_net, aes(x = connectedness.y, y = centralization.y)) + geom_point()#?
ggplot(full_net, aes(x = transitivity.y, y = centralization.y)) + geom_point()#?
ggplot(full_net, aes(x = connectedness.y, y = transitivity.y)) + geom_point()#?
ggplot(full_net, aes(x = num_communities.y, y = transitivity.y)) + geom_point()#neg?





#shannon_div <- diversity(topic_prevalences, index="shannon")

