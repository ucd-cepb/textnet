library(dplyr)
gsp_text_with_meta <- readRDS("data/prepped_for_sna")
gsp_planonly <- gsp_text_with_meta[is_comment==FALSE & is_reference==FALSE]
nrow(gsp_planonly) #= number of pages

gspids <- unique(gsp_planonly$gsp_id)
length(gspids) # = number of plans

all_parsed <- list.files(path = "data", pattern = "parsed", full.names = T)

all_parsed <- all_parsed[1:114]
num_tokens <- 0
num_sentences <- 0
for(i in 1:length(all_parsed)){
  parsed_i <- readRDS(all_parsed[i])
  num_sentences <- num_sentences+ nrow(parsed_i %>% dplyr::count(doc_id, sentence_id))
  num_tokens <- num_tokens + nrow(parsed_i)
}

num_sentences
num_tokens # = number of tokens