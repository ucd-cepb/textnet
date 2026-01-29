### package level settings ###
.datatable.aware <- TRUE

# Suppress R CMD check NOTEs for NSE column references (dplyr, data.table, etc.)
utils::globalVariables(c(
  ".", "..attr", "N", "acronym", "avg_fract_of_a_doc", "dep_rel", "doc_id",
  "doc_sent", "doc_sent_head_tok", "doc_sent_parent", "doc_sent_verb",
  "edgeiscomplete", "eng_words", "entity_id", "entity_name", "entity_type",
  "fraction_of_doc", "has_sources", "hascompleteedge", "head", "head_token_id",
  "head_verb_id", "head_verb_lemma", "head_verb_name", "head_verb_tense",
  "helper_lemma", "helper_token", "keep", "lemma", "name", "neg",
  "num_appearances", "num_graphs_in", "num_mentions", "parent_verb_id",
  "pos", "row_id", "sentence_id", "source_or_target", "tag", "target",
  "token", "token_id", "verb_classifications", "xcomp_helper_lemma",
  "xcomp_helper_token", "xcomp_verb"
))