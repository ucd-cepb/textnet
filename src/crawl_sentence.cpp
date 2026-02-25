#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List crawl_sentence_cpp(CharacterVector pos,
                        CharacterVector dep_rel,
                        IntegerVector head_token_id,
                        CharacterVector lemma,
                        CharacterVector token,
                        IntegerVector token_id,
                        CharacterVector tag) {

  int n = pos.size();

  // Output character vectors (8 scalar-per-token fields)
  CharacterVector source_or_target(n, "");
  CharacterVector head_verb_id(n, "");
  CharacterVector head_verb_name(n, "");
  CharacterVector head_verb_lemma(n, "");
  CharacterVector head_verb_tense(n, "");
  CharacterVector head_verb_dep_rel(n, "");
  CharacterVector x_parent_verb_id(n, "");
  CharacterVector parent_verb_id(n, "");

  // Output list columns (5 list-of-character-vector fields)
  List helper_lemma(n);
  List helper_token(n);
  List xcomp_verb(n);
  List xcomp_helper_lemma(n);
  List xcomp_helper_token(n);

  // Initialize list elements to empty CharacterVectors
  for (int i = 0; i < n; i++) {
    helper_lemma[i] = CharacterVector::create();
    helper_token[i] = CharacterVector::create();
    xcomp_verb[i] = CharacterVector::create();
    xcomp_helper_lemma[i] = CharacterVector::create();
    xcomp_helper_token[i] = CharacterVector::create();
  }

  // --- Pre-tag AUX helpers and xcomp verbs ---
  // Mirrors: ifelse(pos=="AUX" & dep_rel %in% c("aux","auxpass"), "aux", "")
  // and:     ifelse(pos=="VERB" & dep_rel=="xcomp", "xcomp", "")
  // We store these tags in helper_lemma[i] and xcomp_verb[i] as first elements
  std::vector<bool> is_aux(n, false);
  std::vector<bool> is_xcomp(n, false);

  for (int i = 0; i < n; i++) {
    String p = pos[i];
    String dr = dep_rel[i];
    if (p == "AUX" && (dr == "aux" || dr == "auxpass")) {
      is_aux[i] = true;
      helper_lemma[i] = CharacterVector::create("aux");
      helper_token[i] = CharacterVector::create("aux");
    }
    if (p == "VERB" && dr == "xcomp") {
      is_xcomp[i] = true;
      xcomp_verb[i] = CharacterVector::create("xcomp");
    }
  }

  // --- For loop: process each token ---
  for (int tok_num = 0; tok_num < n; tok_num++) {

    // head_token_id is 1-based from R; convert to 0-based
    int head_idx = head_token_id[tok_num] - 1;

    // --- AUX helper appending ---
    if (is_aux[tok_num] && head_idx >= 0 && head_idx < n) {
      CharacterVector hl = helper_lemma[head_idx];
      hl.push_back(lemma[tok_num]);
      helper_lemma[head_idx] = hl;

      CharacterVector ht = helper_token[head_idx];
      ht.push_back(token[tok_num]);
      helper_token[head_idx] = ht;
    }

    // --- xcomp verb appending ---
    if (is_xcomp[tok_num]) {
      int xcounter = 1;
      int parent_tkn = tok_num;
      // Follow xcomp chain up (max 4 hops)
      int parent_head = head_token_id[parent_tkn] - 1;
      while (parent_head >= 0 && parent_head < n) {
        CharacterVector xv = xcomp_verb[parent_head];
        if (xv.size() > 0 && std::string(xv[0]) == "xcomp" && xcounter < 5) {
          xcounter++;
          parent_tkn = parent_head;
          parent_head = head_token_id[parent_tkn] - 1;
        } else {
          break;
        }
      }
      // Append this token's lemma to the parent verb's xcomp_verb list
      int final_head = head_token_id[parent_tkn] - 1;
      if (final_head >= 0 && final_head < n) {
        CharacterVector xv = xcomp_verb[final_head];
        xv.push_back(lemma[tok_num]);
        xcomp_verb[final_head] = xv;
        // Set x_parent_verb_id (1-based)
        x_parent_verb_id[tok_num] = std::to_string(final_head + 1);
      }
    }

    // --- Main classification: source_or_target ---
    int initial_token_idx = tok_num;
    int current_token_idx = initial_token_idx;
    int head_tok_idx = head_token_id[tok_num] - 1;
    source_or_target[tok_num] = NA_STRING;
    int break_while_counter = 0;

    while (CharacterVector::is_na(source_or_target[tok_num]) && break_while_counter < 15) {
      String cur_dep = dep_rel[current_token_idx];
      String cur_pos = pos[current_token_idx];

      if (cur_dep == "appos") {
        source_or_target[tok_num] = "appos";
      } else if (cur_pos == "VERB" || cur_pos == "AUX") {
        source_or_target[tok_num] = "target";
      } else if (cur_dep == "nsubj" || cur_dep == "nsubjpass" ||
                 cur_dep == "csubj" || cur_dep == "csubjpass" ||
                 cur_dep == "agent" || cur_dep == "expl") {
        source_or_target[tok_num] = "source";
      } else if (cur_dep == "ROOT") {
        source_or_target[tok_num] = "root_not_verb";
      } else if (head_tok_idx == current_token_idx) {
        source_or_target[tok_num] = "broken_dep_rel";
      } else if (head_tok_idx == initial_token_idx) {
        source_or_target[tok_num] = "inf_loop";
      } else {
        // Continue following the chain
        current_token_idx = head_tok_idx;
        if (current_token_idx >= 0 && current_token_idx < n) {
          head_tok_idx = head_token_id[current_token_idx] - 1;
        }
      }
      break_while_counter++;
    }

    // --- Fill head verb info based on source_or_target ---
    if (!CharacterVector::is_na(source_or_target[tok_num])) {
      String sot = source_or_target[tok_num];

      if (sot == "target") {
        head_verb_id[tok_num] = std::to_string(token_id[current_token_idx]);
        head_verb_name[tok_num] = token[current_token_idx];
        head_verb_lemma[tok_num] = lemma[current_token_idx];
        head_verb_tense[tok_num] = tag[current_token_idx];
        head_verb_dep_rel[tok_num] = dep_rel[current_token_idx];
        parent_verb_id[tok_num] = std::to_string(head_tok_idx + 1);

      } else if (sot == "source") {
        // Follow chain to find first verb
        bool current_token_is_verb = false;
        int source_while_counter = 0;
        while (!current_token_is_verb && source_while_counter < 10) {
          String cp = pos[current_token_idx];
          current_token_is_verb = (cp == "VERB" || cp == "AUX");
          if (!current_token_is_verb) {
            current_token_idx = head_tok_idx;
            if (current_token_idx >= 0 && current_token_idx < n) {
              head_tok_idx = head_token_id[current_token_idx] - 1;
            }
          }
          source_while_counter++;
        }
        if (current_token_is_verb) {
          head_verb_id[tok_num] = std::to_string(token_id[current_token_idx]);
          head_verb_name[tok_num] = token[current_token_idx];
          head_verb_lemma[tok_num] = lemma[current_token_idx];
          head_verb_tense[tok_num] = tag[current_token_idx];
          head_verb_dep_rel[tok_num] = dep_rel[current_token_idx];
          parent_verb_id[tok_num] = std::to_string(head_tok_idx + 1);
        } else {
          source_or_target[tok_num] = "source";
          head_verb_id[tok_num] = NA_STRING;
          head_verb_name[tok_num] = NA_STRING;
          head_verb_lemma[tok_num] = NA_STRING;
          head_verb_tense[tok_num] = NA_STRING;
          head_verb_dep_rel[tok_num] = NA_STRING;
          parent_verb_id[tok_num] = NA_STRING;
        }
      }
    }
  } // end for tok_num

  // --- Post-loop: tag x_parent_verb_id groups ---
  // For each token with a non-empty x_parent_verb_id, find its head_verb_id,
  // then propagate x_parent_verb_id to all tokens sharing that head_verb_id
  std::vector<int> xparent_rows;
  std::vector<std::string> xhead_verb;
  for (int i = 0; i < n; i++) {
    std::string xpv = as<std::string>(x_parent_verb_id[i]);
    if (xpv.size() > 0) {
      xparent_rows.push_back(i);
      xhead_verb.push_back(as<std::string>(head_verb_id[i]));
    }
  }

  for (int w = 0; w < n; w++) {
    std::string hvi = as<std::string>(head_verb_id[w]);
    bool found = false;
    for (size_t k = 0; k < xhead_verb.size(); k++) {
      if (hvi == xhead_verb[k]) {
        x_parent_verb_id[w] = x_parent_verb_id[xparent_rows[k]];
        found = true;
        break;
      }
    }
    if (!found) {
      x_parent_verb_id[w] = head_verb_id[w];
    }
  }

  // --- Move xcomp helper_lemma/helper_token to parent verb's xcomp_helper fields ---
  for (int i = 0; i < n; i++) {
    if (is_xcomp[i]) {
      std::string xpv = as<std::string>(x_parent_verb_id[i]);
      if (xpv.size() > 0) {
        int headverb_idx = std::stoi(xpv) - 1;
        if (headverb_idx >= 0 && headverb_idx < n) {
          // Append helper_lemma of xcomp to parent's xcomp_helper_lemma
          CharacterVector parent_xhl = xcomp_helper_lemma[headverb_idx];
          CharacterVector child_hl = helper_lemma[i];
          for (int j = 0; j < child_hl.size(); j++) {
            if (!CharacterVector::is_na(child_hl[j])) {
              parent_xhl.push_back(child_hl[j]);
            }
          }
          xcomp_helper_lemma[headverb_idx] = parent_xhl;

          // Append helper_token of xcomp to parent's xcomp_helper_token
          CharacterVector parent_xht = xcomp_helper_token[headverb_idx];
          CharacterVector child_ht = helper_token[i];
          for (int j = 0; j < child_ht.size(); j++) {
            if (!CharacterVector::is_na(child_ht[j])) {
              parent_xht.push_back(child_ht[j]);
            }
          }
          xcomp_helper_token[headverb_idx] = parent_xht;
        }
      }
    }
  }

  // --- Update xcomp targets to parent verb attributes ---
  for (int i = 0; i < n; i++) {
    std::string hvdr = as<std::string>(head_verb_dep_rel[i]);
    if (hvdr == "xcomp") {
      std::string xpv = as<std::string>(x_parent_verb_id[i]);
      if (xpv.size() > 0) {
        int parent_idx = std::stoi(xpv) - 1;
        if (parent_idx >= 0 && parent_idx < n) {
          head_verb_id[i] = head_verb_id[parent_idx];
          head_verb_name[i] = head_verb_name[parent_idx];
          head_verb_lemma[i] = head_verb_lemma[parent_idx];
          head_verb_tense[i] = head_verb_tense[parent_idx];
          parent_verb_id[i] = parent_verb_id[parent_idx];
          helper_lemma[i] = helper_lemma[parent_idx];
          helper_token[i] = helper_token[parent_idx];
          xcomp_helper_lemma[i] = xcomp_helper_lemma[parent_idx];
          xcomp_helper_token[i] = xcomp_helper_token[parent_idx];
        }
      }
    }
  }

  return List::create(
    Named("source_or_target") = source_or_target,
    Named("head_verb_id") = head_verb_id,
    Named("head_verb_name") = head_verb_name,
    Named("head_verb_lemma") = head_verb_lemma,
    Named("head_verb_tense") = head_verb_tense,
    Named("head_verb_dep_rel") = head_verb_dep_rel,
    Named("x_parent_verb_id") = x_parent_verb_id,
    Named("parent_verb_id") = parent_verb_id,
    Named("helper_lemma") = helper_lemma,
    Named("helper_token") = helper_token,
    Named("xcomp_verb") = xcomp_verb,
    Named("xcomp_helper_lemma") = xcomp_helper_lemma,
    Named("xcomp_helper_token") = xcomp_helper_token
  );
}
