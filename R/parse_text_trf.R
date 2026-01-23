# Exported function
#parse_text_trf

#' Parse text using spaCy transformer model with GPU support
#'
#' This function is specifically designed for the en_core_web_trf transformer model
#' with GPU acceleration. For CPU-only processing with en_core_web_lg, use parse_text().
#'
#' @param ret_path filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.
#' @param keep_hyph_together Set to true to replace hyphens within a single word with underscores. Defaults to false.
#' @param phrases_to_concatenate character vector of phrases, in which each element is a string consisting of tokens separated by spaces. These are replaced with their concatenated version in order, from left to right. It is suggested that the most specific phrases, with the most words, are arranged at the left.
#' @param concatenator This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.
#' @param text_list This is a named list, an object of the type resulting from pdf_clean, in which each list element is a document, and each string within a list element represents the text on one page
#' @param parsed_filenames This is a character vector in which each element represents a filepath associated with its respective document.
#' The parsed data will be exported to these files.
#' @param overwrite A boolean. Whether to overwrite existing files
#' @param test A boolean. If TRUE and overwrite is FALSE, will still run parsing but won't save results. Useful for testing parsing on a subset without affecting saved files.
#' @param custom_entities A named list. This does not overwrite the entity determination of the NLP engine, but rather catches user-defined entities that are not otherwise detected by the engine. Best used in combination with phrases_to_concatenate, since the custom entity label will only be applied if the entire token matches the definition. Does not search multiple consecutive tokens to define a match. These will be applied to all documents.
#' @param entity_ruler_patterns A list of pattern dictionaries for spaCy's EntityRuler. Each pattern should be a named list with 'label' and 'pattern' elements. The 'pattern' can be a string for exact matches or a list of token attributes for complex patterns. If provided, these patterns will be added to spaCy's NLP pipeline. Use entity_specify() to create patterns from a dictionary.
#' @param ruler_position A string controlling where EntityRuler is placed in the pipeline: "after" (default) places it after NER so custom patterns can override, "before" places it before NER so NER has final say.
#' @param overwrite_ents A boolean. If TRUE (default), EntityRuler patterns override NER's entity assignments for overlapping spans. If FALSE, EntityRuler only fills gaps where NER found nothing.
#' @return A data.frame of tokens with columns: doc_id, sentence_id, token_id, token, lemma, pos, tag, head_token_id, dep_rel, entity.
#'   Format is compatible with spacyr::spacy_parse output.
#' @importFrom reticulate py_run_string py_eval py_config
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stringi stri_replace_all_regex stri_escape_unicode
#' @importFrom pbapply pblapply
#' @importFrom utils data
#' @export

parse_text_trf <- function(ret_path, keep_hyph_together=F, phrases_to_concatenate=NA,
                           concatenator="_", text_list, parsed_filenames,
                           overwrite=T, test=F, custom_entities = NULL, entity_ruler_patterns = NULL,
                           ruler_position = c("after", "before"), overwrite_ents = TRUE){
  if(!requireNamespace("reticulate", quietly = T)){
    stop("Package 'reticulate' must be installed to use this function.",
         call.=F)
  }

  # Input validation
  if(!is.character(ret_path) || length(ret_path) != 1) {
    stop("'ret_path' must be a single character string")
  }

  if(!is.logical(keep_hyph_together) || length(keep_hyph_together) != 1) {
    stop("'keep_hyph_together' must be a single logical value")
  }

  if(!is.character(phrases_to_concatenate) && !is.na(phrases_to_concatenate)) {
    stop("'phrases_to_concatenate' must be either NA or a character vector")
  }

  if(!is.character(concatenator) || length(concatenator) != 1) {
    stop("'concatenator' must be a single character string")
  }

  if(!is.list(text_list)) {
    stop("'text_list' must be a list")
  }

  if(!is.character(parsed_filenames)) {
    stop("'parsed_filenames' must be a character vector")
  }

  if(!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be a single logical value")
  }

  if(!is.logical(test) || length(test) != 1) {
    stop("'test' must be a single logical value")
  }

  ruler_position <- match.arg(ruler_position)

  if(!is.logical(overwrite_ents) || length(overwrite_ents) != 1) {
    stop("'overwrite_ents' must be a single logical value")
  }

  if(!is.null(custom_entities)){
    if(!is.list(custom_entities) | is.null(names(custom_entities)) |
       "" %in% names(custom_entities)){
      stop("custom_entities must be a named list.")
    }
  }

  if(!is.null(entity_ruler_patterns)){
    if(!is.list(entity_ruler_patterns)){
      stop("entity_ruler_patterns must be a list.")
    }
    for(i in 1:length(entity_ruler_patterns)){
      if(!is.list(entity_ruler_patterns[[i]]) ||
         !all(c("label", "pattern") %in% names(entity_ruler_patterns[[i]]))){
        stop("Each element in entity_ruler_patterns must be a list with 'label' and 'pattern' elements.")
      }
    }
  }

  # Fix OpenMP library conflict (common on macOS with conda)
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Prevent OpenMP thread conflicts between R packages (e.g., data.table) and PyTorch
  Sys.setenv(OMP_NUM_THREADS = "1")
  message("Note: Setting OMP_NUM_THREADS=1 to prevent OpenMP conflicts with transformer model.")

  # Set CUDA_PATH for cupy kernel compilation (if CUDA_HOME is set but CUDA_PATH isn't)
  if(Sys.getenv("CUDA_PATH") == "" && Sys.getenv("CUDA_HOME") != ""){
    Sys.setenv(CUDA_PATH = Sys.getenv("CUDA_HOME"))
    message(paste0("Set CUDA_PATH from CUDA_HOME: ", Sys.getenv("CUDA_PATH")))
  } else if(Sys.getenv("CUDA_PATH") != ""){
    message(paste0("CUDA_PATH already set: ", Sys.getenv("CUDA_PATH")))
  } else {
    warning("CUDA_PATH and CUDA_HOME are not set - GPU may not work correctly")
  }

  # Set up Python environment
  Sys.setenv(RETICULATE_PYTHON=ret_path)
  reticulate::py_config()

  # Initialize GPU for spaCy transformer model
  # Since CUDA_PATH must be set before cupy is imported, we set it in Python too
  gpu_init_success <- tryCatch({
    reticulate::py_run_string("
import os

# Ensure CUDA_PATH is set for cupy kernel compilation
cuda_path = os.environ.get('CUDA_PATH') or os.environ.get('CUDA_HOME')
if cuda_path:
    os.environ['CUDA_PATH'] = cuda_path
    print(f'Python CUDA_PATH: {cuda_path}')
else:
    print('WARNING: No CUDA_PATH or CUDA_HOME found')

# Import and verify cupy FIRST
import cupy
device_count = cupy.cuda.runtime.getDeviceCount()
assert device_count > 0, 'No GPU detected by cupy'
cupy.cuda.Device(0).use()
print(f'cupy verified: {device_count} GPU(s) detected')

# Import thinc and set up GPU ops BEFORE importing spacy
from thinc.api import set_gpu_allocator, require_gpu
from thinc.backends import set_current_ops
from thinc.backends.cupy_ops import CupyOps

# Set the memory allocator for cupy
set_gpu_allocator('pytorch')

# Create and set CupyOps as the current ops
cupy_ops = CupyOps()
set_current_ops(cupy_ops)

# Verify ops
from thinc.api import get_current_ops
ops = get_current_ops()
print(f'Ops type: {type(ops).__name__}')
print(f'Ops xp: {ops.xp}')

# Now import spacy - it will pick up the already-configured ops
import spacy

# Use require_gpu to ensure GPU is used (more strict than prefer_gpu)
require_gpu(0)
print('GPU required successfully')

# Load the transformer model
print('Loading en_core_web_trf model...')
nlp = spacy.load('en_core_web_trf')
print('Model loaded successfully!')
")
    TRUE
  }, error = function(e){
    stop(paste0("GPU initialization failed: ", e$message,
                "\n\nThis function requires GPU. For CPU processing, use parse_text() instead."))
  })

  message("spaCy transformer model loaded with GPU")

  # Define Python parsing function (replaces spacyr::spacy_parse)
  reticulate::py_run_string("
def parse_texts(texts, doc_ids):
    '''Parse texts using the loaded nlp model, returning spacyr-compatible format'''
    import pandas as pd

    all_tokens = []
    all_nounphrases = []

    for text, doc_id in zip(texts, doc_ids):
        if not text or not text.strip():
            continue

        doc = nlp(text)

        # Process by sentence to get sentence-relative token IDs (spacyr format)
        for sent_i, sent in enumerate(doc.sents):
            sent_start = sent.start  # document-level offset of sentence start

            for token in sent:
                ent_tag = ''
                if token.ent_type_:
                    ent_tag = token.ent_type_ + '_' + token.ent_iob_

                # Sentence-relative token_id (1-indexed, resets each sentence)
                token_id_rel = token.i - sent_start + 1

                # Head token_id is also sentence-relative
                # If head is outside sentence (e.g., ROOT pointing to itself), handle it
                if token.head.i >= sent.start and token.head.i < sent.end:
                    head_token_id_rel = token.head.i - sent_start + 1
                else:
                    # ROOT case: head points to itself
                    head_token_id_rel = token_id_rel

                all_tokens.append({
                    'doc_id': doc_id,
                    'sentence_id': sent_i + 1,
                    'token_id': token_id_rel,
                    'token': token.text,
                    'lemma': token.lemma_,
                    'pos': token.pos_,
                    'tag': token.tag_,
                    'head_token_id': head_token_id_rel,
                    'dep_rel': token.dep_,
                    'entity': ent_tag
                })

            # Extract noun phrases for this sentence
            for chunk in doc.noun_chunks:
                if chunk.start >= sent.start and chunk.start < sent.end:
                    all_nounphrases.append({
                        'doc_id': doc_id,
                        'sentence_id': sent_i + 1,
                        'nounphrase': chunk.text,
                        'root': chunk.root.text
                    })

    tokens_df = pd.DataFrame(all_tokens)
    nounphrases_df = pd.DataFrame(all_nounphrases) if all_nounphrases else pd.DataFrame()

    return {'tokens': tokens_df, 'nounphrases': nounphrases_df}
")

  # Configure EntityRuler if patterns are provided
  if(!is.null(entity_ruler_patterns)){
    tryCatch({
      nlp <- reticulate::py_eval("nlp")

      if(ruler_position == "after"){
        position_arg <- list(after = "ner")
      } else {
        position_arg <- list(before = "ner")
      }

      ruler_config <- list(overwrite_ents = overwrite_ents)

      ruler <- do.call(nlp$add_pipe,
                       c(list("entity_ruler"),
                         position_arg,
                         list(config = ruler_config)))
      ruler$add_patterns(entity_ruler_patterns)

      # Verify EntityRuler is in pipeline
      pipe_names <- nlp$pipe_names
      if(!"entity_ruler" %in% pipe_names){
        warning("EntityRuler may not have been added to pipeline correctly")
      }

      message(paste0("EntityRuler configured: position=", ruler_position,
                     ", overwrite_ents=", overwrite_ents,
                     ", ", length(entity_ruler_patterns), " patterns loaded",
                     "\nPipeline: ", paste(pipe_names, collapse=" -> ")))

    }, error = function(e){
      warning(paste0("Failed to configure EntityRuler: ", e$message, ". Proceeding without EntityRuler."))
    })
  }


  # Process text
  pages <- unlist(text_list)

  if(is.null(names(text_list)) | "" %in% names(text_list)){
    stop("text_list must be a named list")
  }
  file_ids <- unlist(sapply(1:length(text_list), function(q) rep(names(text_list[q]),length(text_list[[q]]))))

  phrases_to_concatenate <- phrases_to_concatenate[stringr::str_detect(phrases_to_concatenate,"\\s")]

  if(length(phrases_to_concatenate) > 1 || !is.na(phrases_to_concatenate)){
    phrases_grouped <- gsub("\\s+", concatenator, x = phrases_to_concatenate)
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern = phrases_to_concatenate,
                                      replacement = phrases_grouped,
                                      vectorize= F)
    })
  }

  if(keep_hyph_together){
    pages <- pbapply::pblapply(1:length(pages), function(i){
      stringi::stri_replace_all_regex(pages[i], pattern= "(?<=\\w)[\\-\\u2013](?=\\w)", replacement ="_", vectorize=F)
    })
  }

  unique_files <- base::unique(file_ids)
  all_parsed <- vector(mode="list",length=length(unique_files))

  for (m in 1:length(unique_files)){
    file_exists <- file.exists(parsed_filenames[m])
    should_parse <- overwrite || !file_exists || test
    should_save <- overwrite || !file_exists

    if(should_parse){
      single_plan_text <- unlist(pages[file_ids==unique_files[m]])

      # Create doc_ids for each text element (text1, text2, etc.)
      text_doc_ids <- paste0("text", seq_along(single_plan_text))

      # Call Python parsing function directly (bypasses spacyr)
      parse_fn <- reticulate::py_eval("parse_texts")
      result <- parse_fn(single_plan_text, text_doc_ids)

      parsedtxt <- as.data.frame(result$tokens)
      nounphrases <- as.data.frame(result$nounphrases)

      # Attach nounphrases as attribute (matches spacyr behavior for nounphrase_extract())
      attr(parsedtxt, "nounphrases") <- nounphrases

      lettertokens <- parsedtxt$token[stringr::str_detect(parsedtxt$token, "[a-zA-Z]")]
      lettertokensunicodeescaped <- stringi::stri_escape_unicode(lettertokens)
      utils::data(eng_words)
      pctlettersineng <- sum(lettertokensunicodeescaped %in% eng_words)/length(lettertokensunicodeescaped)

      if(pctlettersineng<0.5){
        warning(paste0("Fewer than 50% of letter-containing tokens in the document ", unique_files[m] ," are English words."))
      }

      if(test && file_exists && !overwrite){
        print(paste0("parsing complete (test mode, not saved): ",unique_files[m]))
      } else {
        print(paste0("parsing complete: ",unique_files[m]))
      }

      all_parsed[[m]] <- parsedtxt

      if(!is.null(custom_entities)){
        for(k in 1:length(custom_entities)){
          custom_entities[[k]] <- stringr::str_replace_all(custom_entities[[k]] ,"\\s",concatenator)
          if(length(all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity)>0){
            all_parsed[[m]][all_parsed[[m]]$token %in% custom_entities[[k]] & all_parsed[[m]]$entity=="",]$entity <- paste0(names(custom_entities[k]), "_B")
          }
        }
      }

      if(should_save){
        saveRDS(all_parsed[[m]], parsed_filenames[m])
      }
    } else {
      print(paste0("Skipping parsed_filenames[",m,"] - file already exists (set overwrite=T to reparse or test=T to test without saving)"))
      all_parsed[[m]] <- NULL
    }
  }

  return(all_parsed)
}
