library(testthat)

# Locate Python with spaCy. We prefer the conda spacy-env; fall back to
# any system Python that has spaCy. Skip gracefully if nothing is found.
.find_spacy_python <- function() {
  # Common conda locations for spacy-env
  candidates <- c(
    "/opt/miniconda3/envs/spacy-env/bin/python",
    "/opt/anaconda3/envs/spacy-env/bin/python",
    file.path(Sys.getenv("HOME"), "miniconda3/envs/spacy-env/bin/python"),
    file.path(Sys.getenv("HOME"), "anaconda3/envs/spacy-env/bin/python")
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  NULL
}

python_path <- .find_spacy_python()
if (is.null(python_path)) skip("spacy-env conda environment not found")

protocol <- read.table(
  testthat::test_path("testdata", "sentences_that_work.txt"),
  sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE
)

# Parse custom_entities column into R character vectors
get_custom <- function(arg_str) {
  if (is.na(arg_str) || !grepl("custom_entities", arg_str)) return(NULL)
  m <- regmatches(arg_str, gregexpr('"[^"]+"', arg_str))[[1]]
  gsub('"', "", m)
}

for (i in seq_len(nrow(protocol))) {
  local({
    row <- protocol[i, ]
    idx <- i
    test_that(paste0("[", idx, "] ", row$Test.Purpose), {
      ce     <- get_custom(row$textNet..parse_text.Arguments)
      result <- run_sentence(row$Sentence, python_path = python_path,
                             custom_entities = ce)

      expected_edges <- parse_expected_output(row$Expected.Output)
      for (e in expected_edges) {
        expect_true(
          has_edge(result$edgelist, e$source, e$verb, e$target),
          label = paste0("edge not found: ", e$source,
                         " -", e$verb, "-> ", e$target)
        )
      }
    })
  })
}
