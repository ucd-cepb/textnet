#' Build structural patterns for government organization names
#'
#' Creates spaCy EntityRuler patterns that match the grammatical shape of
#' government organization names (e.g., "California Department of Fish &
#' Wildlife") rather than enumerating exact strings. Designed to complement
#' [entity_specify()], which matches exact dictionary entries.
#'
#' Patterns anchor on a set of organizational keywords (e.g., "department",
#' "bureau") and require title-case tokens on either side. Three pattern
#' templates are generated:
#' 1. `{title+} ORG_WORD [of/for] {title+}` — plain multi-word suffix
#' 2. `{title+} ORG_WORD [of/for] {title} and/& {title}` — coordinated suffix
#' 3. `{title+} ORG_WORD of/for {title} {title} and/& {title}` — three-part
#'    coordinated suffix
#'
#' `max_preceding` and `max_trailing` control how many title-case tokens are
#' consumed on each side of the org keyword, letting you trade recall for
#' precision.
#'
#' @param label A character string. The entity label assigned to all matched
#'   spans. Defaults to `"PATTERN"`. Use a distinct label (e.g. `"PATTERN"`)
#'   so structural matches can be distinguished from exact dictionary matches
#'   produced by [entity_specify()].
#' @param org_keywords A character vector of **lowercase** organizational
#'   keywords used as anchors in patterns. Any token whose lowercase form
#'   appears in this vector triggers a match attempt. Defaults to a built-in
#'   set of common US government org terms.
#' @param max_preceding Controls how many consecutive title-case tokens may
#'   precede the organizational keyword. Accepts:
#'   * A positive integer — caps the prefix length (e.g., `2L` covers
#'     "California" and "San Bernardino"). Uses spaCy `OP = "{1,n}"`.
#'   * `Inf` — unlimited; uses `OP = "+"`. Use with caution as this may
#'     greedily consume long leading noun phrases.
#'   * `TRUE` — alias for `Inf`. `FALSE` — alias for `1L`.
#'
#'   Defaults to `2L`, which covers the most common cases (one- and two-word
#'   geographic prefixes such as "California" or "San Bernardino County").
#' @param max_trailing Controls how many consecutive title-case tokens may
#'   follow the organizational keyword phrase. Accepts the same values as
#'   `max_preceding`. Defaults to `Inf` (unlimited), greedily capturing
#'   multi-word suffixes such as "Board of Supervisors".
#'
#' @return A list of three pattern dictionaries formatted for spaCy's
#'   EntityRuler, suitable for appending to the output of [entity_specify()]
#'   and passing to [parse_text_trf()] via `entity_ruler_patterns`.
#'
#' @seealso [entity_specify()], [parse_text_trf()]
#'
#' @examples
#' \dontrun{
#' # Default: up to 2 preceding, unlimited trailing
#' pats <- build_structural_org_patterns()
#' str(pats)
#'
#' # Tighter matching: 2 preceding, at most 3 trailing
#' pats2 <- build_structural_org_patterns(max_trailing = 3L)
#'
#' # Custom org keywords
#' pats3 <- build_structural_org_patterns(
#'   org_keywords = c("department", "bureau", "commission"),
#'   max_preceding = 1L,
#'   max_trailing  = 2L
#' )
#'
#' # Combine with dictionary patterns from entity_specify()
#' dict_pats <- entity_specify(my_names, entity_label = "DICT")
#' all_pats  <- c(dict_pats, build_structural_org_patterns())
#' }
#'
#' @export
build_structural_org_patterns <- function(
    label        = "PATTERN",
    org_keywords = c(
      "department", "bureau", "commission", "office", "board", "service",
      "agency", "authority", "administration", "council", "division",
      "district", "institute", "center"
    ),
    max_preceding = 2L,
    max_trailing  = Inf
) {
  # --- input validation ---------------------------------------------------
  if (!is.character(label) || length(label) != 1L || nchar(label) == 0L) {
    stop("'label' must be a single non-empty character string")
  }
  if (!is.character(org_keywords) || length(org_keywords) == 0L) {
    stop("'org_keywords' must be a non-empty character vector")
  }

  resolve_op <- function(x, arg_name) {
    if (isTRUE(x))  x <- Inf
    if (isFALSE(x)) x <- 1L
    if (length(x) != 1L || !is.numeric(x) || is.na(x) || x < 1) {
      stop(paste0("'", arg_name, "' must be Inf, TRUE/FALSE, or a positive integer"))
    }
    if (is.infinite(x)) {
      "+"
    } else if (x == 1L) {
      NULL           # no OP = exactly 1 required token
    } else {
      paste0("{1,", as.integer(x), "}")
    }
  }

  pre_op      <- resolve_op(max_preceding, "max_preceding")
  trailing_op <- resolve_op(max_trailing,  "max_trailing")

  # --- token helpers ------------------------------------------------------
  title_tok <- function(op = NULL) {
    tok <- list(IS_TITLE = TRUE)
    if (!is.null(op)) tok$OP <- op
    tok
  }
  org_kw  <- function() list(LOWER = list(`in` = as.list(org_keywords)))
  of_opt  <- function() list(LOWER = list(`in` = list("of", "for")), OP = "?")
  of_req  <- function() list(LOWER = list(`in` = list("of", "for")))
  and_tok <- function() list(LOWER = list(`in` = list("and", "&")))
  pat     <- function(...) list(label = label, pattern = list(...))

  list(
    # {title^pre} ORG_WORD [of/for] {title^trailing}
    # "California Department of Agriculture"
    # "San Bernardino County Board of Supervisors"
    pat(title_tok(pre_op), org_kw(), of_opt(), title_tok(trailing_op)),

    # {title^pre} ORG_WORD [of/for] title and/& title
    # "California Department of Fish & Wildlife"
    # "Los Angeles Department of Water and Power"
    pat(title_tok(pre_op), org_kw(), of_opt(), title_tok(), and_tok(), title_tok()),

    # {title^pre} ORG_WORD of/for title title and/& title
    # "California Department of Food and Agriculture"
    pat(title_tok(pre_op), org_kw(), of_req(), title_tok(), title_tok(), and_tok(), title_tok())
  )
}
