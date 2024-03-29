% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_text.R
\name{parse_text}
\alias{parse_text}
\title{Creates an edgelist and nodelist for each document}
\usage{
parse_text(
  ret_path,
  keep_hyph_together = F,
  phrases_to_concatenate = NA,
  concatenator = "_",
  pages,
  file_ids,
  parsed_filenames,
  overwrite = T
)
}
\arguments{
\item{ret_path}{filepath to use for Sys.setenv reticulate python call. Note: Python and miniconda must already be installed.}

\item{keep_hyph_together}{Set to true to replace hyphens within a single word with underscores. Defaults to false.}

\item{phrases_to_concatenate}{character vector of phrases, in which each element is a string consisting of tokens separated by spaces. These are replaced with their concatenated version in order, from left to right. It is suggested that the most specific phrases, with the most words, are arranged at the left.}

\item{concatenator}{This is a character or string that will be used to replace the spaces in the phrases_to_concatenate.}

\item{pages}{This is a character vector, in which each element is a string that represents one page of text}

\item{file_ids}{This is a vector defining which pages are associated with which documents. The length is equal to the number of total pages.}

\item{parsed_filenames}{This is a character vector in which each element represents a filepath associated with its respective document.
The parsed data will be exported to these files.}

\item{overwrite}{A boolean. Whether to overwrite existing files}
}
\description{
Creates an edgelist and nodelist for each document
}
