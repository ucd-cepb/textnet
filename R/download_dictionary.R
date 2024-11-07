# Non-exported functions 
# download_dictionary

#' Downloads the SCOWL 2020.12.07 dictionary created by Kevin Atkinson, which has a larger vocabulary than Qdap
#' Code adapted from Mirek Długosz 2016 https://mirekdlugosz.com/blog/2016/how-to-use-r-to-recognize-if-given-string-is-a-word/
#' @importFrom utils download.file unzip
#' @importFrom tools file_ext
#' @importFrom stringi stri_read_lines stri_escape_unicode

download_dictionary <- function(){
  
  dictdir <- tempdir()
  dicturl <- 'https://sourceforge.net/projects/wordlist/files/SCOWL/2020.12.07/scowl-2020.12.07.zip/download'
  dictpath <- file.path(dictdir, basename(dicturl))
  if (! file.exists(dictpath)) {
    utils::download.file(dicturl, dictpath)
    utils::unzip(dictpath, exdir=paste0(dictdir,"/dictionary"))
  }
  
  eng_files <- list.files(file.path(
    paste0(dictdir,"/dictionary"), 'final'), full.names=TRUE)
  which_files <- (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("english", eng_files, fixed = TRUE)) | 
    (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("final/variant", eng_files, fixed = TRUE)) | 
    (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("american", eng_files, fixed = TRUE)) |
    (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("british", eng_files, fixed = TRUE)) |
    (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("canadian", eng_files, fixed = TRUE)) |
    (as.numeric(tools::file_ext(eng_files)) <= 60 & grepl("australian", eng_files, fixed = TRUE)) 
  
  eng_files <- eng_files[which_files]
  
  eng_words <- unique(unlist(sapply(eng_files, stringi::stri_read_lines, encoding="ISO-8859-1")))
  length(eng_words)
  eng_words <- stringi::stri_escape_unicode(eng_words)
  #when generating the data file eng_words, the following line of code was run
  save(eng_words, file = "data/eng_words.rda")
  
}
