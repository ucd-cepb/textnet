# Exported functions 
# download_dictionary

#' Downloads the SCOWL dictionary, which has a larger vocabulary than Qdap
#' 


download_dictionary <- function(){
  
  dict_dir <- tempdir()
  dict_url <- 'http://downloads.sourceforge.net/wordlist/scowl-2016.01.19.zip'
  dict_local_zip <- file.path(dict_dir, basename(dict_url))
  if (! file.exists(dict_local_zip)) {
    download.file(dict_url, dict_local_zip)
    unzip(dict_local_zip, exdir=paste0(dict_dir,"/dictionary"))
  }
  
  dict_files <- list.files(file.path(
    paste0(dict_dir,"/dictionary"), 'final'), full.names=TRUE)
  dict_files_match <- as.numeric(tools::file_ext(dict_files)) <= 60 & grepl("english-", dict_files, fixed = TRUE)
  dict_files <- dict_files[ dict_files_match ]
  
  eng_words <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE))
  length(eng_words)
  #when generating the data file eng_words, the following line of code was run
  #save(eng_words, file = "data/eng_words.rda")
  
}

