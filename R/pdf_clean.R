# Exported function
# pdf_clean

#' Take a vector and clean the strings using regex. 
#' Uppercase or lowercase math font is converted to uppercase or lowercase letters, respectively.
#' Any trailing "'s" at the end of the entity name is removed. 
#' All non-word characters are removed.
#' Consecutive underscores are collapsed to a single underscore.
#' Leading and trailing underscores are removed.
#' Entities that have no letters are removed, if remove_nums is set to T.
#'
#' @param pdfs a vector of file names
#' @param keep_pages By default, NULL keeps all pages. Alternatively, the user can specify a list of logical or numeric vectors of pages to keep in the exported file. 
#' When specified, the length of keep_pages must be equivalent to the length of pdfs.
#' Each logical vector in keep_pages must have a length equivalent to the number of pages in the corresponding input pdf. 
#' All pages with a value of T will be included in the exported file. 
#' Each numeric vector in keep_pages enumerates specific page numbers to keep. 
#' For numeric vectors in keep_pages, duplicate values will be returned only once, and the vector will be sorted before processing.
#' @param ocr A logical value: T to run ocr image-to-text detection on pages with fewer than 20 characters;
#' F to not run ocr on any pages.
#' @param maxchar A numeric value representing the maximum allowable number of characters per page, 
#' with a default of 10,000. Pages on which the number of characters exceeds maxchar will have the output text 
#' set to NA.
#' @param export_paths A vector of filenames to which to save the text outputs. 
#' Defaults to NULL, which does not save the output to a file.
#' @param return_to_memory A logical value representing whether or not to return 
#' the output text to memory. Defaults to T.
#' @param suppressWarn Boolean; F by default. Setting this to T suppresses pdf_text conversion warnings, which can be verbose.
#' @param auto_headfoot_remove Boolean. Tries to remove headers and footers if T. May remove small amounts of paragraph text, especially if there is no header on a page. Defaults to T to avoid having entities that appear consistently in headers be artificially inflated in the textnet_extract network. Recommend keeping this parameter T unless using a separate header/footer removal tool.
#' @return If return_to_memory is T, returns a list with a length equal 
#' to the length of the pdfs parameter. Each element in the list is a character vector
#' with a length equal to the number of pages in its respective document. If export_paths
#' is not null, each character vector of text will be saved to a separate RDS file, where the 
#' number of files is equal to the number of pdfs. 
#' @importFrom pdftools pdf_text pdf_ocr_text
#' @importFrom methods is
#' @importFrom stringr str_split str_detect str_remove
#' @export
#' 

pdf_clean <- function(pdfs, keep_pages=NULL, ocr=F, maxchar=10000, export_paths=NULL, return_to_memory=T, suppressWarn = F, auto_headfoot_remove = T){
  if(!is.character(pdfs)) {
    stop("'pdfs' must be a character vector of file names")
  }
  
  if(!is.null(keep_pages)){
    if(!is.list(keep_pages) | length(keep_pages)!=length(pdfs)){
      stop("if specified, keep_pages must be a list of the same length as pdfs")
    }
    if(any(unlist(lapply(keep_pages, function(i) !is.logical(i) & !is.numeric(i))))){
      stop("if specified, elements of the list keep_pages must be either numeric or logical")
    }
  }
  
  if(!is.logical(ocr) || length(ocr) != 1) {
    stop("'ocr' must be a single logical value")
  }
  
  if(!is.numeric(maxchar) || length(maxchar) != 1 || maxchar <= 0) {
    stop("'maxchar' must be a single positive numeric value")
  }
  
  if(!is.null(export_paths) && !is.character(export_paths)) {
    stop("'export_paths' must be NULL or a character vector")
  }
  
  if(!is.logical(return_to_memory) || length(return_to_memory) != 1) {
    stop("'return_to_memory' must be a single logical value")
  }
  
  if(!is.logical(suppressWarn) || length(suppressWarn) != 1) {
    stop("'suppressWarn' must be a single logical value")
  }
  
  if(!is.logical(auto_headfoot_remove) || length(auto_headfoot_remove) != 1) {
    stop("'auto_headfoot_remove' must be a single logical value")
  }

  if(return_to_memory==F & is.null(export_paths)){
    stop("Either return_to_memory must be true or export_paths must be non-null.")
  }
  
  if(return_to_memory){
    all_pdfs <- vector(mode = "list", length = length(pdfs))
  }
  
  for(k in 1:length(pdfs)){
    if(suppressWarn==T){
      texts <- suppressMessages(pdf_text(pdfs[[k]]))
    }else{
      texts <- pdf_text(pdfs[[k]])
    }
    if(!is.null(keep_pages)){
      if(is.logical(keep_pages[[k]])){
        if(length(keep_pages[[k]]) != 1 & length(keep_pages[[k]]) != length(texts)){
          stop("Each logical vector within keep_pages must be either length 1 or the page length of the input document")
        }
        texts <- texts[keep_pages[[k]]]
      }
      if(is.numeric(keep_pages[[k]])){
        if(any(keep_pages[[k]] > length(texts))){
          stop("page numbers in keep_pages to keep may not exceed the page length of the input document")
        }
        if(any(keep_pages[[k]] < 1) | any(!keep_pages[[k]]%%1 == F)){
          stop("keep_pages must only be positive integers")
        }
        #removes duplicated pages and sorts from first to last
        texts <- texts[sort(base::unique(keep_pages[[k]]))]
      }
    } 
    
    if(ocr==T){
      for(i in 1:length(texts)){
        if (nchar(texts[i])< 20){
          texts[i] <- pdf_ocr_text(pdfs[[k]], pages=i,language = "eng")
        }
      }
    }
    
    #if too many chars, it probably is not an actual page of text but rather a map or figure
    for(i in 1:length(texts)){
      if (nchar(texts[i])> maxchar){
        texts[i] <- NA
      }
    }
    
    if(auto_headfoot_remove == T){
      for(pagenum in 1:length(texts)){
        linebreaks <- str_split(texts[pagenum],"\\n")[[1]]
        
        #HEADER
        if(length(linebreaks)>6){
          #search for last set of at least two \\n in a row 
          #which corresponds to a row of linebreaks that has only \\s* in it
          #before the sixth \\n 
          #which is row six in linebreaks
          #and delete it and all the rows above it
          linebreakhead <- linebreaks[1:6]
          emptylines <- which(str_detect(linebreakhead,"^\\s*$"))
          if(length(emptylines)>=1){
            headercut <- emptylines[length(emptylines)]
            linebreaks <- linebreaks[(headercut+1):length(linebreaks)]
          }
          #if there are no empty lines, don't remove anything
        }else{
          emptylines <- which(str_detect(linebreaks,"^\\s*$"))
          if(length(emptylines)>=1){
            #just remove everything before the first set of two \\n,
            headercut <- emptylines[1]
            if(length(linebreaks)>headercut){
              linebreaks <- linebreaks[(headercut+1):length(linebreaks)]
            }else{
              #if there are no lines of text after the cut, make the text NA
              linebreaks <- NA
            }
          }else{
            # or if one doesn't exist, 
            #remove everything since it's probably just a figure caption 
            #or header on a map page
            linebreaks <- NA
          }
          
        }
        
        #FOOTER
        #and search for first set of at least two \\n in a row 
        #on or after the 3rd-from-bottom group of \\n  
        #which is where the indicator of "emptylinegroups" starting from the bottom is < 3
        #and delete everything below it
        #as long as there are four or fewer lines of text after that cut line
        counter = 0
        emptylines <- which(str_detect(linebreaks,"^\\s*$"))
        
        if(length(emptylines)>=1){
          emptylinegroups <- sapply(c(1:length(emptylines)),
                                    function(i) {ifelse(i!=1 && (emptylines[i]==emptylines[i-1]+1),NA,emptylines[i])
                                    })
          emptylinegroups <- emptylinegroups[!is.na(emptylinegroups)]
          footercut <- ifelse(length(emptylinegroups)<3,emptylinegroups[1],
                              emptylinegroups[length(emptylinegroups)-2])
          lineswithtext <- which(str_detect(linebreaks,"^\\s*$",negate = T))
          
          while(sum(lineswithtext > footercut)>4 & counter <=2){
            counter = counter + 1
            #the footer is too big! only allow max four footer lines
            footercut <- ifelse(length(emptylinegroups)<(3-counter),emptylinegroups[1],
                                emptylinegroups[length(emptylinegroups)-(2-counter)])
            
          }
          if(sum(lineswithtext > footercut)>4){
            #there are too many lines of text after the last empty row. keep all text.
          }else{
            #found the footer! cut everything after footercut
            if(footercut==1){
              #that means there's nothing useful on the page -- it's all footer
              linebreaks <- NA
            }
            linebreaks <- linebreaks[1:(footercut-1)]
          }
        }
        #else don't remove anything, since
        #there are no empty lines.
        
        texts[pagenum] <- ifelse(is.na(linebreaks[1]), NA, paste(linebreaks, collapse = " "))
        texts[pagenum] <- str_remove(texts[pagenum],"\\s{3,}([0-9|x|v|i]{1,6}|([a-z]+\\p{Pd}[0-9]+))\\s*$")
        #remove page numbers: 5+ spaces followed by (a combo of 1-6
        #roman and arabic numerals) or (a letter, hyphen, and
        #set of numbers such as c-28) at the end of a page
      }   
    }
    
    if(!is.null(export_paths)){
      saveRDS(texts, export_paths[k])
    }
    if(return_to_memory==T){
      
      all_pdfs[[k]] <- texts
      names(all_pdfs[k]) <- pdfs[k]
    }
  }
  if(return_to_memory==T){
    return(all_pdfs)
  }
}
