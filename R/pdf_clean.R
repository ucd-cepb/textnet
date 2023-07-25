# Exported functions 
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
#' @param keep_pages A list of logical vectors, such that each vector in the list represents a pdf 
#' and each element of the vector represents a page. All pages for which keep_pages == T will be included
#' in the exported file. keep_pages defaults to a single logical value: T, which keeps all pages.
#' @param ocr A logical value: T to run ocr image-to-text detection on pages with fewer than 20 characters;
#' F to not run ocr on any pages.
#' @param maxchar A numeric value representing the maximum allowable number of characters per page, 
#' with a default of 10,000. Pages on which the number of characters exceeds maxchar will have the output text 
#' set to NA.
#' @param export_paths A vector of filenames to which to save the text outputs. 
#' Defaults to NULL, which does not save the output to a file.
#' @param return_to_memory A logical value representing whether or not to return 
#' the output text to memory. Defaults to T.
#' 
#' @import pdftools
#' @import tesseract
#' @import stringr

#' @return If return_to_memory is T, returns a list with a length equal 
#' to the length of the pdfs parameter. Each element in the list is a character vector
#' with a length equal to the number of pages in its respective document. If export_paths
#' is not null, each character vector of text will be saved to a separate RDS file, where the 
#' number of files is equal to the number of pdfs. 
#' 
#' @export
#' 

pdf_clean <- function(pdfs, keep_pages=T, ocr=F, maxchar=10000, export_paths=NULL, return_to_memory=T){
  if(return_to_memory==F & is.null(export_paths)){
    stop("Either return_to_memory must be true or export_paths must be non-null.")
  }
  if(return_to_memory){
    all_pdfs <- vector(mode = "list", length = length(pdfs))
  }
  for(k in 1:length(pdfs)){
    texts <- suppressMessages(pdf_text(pdfs[[k]]))
    
    if(class(keep_pages)=="list"){
      texts <- texts[keep_pages[[k]]]
    } else if(class(keep_pages)!="logical" || keep_pages == F){
      stop("keep_pages must be either T (to keep all pages) or a list of logical vectors.")
    }
    
    if(ocr==T){
      #test took 20 minutes to parse 24 pages at 600 dpi. There are 95 total in the 688 page "0007" GSP
      #if not enough chars, it might be a photocopy that needs to be scanned
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
      
    }       
    
    #remove page numbers: 5+ spaces followed by (a combo of 1-6
    #roman and arabic numerals) or (a letter, hyphen, and
    #set of numbers such as c-28) at the end of a page
    
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
