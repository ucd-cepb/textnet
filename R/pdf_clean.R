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
#' @importFrom methods is
#' @importFrom stringr str_split str_detect str_remove
#' @export
#' 

pdf_clean <- function(pdfs, keep_pages=NULL, ocr=F, maxchar=10000, export_paths=NULL, return_to_memory=T, suppressWarn = F, auto_headfoot_remove = T){
  if(!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for pdf_clean(). ",
         "Install it with: pak::pak('pdftools') or install.packages('pdftools')\n",
         "Note: pdftools requires the poppler library. ",
         "On Ubuntu/Debian: sudo apt-get install libpoppler-cpp-dev\n",
         "On macOS: brew install poppler",
         call. = FALSE)
  }

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
      texts <- suppressMessages(pdftools::pdf_text(pdfs[[k]]))
    }else{
      texts <- pdftools::pdf_text(pdfs[[k]])
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
          texts[i] <- pdftools::pdf_ocr_text(pdfs[[k]], pages=i,language = "eng")
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
        #uses the first element because only splitting one string
        #equivalent to str_split_1
        linebreaks <- str_split(texts[pagenum],"\\n")[[1]]
        
        #HEADER
        #if more than six lines of text on the page, remove all title-case lines
        #before whichever comes first: a non-title case line, or
        #the last empty row on or before the first six rows of the page
        if(length(linebreaks)>6){
          #search first six rows
          linebreakhead <- linebreaks[1:6]
          #search for sets of at least two \\n in a row
          #which correspond to a row of linebreaks that only has \\s* in it
          emptylines <- which(str_detect(linebreakhead,"^\\s*$"))
          
          emptylinegroups <- sapply(c(1:length(emptylines)),
                                    function(i) {ifelse(i!=1 && (emptylines[i]==emptylines[i-1]+1),NA,emptylines[i])
                                    })
          emptylinegroups <- emptylinegroups[!is.na(emptylinegroups)]
          
          #find last empty line if it exists and check everything before that for removal
          #we got rid of na's, so this outside condition makes sure the var isn't empty
          
          if(length(emptylinegroups)> 0){
            #this condition makes sure the last empty line isn't on line one
            if(emptylinegroups[length(emptylinegroups)] > 1){
              maxheaderlength <- emptylinegroups[length(emptylinegroups)] - 1
              #checker to see if a previous line was preserved, which means all 
              #potential header lines afterward should
              #also be preserved.
              prevlinepreserved = F
              linegetspreserved <- vector(mode = "logical", length = maxheaderlength)
              for(j in 1:maxheaderlength){
                #if there are over six lowercase words, it's probably a sentence.
                linegetspreserved[j] <- str_count(linebreaks[j], "\\s[a-z]") > 6 | 
                  #alternatively, if there's at least four long lowercase words, it's probably a sentence
                  #and may have a lot of capitalized entities in it
                  str_count(linebreaks[j], "\\s[a-z]{7,}") > 3
                if(linegetspreserved[j]==T){
                  prevlinepreserved = T
                  if(j < maxheaderlength){
                    #break out of the loop and preserve later header lines if you find a 
                    #sentence
                    linegetspreserved[j:maxheaderlength] <- T
                    break()
                  }
                }
              }
              #remove the parts of the text that aren't supposed to get preserved
              if(any(linegetspreserved==F)){
                linebreaks <- linebreaks[-which(!linegetspreserved)]
              }
            }
          }else{
            #either the only empty line is on row one, or there isn't one
            #we will bias toward removing more liberally in this case
            #only search top three rows in this case
            maxheaderlength <- 3
            #checker to see if a previous line was preserved, which means all 
            #potential header lines afterward should
            #also be preserved.
            prevlinepreserved = F
            linegetspreserved <- vector(mode = "logical", length = maxheaderlength)
            for(j in 1:maxheaderlength){
              #if there are over eight lowercase words, it's probably a sentence.
              linegetspreserved[j] <- str_count(linebreaks[j], "\\s[a-z]") > 8 | 
                #alternatively, if there's at least six long lowercase words, it's probably a sentence 
                #and may have a lot of capitalized entities in it
                str_count(linebreaks[j], "\\s[a-z]{7,}") > 5
              if(linegetspreserved[j]==T){
                prevlinepreserved = T
                if(j < maxheaderlength){
                  #break out of the loop and preserve later header lines if you find a 
                  #sentence
                  linegetspreserved[j:maxheaderlength] <- T
                  break()
                }
              }
            }
            
            #remove the parts of the text that aren't supposed to get preserved
            if(any(linegetspreserved==F)){
              linebreaks <- linebreaks[-which(!linegetspreserved)]
            }
          }
          
        }else{
          #fewer than six lines on the page
          #let's find the empty lines
          emptylines <- which(str_detect(linebreaks,"^\\s*$"))
          
          if(length(emptylines) > 0){
            #maxheader length is the smaller of the pagelength and first emptyline location
            maxheaderlength <- min(length(linebreaks), emptylines[1])
          }else{
            maxheaderlength <- length(linebreaks)
          }
          
          #checker to see if a previous line was preserved, which means all 
          #potential header lines afterward should
          #also be preserved.
          prevlinepreserved = F
          linegetspreserved <- vector(mode = "logical", length = maxheaderlength)
          if(length(linebreaks)>0){
            for(j in 1:maxheaderlength){
              #if there are over six lowercase words, it's probably a sentence.
              linegetspreserved[j] <- str_count(linebreaks[j], "\\s[a-z]") > 6 | 
                #alternatively, if there's at least four long lowercase words, it's probably a sentence 
                #and may have a lot of capitalized entities in it
                str_count(linebreaks[j], "\\s[a-z]{7,}") > 3
              if(linegetspreserved[j]==T){
                prevlinepreserved = T
                if(j < maxheaderlength){
                  #break out of the loop and preserve later header lines if you find a 
                  #sentence
                  linegetspreserved[j:maxheaderlength] <- T
                  break()
                }
              }
            }
            #remove the parts of the text that aren't supposed to get preserved
            if(any(linegetspreserved==F)){
              linebreaks <- linebreaks[-which(!linegetspreserved)]
            }
          }else{
            #the page is empty. we will deal with this later
          }
          
        }
        #we check this again because conditions are now different from before
        if(length(linebreaks)==0){
          linebreaks <- NA
        }
            
        #FOOTER
        #and search for first set of at least two \\n in a row 
        #on or after the 3rd-from-bottom group of \\n  
        #which is where the indicator of "emptylinegroups" starting from the bottom is < 3
        #and delete everything below it
        #as long as there are four or fewer lines of text after that cut line
        #and as long as the top chunk of text on/after the 3rd-from-bottom group of \\n
        #doesn't have sentences. Otherwise, don't remove that top chunk.
        
        emptylines <- which(str_detect(linebreaks,"^\\s*$"))
        
        if(length(emptylines)>=1){
          #consecutive empty lines are in the same "group"
          emptylinegroups <- sapply(c(1:length(emptylines)),
                                    function(i) {ifelse(i!=1 && (emptylines[i]==emptylines[i-1]+1),NA,emptylines[i])
                                    })
          emptylinegroups <- emptylinegroups[!is.na(emptylinegroups)]
          #earliest emptyline group should be third from last, or
          #the first group if there are less than three
          footercut <- ifelse(length(emptylinegroups)<3,emptylinegroups[1],
                              emptylinegroups[length(emptylinegroups)-2])
          
          #if footercut is more than 6 lines from the bottom of the page, we should move it to
          #the 6th line from the bottom of the page, so stuff in the middle of the page
          #isn't eligible for being cut
          if(footercut < length(linebreaks) - 6){
            footercut <- length(linebreaks) - 6
          }
          
          #checker to see if a lower line was preserved, which means all 
          #potential footer lines upward should
          #also be preserved.
          #footer is removing more prose compared to header so we will make the prose
          #criterion looser
          lowerlinepreserved = F
          linegetspreserved <- vector(mode = "logical", length = length(linebreaks))
          #starts check at bottom and goes up
          for(j in length(linebreaks):footercut){
            #if there are over five lowercase words, it's probably a sentence.
            linegetspreserved[j] <- str_count(linebreaks[j], "\\s[a-z]") > 5 | 
              #alternatively, if there's at least three long lowercase words, it's probably a sentence 
              #and may have a lot of capitalized entities in it
              str_count(linebreaks[j], "\\s[a-z]{7,}") > 2
            if(linegetspreserved[j]==T){
              lowerlinepreserved = T
              if(j > footercut){
                #break out of the loop and preserve all earlier lines if you find a 
                #sentence
                linegetspreserved[1:j] <- T
                break()
              }
            }
          }
          #even if we didn't match, we don't want to remove any lines earlier than 
          #the original footercut
          if(footercut > 1){
            linegetspreserved[1:(footercut-1)] <- T
          }
          
          #make sure there aren't more than four lines getting removed
          #if there are, move footer cut to the next emptyline group
          
          #first, set the new footercut to be at the text that isn't supposed to get preserved
          #this either returns a line number or na if there is nothing to cut
          #but because of the second argument of max, it can't go backwards compared
          #to where it was before the for loop
          footercut <- max(which(!linegetspreserved)[1], length(linebreaks) - 6)
          
          lineswithtext <- which(str_detect(linebreaks,"^\\s*$",negate = T))
          
          counter = 0
          #counter check should be <=1, to prevent footercut from being NA
          #if footercut is already NA, that means to keep the whole page, so skip this check
          while(!is.na(footercut) & sum(lineswithtext > footercut)>4 & counter <=1){
            #the footer is too big! only allow max four footer lines
            counter = counter + 1
            #iterate one element to the right in emptylinegroups each time through the 
            #while statement
            #each loop has a stricter requirement on when to set footercut to the first group
            #and otherwise iterates one group to the right each loop
            nextgroup <- ifelse(length(emptylinegroups)<(3-counter),emptylinegroups[1],
                                emptylinegroups[length(emptylinegroups)-(2-counter)])
            #if footercut is already past that group, keep it where it is
            footercut <- max(footercut, nextgroup)
            
          }
          if(is.na(footercut) | sum(lineswithtext > footercut)>4){
            #either footercut is set to na to keep all text or
            #there are too many lines of text after the last empty row. 
            #either way, keep all text by not adjusting the var linebreaks.
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
        
        texts[pagenum] <- ifelse(is.na(linebreaks[1]), NA, paste(linebreaks, collapse = "\n"))
        texts[pagenum] <- str_remove(texts[pagenum],"(((p|P)age\\s)|\\s{3,})([0-9|x|v|i]{1,6}|([A-Za-z]+\\p{Pd}[0-9]+))\\s*$")
        #remove page numbers: (word page and space or 3+ spaces) followed by (a combo of 1-6
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
