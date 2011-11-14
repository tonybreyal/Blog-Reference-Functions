# Author: Tony Breyal
# Date: 2011-11-08
# Modified: 2011-11-13
# Description: This function will retrieve as information as it can about each result on a Google Scholar search page
# Contributations: Philipp Riemer - improvements to the xpathLVApply function code, see http://tonybreyal.wordpress.com/2011/11/11/web-scraping-yahoo-search-page-via-xpath/#comment-45
# Blog Reference: http://tonybreyal.wordpress.com/2011/11/08/web-scraping-google-scholar-part-2-complete-success/

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License 
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

googleScholarXScraper <- function(input) {
  ###--- PACKAGES ---###
  # load packages
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###  
  # I added a wrapper around xpathSApply to deal with cases return NULL and are thus were removed during the list to vector conversion process. This function ensures the NULLs are replaced by NA
  xpathLVApply <- function(doc, xpath.base, xpath.ext, FUN, FUN2 = NULL) {
    # get xpaths to each child node of interest
    nodes.len <- length(xpathSApply(doc, xpath.base))
    paths <- sapply(1:nodes.len, function(i) paste(xpath.base, "[", i, "]", xpath.ext, sep = ""))
      
    # extract child nodes
    xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
    
    # perform extra processing if required
    if(!is.null(FUN2)) xx <- FUN2(xx)
      
    # convert NULL to NA in list
    xx[sapply(xx, length)<1] <- NA
      
    # return node values as a vector
    return(as.vector(unlist(xx)))
  }
  
  # Determine how to grab html for each element of input
  evaluate_input <- function(input) {
    # determine which elements of input are files (assumed to contain valid html) and which are not(assumed to be valid URLs)
    is.file <- file.exists(input)
    
    # stop if input does not seem to be URLS and/or files
    if(sum(is.file) < 1 && length(input) > 1) stop("'input' to googleScholarXScraper() could not be processed.")
    
    # read html from each file
    html.files <- lapply(input[is.file], readLines, warn = FALSE)
    
    # read html from each URL
    html.webpages <- lapply(input[!is.file], getURL, followlocation = TRUE)
    
    # return all html data as list
    return(c(html.files, html.webpages))
  }
  
  # construct data frame from the html of a single Google Scholar search page
  get_google_scholar_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)
  
    # construct data frame
    xpath.base <- "/html/body/div[@class='gs_r']"
    df <- data.frame(
            footer = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']", xmlValue),
            title = xpathLVApply(doc, xpath.base, "/div[@class='gs_rt']/h3", xmlValue),
            type = xpathLVApply(doc, xpath.base, "/div[@class='gs_rt']/h3/span", xmlValue),
            publication = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_a']", xmlValue),
            description = xpathLVApply(doc, xpath.base, "/font", xmlValue),
            cited.by = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Cited by')]/text()", xmlValue),
            cited.ref = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Cited by')]", xmlAttrs),
            title.url = xpathLVApply(doc,  xpath.base, "/div[@class='gs_rt']/h3/a", xmlAttrs),
            view.as.html = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'View as HTML')]", xmlAttrs),
            view.all.versions = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,' versions')]", xmlAttrs),
            from.domain = xpathLVApply(doc, xpath.base, "/span[@class='gs_ggs gs_fl']/a", xmlValue),
            related.articles = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Related articles')]", xmlAttrs),
            library.search = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Library Search')]", xmlAttrs),
            result.set = xpathSApply(doc, "/html/body/form/table/tr/td[2]", xmlValue),
            stringsAsFactors = FALSE)
    # free doc from memory
    free(doc)
      
    # Clean up extracted text
    df$title <- sub(".*\\] ", "", df$title)
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$publication[i], "", df$description[i], fixed = TRUE))
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$footer[i], "", df$description[i], fixed = TRUE))
    df$type <- gsub("\\]", "", gsub("\\[", "", df$type))
    df$cited.by <- as.integer(gsub("Cited by ", "", df$cited.by, fixed = TRUE))
      
    # remove footer as it is now redundant after doing clean up  and return dataframe
    return(df[,-1])
  }
  
   
  ###--- MAIN ---##
  # STEP 1: Determine input type(s) and grab html accordingly
  doc.list <- evaluate_input(input)
  
  # STEP 2: get google scholar data frame.
  df <- do.call("rbind", lapply(doc.list, get_google_scholar_df))
  return(df)
}


# ###--- EXAMPLES ---###
# # example 1: A single URL
# u <- "http://scholar.google.com/scholar?as_q=baldur%27s+gate+2&num=20&btnG=Search+Scholar&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=&as_ylo=&as_yhi=&as_sdt=1.&as_sdtp=on&as_sdtf=&as_sdts=5&hl=en"
# df <- googleScholarXScraper(u)
# t(df[1, ])
# 
# # title             "Baldur's gate and history: Race and alignment in digital role playing games"
# # type              "PDF"
# # publication       "C Warnes - Digital Games Research Conference (DiGRA), 2005 - digra.org"
# # description       "... It is argued that games like Baldur's Gate I and II cannot be properly understood without\nreference to the fantasy novels that inform them. ... Columbia University Press, New York, 2003.\npp 2-3. 12. 8. Hess, Rhyss. Baldur's Gate and Tales of the Sword Coast. ... \n"
# # cited_by          "8"
# # cited_ref         "/scholar?cites=13835674724285845934&as_sdt=2005&sciodt=0,5&hl=en&oe=ASCII&num=20"
# # title_url         "http://digra.org:8080/Plone/dl/db/06276.04067.pdf"
# # view_as_html      "http://scholar.googleusercontent.com/scholar?q=cache:rpHocNswAsAJ:scholar.google.com/+baldur%27s+gate+2&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# # view_all_versions "/scholar?cluster=13835674724285845934&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# # from_domain       "[PDF] from digra.org"
# # related_articles  "/scholar?q=related:rpHocNswAsAJ:scholar.google.com/&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# # library_search    NA
# # result_set        "Results 1 - 20 of about 404.   (0.29 sec)Â "
