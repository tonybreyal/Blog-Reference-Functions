# Author: Tony Breyal
# Date: 2011-11-14
# Modified: 2011-11-14
# Description: This function extracts as much information as it can for each result returned by a Bing Search page into a data.frame structure.
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


bingSearchXScraper <- function(input) {
  ###--- PACKAGES ---###
  # load packages
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###  
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL
    if(grepl("www.bing.com/", input) && !grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="[feel free to delete] bingSearchXScraper SSL cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
 
  # I added a wrapper around xpathSApply to deal with cases are NULL and are lost during the list to vector conversion process.
  xpathLVApply <- function(doc, xpath.base, xpath.ext, FUN = I, FUN2 = NULL) {
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
  
  # construct data frame from the html of a single Bing Search html character vector
  get_bing_search_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)

    # xpath to node set of interest       
    xpath.base <- xpath.base <- "//li[@class='sa_wr']"
    
    # construct Bing Search data frame
    FUN2 <- function(x) lapply(x, function(xx) paste(unique(xx), collapse = " [NEXT>>] "))
    df <- data.frame(
      title = xpathLVApply(doc, xpath.base, "//h3//a[@href]", xmlValue),
      url = xpathLVApply(doc, xpath.base, "//h3//a/@href"),
      description = xpathLVApply(doc, xpath.base, "//p", xmlValue),
      meta.names = xpathLVApply(doc, xpath.base, "/div[@class='sa_cc']/ul[@class='sp_pss']//li", xmlValue, FUN2 = FUN2),
      meta.links = xpathLVApply(doc, xpath.base, "/div[@class='sa_cc']/ul[@class='sp_pss']//li/a/@href", FUN2 = FUN2),
      count = xpathSApply(doc, "//span[@id='count']", xmlValue),
    stringsAsFactors = FALSE)
    
    # free doc from memory
    free(doc)
    
    # return data frame
    return(df)
  }  
  
  
  ###--- MAIN ---###
  # STEP 1: Determine input type(s) and grab html accordingly  
  doc.list <- unlist(lapply(input, evaluate_input))
  
  # STEP 2: get Bing Search data frame.
  df <- do.call("rbind", lapply(doc.list, get_bing_search_df))
  return(df)  
}


# ###--- EXAMPLES ---###
# 
# # Example 1: input is a single Bing Search URL
# input <- "http://www.bing.com/search?q=elder+scrolls+skyrim&go=&qs=AS&sk=&pq=elder%2520scrols%2520skyrim&sp=1&sc=8-19&form=QBLH&filt=all"
# df <- bingSearchXScraper(input)
# t(df[2,])
# 
# # title       "The Elder Scrolls V: Skyrim - Wikipedia, the free encyclopedia"                                                                                                                                                                                                                                
# # url         "http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:_Skyrim"                                                                                                                                                                                                                                      
# # description "The Elder Scrolls V: Skyrim is a role-playing video game developed by Bethesda Game Studios and published by Bethesda Softworks. It is the fifth installment in The Elder ..."                                                                                                                 
# # meta.names  "Gameplay ·  [NEXT>>] Plot ·  [NEXT>>] Development ·  [NEXT>>] Music"                                                                                                                                                                                                                           
# # meta.links  "http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:_Skyrim#Gameplay [NEXT>>] http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:_Skyrim#Plot [NEXT>>] http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:_Skyrim#Development [NEXT>>] http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:_Skyrim#Music"
# # count       "1-10 of 15,300,000 results"  
