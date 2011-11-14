# Author: Tony Breyal
# Date: 2011-11-11
# Modified: 2011-11-13
# Description: This function extracts as much information as it can for each result returned by a Yahoo search page.
# Contributations: Philipp Riemer - improvements to the xpathLVApply function code, see http://tonybreyal.wordpress.com/2011/11/11/web-scraping-yahoo-search-page-via-xpath/#comment-45
# Blog Reference: http://tonybreyal.wordpress.com/2011/11/10/facebook-graph-api-explorer-with-r/

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


yahooSearchXScraper <- function(input) {
  ###--- PACKAGES ---###
  # load packages
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # I added a wrapper around xpathSApply to deal with cases are NULL and are lost during the list to vector conversion process.
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
    if(sum(is.file) < 1 && length(input) > 1) stop("'input' to YahooSearchXScraper() could not be processed.")
    
    # read html from each file
    html.files <- lapply(input[is.file], readLines, warn = FALSE)
    
    # read html from each URL
    html.webpages <- lapply(input[!is.file], getURL, followlocation = TRUE)
    
    # return all html data as list
    return(c(html.files, html.webpages))
  }
  
  # construct data frame from the html of a single Yahoo search page
  get_yahoo_search_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)

    # construct yahoo search data frame
    xpath.base <- "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li"
    df <- data.frame(
      title = xpathLVApply(doc, xpath.base, "/div/div/h3/a", xmlValue),
      url = xpathLVApply(doc, xpath.base, "/div/div/h3/a/@href", I),
      description = xpathLVApply(doc, xpath.base, "/div/div[@class='abstr']", xmlValue),
      cached = xpathLVApply(doc, xpath.base, "/div/a[text()='Cached']/@href", I),
      recorded = xpathLVApply(doc, xpath.base, "/div/div/span[@id='resultTime']", xmlValue),
      stringsAsFactors = FALSE)
  
    # free doc from memory
    free(doc)
    
    # return yahoo search dataframe
    return(df)
  }
  
  
  ###--- MAIN ---##
  # STEP 1: Determine input type(s) and grab html accordingly
  doc.list <- evaluate_input(input)
  
  # STEP 2: get yahoo data frame.
  df <- do.call("rbind", lapply(doc.list, get_yahoo_search_df))
  return(df)
}


# ###--- EXAMPLES ---###
# #
# # Example 1
# input <- c("http://uk.search.yahoo.com/search;_ylt=A7x9QV6rWrxOYTsAHNFLBQx.?fr2=time&rd=r1&fr=yfp-t-702&p=Wil%20Wheaton&btf=w", "/home/tony/Wil Wheaton - Yahoo! Search Results.html")
# df <- yahooSearchXScraper(input)
# t(df[1, ])
# 
# # title       "Wil Wheaton - Google+"
# # url         "https://plus.google.com/108176814619778619437"
# # description "Wil Wheaton - Google+6 days ago"
# # cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4592664708059042&mkt=en-GB&setlang=en-GB&w=48d4b732,65b6306b&icp=1&.intl=uk&sig=6lwcOA8_4oGClQam_5I0cA--"
# # recorded    "6 days ago"
