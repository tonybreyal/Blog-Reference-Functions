# Author: Tony Breyal
# Date: 2011-11-11
# Modified: 2011-11-13
# Description: This function extracts as much information as it can for each result returned by a Google search page.
# Licence: non-comercial
# Contributations: Philipp Riemer - improvements to the xpathLVApply function code, see http://tonybreyal.wordpress.com/2011/11/11/web-scraping-yahoo-search-page-via-xpath/#comment-45
# Blog Reference: http://tonybreyal.wordpress.com/2011/11/10/facebook-graph-api-explorer-with-r/


googleSearchXScraper <- function(input) {
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
    if(sum(is.file) < 1 && length(input) > 1) stop("'input' to googleSearchXScraper() could not be processed.")
    
    # read html from each file
    html.files <- lapply(input[is.file], readLines, warn = FALSE)
    
    # read html from each URL
    html.webpages <- lapply(input[!is.file], getURL, followlocation = TRUE)
    
    # return all html data as list
    return(c(html.files, html.webpages))
  }
  
  # construct data frame from the html of a single Yahoo search page
  get_google_search_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)

    # construct yahoo search data frame
    xpath.base <- "//li[@class='g']"
    df <- data.frame(
      title = xpathLVApply(doc, xpath.base, "//h3//a[@href]", xmlValue),
      url = xpathLVApply(doc, xpath.base, "//h3//a/@href", I),
      description = xpathLVApply(doc, xpath.base, "//div[@class='s']", xmlValue),
      cached = xpathLVApply(doc, xpath.base, "//div[@class='s']//span[@class='flc']//a/@href", 
                            FUN = function(x) ifelse(length(x[grepl("webcache", x)])<1, "", x[grepl("webcache", x)]),
                            FUN2 = function(xx) sapply(xx, function(x) ifelse(is.null(x[x!=""]), NA, x[x!=""]))),
      similar = xpathLVApply(doc, xpath.base, "//div[@class='s']//span[@class='flc']//a/@href", 
                             FUN = function(x) ifelse(length(x[!grepl("webcache", x)])<1, "", x[!grepl("webcache", x)]), 
                             FUN2 = function(xx) sapply(xx, function(x) ifelse(is.null(x[x!=""]), NA, x[x!=""]))),
      stringsAsFactors = FALSE)
  
    # free doc from memory
    free(doc)
    
    # return yahoo search dataframe
    return(df)
  }
  
  
  ###--- MAIN ---##
  # STEP 1: Determine input type(s) and grab html accordingly
  html.list <- evaluate_input(input)
  
  # STEP 2: get google data frame.
  df <- do.call("rbind", lapply(html.list, get_google_search_df))
  return(df)
}


###--- EXAMPLES ---###
#
# Example 1
input <- "http://www.google.co.uk/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=r+project#q=r+project&hl=en&tbo=1&prmdo=1&output=search&source=lnt&tbs=qdr:m&sa=X&ei=qvO_Ttj1KITb8AOPzqT_Aw&ved=0CAoQpwUoBA&fp=1&biw=1920&bih=1086&cad=b&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb"
df <- googleSearchXScraper(input)
t(df[1, ])

