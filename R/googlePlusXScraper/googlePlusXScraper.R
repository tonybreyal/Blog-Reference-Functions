# Author: Tony Breyal
# Date: 2011-11-11
# Description: This function extracts as much information as it can for each post on a google plus status update page
# Reference: tonybreyal.wordpress.com/2011/11/11/web-scraping-google-via-xpath/

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


googlePlusXScraper <- function(input) {
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
    if(sum(is.file) < 1 && length(input) > 1) stop("'input' to googlePlusXScraper() could not be processed.")
    
    # read html from each file
    html.files <- lapply(input[is.file], readLines, warn = FALSE)
    
    # read html from each URL
    
    # get security certificates so we can access https links (we'll delete it once we have the webpages)
    if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
    html.webpages <- lapply(input[!is.file], getURL, followlocation = TRUE, cainfo = "cacert.perm")
    file.remove("cacert.perm")
    
    # return all html data as list
    return(c(html.files, html.webpages))
  }
  
  # construct data frame from the html of a single Google Plus posts page
  get_google_plus_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)

    # path to the set of nodes which are of interest
    xpath.base <- "//div[starts-with(@id, 'update')]"
    
    # construct google plus data frame
    df <- data.frame(
      posted.by = xpathLVApply(doc, xpath.base, "//span[@class='Hf']", xmlValue),
      ID = xpathLVApply(doc, xpath.base, "//span[@class='eE']/a/@oid", I),
      message = xpathLVApply(doc, xpath.base,"//div[@class='vg']", xmlValue),
      message.embeded.names = xpathLVApply(doc, xpath.base, "//div[@class='Ve jn']//a", xmlValue,  FUN2=function(x) lapply(x, function(xx) paste(unique(xx), collapse = " [NEXT>>] "))),
      message.embeded.links = xpathLVApply(doc, xpath.base, "//div[@class='Ve jn']//a/@href", I,  FUN2=function(x) lapply(x, function(xx) paste(unique(xx), collapse = " [NEXT>>] "))),
      post.date = xpathLVApply(doc, xpath.base, "//span[@class='mo fj']//span[@class='Qh kn']//a[@class='c-G-j c-i-j-ua hl']", xmlValue),
      pluses = xpathLVApply(doc, xpath.base, "//span[@class='me lg c-G-j']//div[@class='sB gl a-l-k me']", xmlValue),
      comments = xpathLVApply(doc, xpath.base, "//span[@class='a-j zx']//span[@class='Fw tx']", xmlValue),
      comments.by = xpathLVApply(doc, xpath.base,"//span[@class='px']//span[@class='uo']", xmlValue),
      sample.comments = xpathLVApply(doc, xpath.base, "//div[@class='Qd']", xmlValue),
      shares = xpathLVApply(doc, xpath.base, "//div[@class='Qx']//span[@class='a-j zo']", xmlValue),
      shares.by = xpathLVApply(doc, xpath.base, "//div[@class='Qx']//span[@class='uo']", xmlValue),
      type = xpathLVApply(doc, xpath.base, "//span[@class='mo fj']//span[@class='a-j Rh Fo il']", xmlValue),
      stringsAsFactors = FALSE)
    
    # free doc from memory
    free(doc)

    # return cleaned up data frame
    df$post.date <- gsub("Post date: ", "", df$post.date, fixed = TRUE)
    df$pluses <- as.integer(gsub("+", "", df$pluses, fixed = TRUE))
    df$shares <- as.integer(gsub(" shares", "", df$shares, fixed = TRUE))
    df$comments <- as.integer(df$comments)
    return(df)
  }

  
  ###--- MAIN ---##
  # STEP 1: Determine input type(s) and grab html accordingly
  doc.list <- evaluate_input(input)
  
  # STEP 2: get google plus data frame.
  df <- do.call("rbind", lapply(doc.list, get_google_plus_df))
  return(df)
}


# ###--- EXAMPLES ---###
# 
# # EXAMLPLE 1: Using a URL
# input <- "https://plus.google.com/110286587261352351537/posts"
# df <- googlePlusXScraper(input)
# t(df[2, ])
# 
# # posted.by             "Felicia Day"                                                                                                                                                                           
# # ID                    "110286587261352351537"                                                                                                                                                                 
# # message               "Um, I cannot wait.  So what class are you gonna be?!?!  I want to be a wood elf now, the Kajit are cool but I think I want a pretty humanoid to play for 100+ hours haha."             
# # message.embeded.names " [NEXT>>] Felicia Day [NEXT>>] Post date: 2011-11-10 [NEXT>>] VGW Review: The Elder Scrolls V: Skyrim [NEXT>>] Gabriel Nathan"                                                         
# # message.embeded.links "./110286587261352351537 [NEXT>>] 110286587261352351537/posts/Vo21AWjk1BK [NEXT>>] http://videogamewriters.com/review-the-elder-scrolls-v-skyrim-28778 [NEXT>>] ./111164057446652654740"
# # post.date             "2011-11-10"                                                                                                                                                                            
# # pluses                "760"                                                                                                                                                                                   
# # comments              "463"                                                                                                                                                                                   
# # comments.by           "Gabriel Nathan, Phillip Tunstall, Jamie Kristopher, DeAdrian Lewis, Eric Ogles and 1 more"                                                                                             
# # sample.comments       "Gabriel Nathan  -  Good Review    "                                                                                                                                                    
# # shares                "44"                                                                                                                                                                                    
# # shares.by             "Achmad Soemiardhita, Adam Pace, Alexis Bane, Allen Carrigan, Amanda Troutman and 39 more"                                                                                              
# # type                  "Public"