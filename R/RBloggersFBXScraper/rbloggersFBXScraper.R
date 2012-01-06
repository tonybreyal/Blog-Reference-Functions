# Author: Tony Breyal
# Date: 2012-01-06
# Description: Scrape the R-bloggers facebook wallpage and scraping r-bloggers.com for metadata
# Reference: not yet published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


rbloggersFBXScraper <- function() {
  # load require packages
  require(RCurl)
  require(XML)
  
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
  
  get_facebook_df <- function(f) {
    # parse document into html tree structure
    html <- paste(readLines(f, warn=FALSE), collapse = "\n", sep = "")
    doc <- htmlParse(html)
    
    # xpath to node set of interest       
    xpath.base <- "//ul[@id='profile_minifeed']/li"
        
    # parse facebook wall 
    timestamp <- xpathLVApply(doc, xpath.base, "//span[@class='uiStreamSource']//abbr/@title", I)
    num.likes <- xpathLVApply(doc, xpath.base, "//a[@title='See people who like this item']", function(x) paste(xmlValue(x), "like this"))
    num.comments <- xpathLVApply(doc, xpath.base, "//input[@name='view_all[1]']/@value", I)
    num.shares <- xpathLVApply(doc, xpath.base, "//a[@class='uiIconText uiUfiViewSharesLink']", xmlValue)
    posted.by <- xpathLVApply(doc, xpath.base, "//div[@class='actorDescription actorName']", xmlValue)
    message <- xpathLVApply(doc, xpath.base, "//span[@class='messageBody']", xmlValue)
    embeded.link <- xpathLVApply(doc, xpath.base, "//div[@class='uiAttachmentTitle']//a/@href", I)
    embeded.link.text <- xpathLVApply(doc, xpath.base, "//div[@class='mts uiAttachmentDesc translationEligibleUserAttachmentMessage']", xmlValue)
    sample.comments <- xpathLVApply(doc, xpath.base, "//ul[@class='commentList']", xmlValue)

                
    # pull vectors into a single data.frame
    df <- data.frame(timestamp, num.likes, num.comments, num.shares, posted.by, message, embeded.link, embeded.link.text, sample.comments, stringsAsFactors=FALSE)
        
    # free doc from memory (very important as otherwise doc will continue to exit outside of this local function)
    free(doc)
        
    # return df
    return( df[(!is.na(df$posted.by)), ] )
  }
  
  format_df <- function(df) {
    # remove commas from numbers
    df$num.likes <- gsub(",", "", df$num.likes, fixed = TRUE)
    df$num.comments <- gsub(",", "", df$num.comments, fixed = TRUE)
    df$num.shares <- gsub(",", "", df$num.shares, fixed = TRUE)
    
    # remove NA
    df$num.comments[is.na(df$num.comments) & df$sample.comments!=""] <- "At least 1 comment"
    df$num.comments[is.na(df$num.comments)] <- "0"
    df$num.likes[is.na(df$num.likes)] <- "0"
    df$num.shares[is.na(df$num.shares)] <- "0"    

    
    # add column to recognise an r-bloogers.com weblink
    df$rbloggers.link <- NA
    ind <- grep(".*http://www.r-bloggers.com/\\w+", df$embeded.link)
    df$rbloggers.link[ind] <- df$embeded.link[ind]
    
    # return cleaned up dataframe
    return(df)
  }
  
  get_metadata <- function(df) {
    scrape_rbloggers <- function(u) {
      if(is.na(u)) return(data.frame(title=NA, first.published=NA, author=NA, blog.name=NA, blog.link=NA, tags=NA, stringsAsFactors=FALSE))
      print(u)
      html <- try(getURL(u), silent = TRUE)
      if(inherits(html, "try-error")) return(data.frame(title=NA, first.published=NA, author=NA, blog.name=NA, blog.link=NA, tags=NA, stringsAsFactors=FALSE))
      doc <- try(htmlParse(html), silent = TRUE)
      if(inherits(doc, "try-error")) return(data.frame(title=NA, first.published=NA, author=NA, blog.name=NA, blog.link=NA, tags=NA, stringsAsFactors=FALSE))
      
      xpath.base <- "//div[@id='mainwrapper']"
      title <- xpathLVApply(doc, xpath.base, "//h1", xmlValue)[1]
      first.published <- xpathLVApply(doc, xpath.base,"//div[@class='date']", xmlValue)[1]
      author <- xpathLVApply(doc, xpath.base,"//div[@class='meta']/a", xmlValue)[1] 
      blog.name <- xpathLVApply(doc, xpath.base,"//div[@class='entry']/div[1]/strong/a", xmlValue)[1] 
      blog.link <- xpathLVApply(doc, xpath.base,"//div[@class='entry']/div[1]/strong/a/@href")[1]
      tags <- gsub("Tags: ", "", xpathLVApply(doc, xpath.base,"//p[@class='tags']", xmlValue)[1], fixed = TRUE)
      return(data.frame(title, first.published, author, blog.name, blog.link, tags, stringsAsFactors = FALSE)[1,])
    }
    
    # determine which posts we have a link to rbloggers.com to get more meta data
    ind <- which(!is.na(df$rbloggers.link))
    df.list <- do.call("rbind", lapply(df$rbloggers.link, scrape_rbloggers))
    df2 = cbind(df, df.list)
    return(df2)
  }
  
  ### main function code ###
  # step 1: read in Facebook Wall html file stored (must have been saved as a COMPLET webpage, or something to that effect, NOT html only)
  f <- file.choose()
  
  # step 2: parse facebook wall webpage
  df <- get_facebook_df(f)
  
  # step 3: clean up dataframe
  df <- format_df(df)
  
  # step 4: get metadata from r-bloggers website
  df <- get_metadata(df)
  return(df)
}


###--- EXAMPLES ---###
# df <- rbloggersFBXScraper()
#
# rownames(subset(df, author == "Tony Breyal"))
# #[1] "4"  "30" "38" "63" "72"
#
# t(df[72, ])
# #                   72                                                                                                                                                                                                                                                                                                                         
# # timestamp         "Tuesday, November 8, 2011 at 9:12pm"                                                                                                                                                                                                                                                                                      
# # num.likes         "7"                                                                                                                                                                                                                                                                                                                        
# # num.comments      NA                                                                                                                                                                                                                                                                                                                         
# # num.shares        NA                                                                                                                                                                                                                                                                                                                         
# # posted.by         "R bloggers"                                                                                                                                                                                                                                                                                                               
# # message           "http://www.r-bloggers.com/web-scraping-google-scholar-partial-success/"                                                                                                                                                                                                                                                   
# # embeded.link      "http://www.r-bloggers.com/web-scraping-google-scholar-partial-success/"                                                                                                                                                                                                                                                   
# # embeded.link.text "I\n wanted to scrape the information returned by a Google Scholar web \nsearch into an R data frame as a quick XPath exercise. The following \nwill successfully extract  the ‘title’, ‘url’ , ‘publication’ and \n‘description’.  If any of these fields are not available, as in the case\n of a citation, the corre..."
# # sample.comments   "a good blog .. need to try out the code [NEXT>>] Abstract still needs polishing :-) [NEXT>>]  (after scraping content?)"                                                                                                                                                                                                  
# # rbloggers.link    "http://www.r-bloggers.com/web-scraping-google-scholar-partial-success/"                                                                                                                                                                                                                                                   
# # title             "Web Scraping Google Scholar (Partial Success)"                                                                                                                                                                                                                                                                            
# # first.published   "November 8, 2011"                                                                                                                                                                                                                                                                                                         
# # author            "Tony Breyal"                                                                                                                                                                                                                                                                                                              
# # blog.name         " Consistently Infrequent Â» R"                                                                                                                                                                                                                                                                                            
# # blog.link         "http://tonybreyal.wordpress.com/2011/11/08/web-scraping-google-scholar-partial-success/"                                                                                                                                                                                                                                  
# # tags              "google scholar, R, rstats, web scraping, XML, XPath"