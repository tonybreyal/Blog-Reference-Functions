# Author: Tony Breyal
# Date: 2011-11-11
# Description: This function extracts as much information as it can for each post on a google plus status update page
# Reference: tonybreyal.wordpress.com/2011/11/11/web-scraping-google-via-xpath/


scrape_google_plus_page <- function(u, is.URL = grepl("http.*://.*\\.", u)) {
  # load required packages for scraper function
  require(RCurl)
  require(XML)

  # I hacked my own version of xpathSApply to deal with cases that return NULL which were causing me problems when converting a list to a vector
  xpathLVApply <- function(doc, path.base, path, FUN, FUN2 = NULL) {
    nodes.len <- length(xpathSApply(doc, path.base))
    paths <- sapply(1:nodes.len, function(i) gsub( path.base, paste(path.base, "[", i, "]", sep = ""), path, fixed = TRUE))
    xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
    if(!is.null(FUN2)) xx <- FUN2(xx)
    xx[sapply(xx, length)<1] <- NA
    xx <- as.vector(unlist(xx))
    return(xx)
  }
  
  # determine actions if u is a file or a URL
  if(is.URL) {
    # If url is HTTPS makes sure we have the SSL certificate off the cURL website to download html
    if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
    html <- getURL(u, cainfo = "cacert.perm")
  } else {
    html <- readLines(u, warn = FALSE)
  }
  doc <- htmlParse(html)

  # path to the set of nodes which are of interest
  base <- "//div[starts-with(@id, 'update')]"

  # construct data frame
  df <- data.frame(
    posted.by = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//span[@class='Hf']", xmlValue),
    ID = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')][1]//span[@class='eE']/a[@oid]", xmlAttrs, FUN2 = function(xx) sapply(xx, function(x) x[3])),
    message = xpathLVApply(doc, base,"//div[starts-with(@id, 'update')]//div[@class='vg']", xmlValue),
    message.share.link.name = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//div[@class='Ve jn']//div[@class='Bx']//div[@class='Us Gk']//div[@class='Jm']//div[@class='B-u-C dE']//a[@href]", xmlValue, FUN2 = function(xx) sapply(xx, function(x) ifelse(length(x)<1, return(NA), x[1]))),
    message.share.link = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//div[@class='Ve jn']//div[@class='Bx']//div[@class='Us Gk']//div[@class='Jm']//div[@class='B-u-C dE']//a[@href]", xmlAttrs,  FUN2 =   function(xx) {
  xx[sapply(xx, length)<1] <- NA
  return(sapply(xx, function(x) x[1]))
}),
    post.date = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//span[@class='mo fj']//span[@class='Qh kn']//a[@class='c-G-j c-i-j-ua hl']", xmlValue),
    comments = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//span[@class='a-j zx']//span[@class='Fw tx']", xmlValue),
    comments.by = xpathLVApply(doc, base,"//div[starts-with(@id, 'update')]//span[@class='px']//span[@class='uo']", xmlValue),
    sample.comments = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//div[@class='Qd']", xmlValue),
    shares = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//div[@class='Qx']//span[@class='a-j zo']", xmlValue),
    shares.by = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//div[@class='Qx']//span[@class='uo']", xmlValue),
    pluses = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//span[@class='me lg c-G-j']//div[@class='sB gl a-l-k me']", xmlValue),
    type = xpathLVApply(doc, base, "//div[starts-with(@id, 'update')]//span[@class='mo fj']//span[@class='a-j Rh Fo il']", xmlValue),
    stringsAsFactors = FALSE)

  
  # df clean up
  df$post.date <- gsub("Post date: ", "", df$post.date, fixed = TRUE)
  df$pluses <- as.integer(gsub("+", "", df$pluses, fixed = TRUE))
  df$shares <- as.integer(gsub(" shares", "", df$shares, fixed = TRUE))
  df$comments <- as.integer(df$comments)
  
  # free doc from memory
  free(doc)
  
  # return data frame
  return(df)
}


###--- EXAMLPLE 1: Using a URL ---###

u <- "https://plus.google.com/110286587261352351537/posts"
df <- scrape_google_plus_page(u, is.URL = TRUE)

dim(df)
# [1] 20 12

t(df[2, ])
                                                                                            
posted.by               "Felicia Day"                                                                                             
ID                      "110286587261352351537"                                                                                   
message                 "Commentary vid for DA: R Ep 5!"                                                                          
message.share.link.name "Dragon Age: Redemption Ep5 Annotation"                                                                   
message.share.link      "http://www.youtube.com/watch?v=GZ4NGa0qeaM"                                                              
post.date               "2011-11-09"                                                                                              
comments                "67"                                                                                                      
comments.by             "Christopher H, Jesse McGlothlin, Ronel Villeno, Sealavindo Marine, Alexander Pinckard and 1 more"        
sample.comments         "Christopher H  -  Watching your commentary videos are like watching your video blog in The Guild! :)    "
shares                  "20"                                                                                                      
shares.by               "Amy Mayer, Bonnie Zabytko, Brad Chasenore, Dark Matter fanzine, Donald Coleman and 15 more"              
pluses                  "270"                                                                                                     
type                    "Public"


###--- EXAMLPLE 2: Using a File ---###

# choose the .html file you've saved of the Google+ page
u <- file.choose()
df <- scrape_google_plus_page(u, is.URL = FALSE)

dim(df)
# [1] 60 12

