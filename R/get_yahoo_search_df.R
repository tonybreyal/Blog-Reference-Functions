# Author: Tony Breyal
# Date: 2011-11-11
# Description: This function extracts as much information as it can for each result returned by a Yahoo search page.
# Reference: http://tonybreyal.wordpress.com/2011/11/10/facebook-graph-api-explorer-with-r/


get_yahoo_search_df <- function(u) {
  # load packages
  require(RCurl)
  require(XML)

  # I hacked my own version of xpathSApply to deal with cases that return NULL which were causing me problems
  xpathSNullApply <- function(doc, path.base, path, FUN, FUN2 = NULL) {
    nodes.len <- length(xpathSApply(doc, path.base))
    paths <- sapply(1:nodes.len, function(i) gsub( path.base, paste(path.base, "[", i, "]", sep = ""), path, fixed = TRUE))
    xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
    if(!is.null(FUN2)) xx <- FUN2(xx)
    xx[sapply(xx, length)<1] <- NA
    xx <- as.vector(unlist(xx))
    return(xx)
  }

  # download html and parse into tree structure
  html <- getURL(u, followlocation = TRUE)
  doc <- htmlParse(html)

  # path to nodes of interest
  path.base <- "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li"

  # construct data frame
  df <- data.frame(
    title = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/h3/a", xmlValue),
    url = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/h3/a[@href]", xmlAttrs, FUN2 = function(xx) sapply(xx, function(x) x[2])),
    description = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div", xmlValue),
    cached = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/a[@href][text()='Cached']", xmlAttrs, FUN2 = function(xx) sapply(xx, function(x) x[1])),
    recorded = xpathSNullApply(doc, path.base, "/html/body/div[@id='doc']/div[@id='bd-wrap']/div[@id='bd']/div[@id='results']/div[@id='cols']/div[@id='left']/div[@id='main']/div[@id='web']/ol/li/div/div/span[@id='resultTime']", xmlValue),
    stringsAsFactors = FALSE)

  # free doc from memory
  free(doc)

  # return data frame
  return(df)
}


###--- EXAMPLE ---###

u <- "http://uk.search.yahoo.com/search;_ylt=A7x9QV6rWrxOYTsAHNFLBQx.?fr2=time&rd=r1&fr=yfp-t-702&p=Wil%20Wheaton&btf=w"
df <- get_yahoo_search_df(u)

t(df[1, ])

# title       "Wil Wheaton - Google+"
# url         "https://plus.google.com/108176814619778619437"
# description "Wil Wheaton - Google+6 days ago"
# cached      "http://87.248.112.8/search/srpcache?ei=UTF-8&p=Wil+Wheaton&rd=r1&fr=yfp-t-702&u=http://cc.bingj.com/cache.aspx?q=Wil+Wheaton&d=4592664708059042&mkt=en-GB&setlang=en-GB&w=48d4b732,65b6306b&icp=1&.intl=uk&sig=6lwcOA8_4oGClQam_5I0cA--"
# recorded    "6 days ago"
