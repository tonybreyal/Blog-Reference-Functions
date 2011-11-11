# Author: Kay Cichini
# Hacker: Tony Breyal
# Description: Returns the html of Google Scholar wepages in a list format
# Notes: Not published on my blog, see references
# Reference: http://thebiobucket.blogspot.com/2011/11/r-function-google-scholar-webscraper.html
#            https://docs.google.com/document/d/1w_7niLqTUT0hmLxMfPEB7pGiA6MXoZBy6qPsKsEe_O0/edit?hl=en_US


get_GS_webpages <- function(search.str) {
  # load packages
  require(RCurl)
  require(stringr)
  
  # Initial URL
  url <- paste("http://scholar.google.com/scholar?start=0&q=", search.str, 
               "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1",
               sep = "")
  
  # ...we’re using urls like: http://scholar.google.com/scholar?start=0&q=allintitle:+amphibians+richness+OR+diversity&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1
  html_str <- getURL(url)
  
  # Find html place holders (2 alternatives!) for number of results,
  # and pull the number.
  # (!) Strangely Google Scholar gives different numbers of results
  # dependent on start value.. i.e., a change from 900 to 980 results
  # when changing start = 0 to start = 800
  match_no.res <- str_match(html_str, "Results <b>1</b> - <b>(.*?)</b> of <b>(.*?)</b>")
  no.res <- match_no.res[1, max(dim(match_no.res))]
  
  # stop if no search results found
  if(length(no.res) == 0 | is.na(no.res)){
   match_no.res <- str_match(html_str, "Results <b>1</b> - <b>(.*?)</b> of about <b>(.*?)</b>")
   no.res <- match_no.res[1, max(dim(match_no.res))]
  }
  
  # Remove punctuation (Google uses decimal commas):
  no.res <- as.integer(gsub("[[:punct:]]", "", no.res))
  
  # If there are no results, stop and throw an error message:
  if(length(no.res) == 0 | is.na(no.res)){stop("\n\n...There is no result for the submitted search string!")}
  
  # Define number of pages with results to be used subsequently
  # pages.max = maximum number of pages (chunk with 100 results each)
  # to be submitted subsequently.
  # Above it was said that no.res varies, depending on start value.
  # However, we use ceiling and the change will very unlikely be greater
  # than 100, so we may also add one page plus, to be save:
  pages.max <- ceiling(as.integer(no.res)/100)+1
  
  # "start" as used in url, defines the i-th result to start the page with
  # start = 0 was already used above so we need 100, 200, ...
  start <- c(100*1:(pages.max-1))
  
  # Collect webpages as list, the first was already retrieved and is assigned to first
  # list-element. the rest will be assigned in th below for loop:
  urls <- paste("http://scholar.google.com/scholar?start=", start[(2:pages.max)-1],
                "&q=", search.str,
                "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1", 
                sep = "")
  
  webpages <- lapply(urls, getURL)
  
  # return webpages
  return(c(html_str, webpages))
}


###--- EXAMPLE ---###

search.str <- "allintitle:+amphibians+richness+OR+diversity"
webpages <- get_GS_webpages(search.str)

str(webpages)

List of 3
#  $ : chr "<html><head><meta http-equiv=\"content-type\" content=\"text/html;charset=ISO-8859-1\"><meta http-equiv=\"imagetoolbar\" conten"| __truncated__
#  $ : chr "<html><head><meta http-equiv=\"content-type\" content=\"text/html;charset=ISO-8859-1\"><meta http-equiv=\"imagetoolbar\" conten"| __truncated__
#  $ : chr "<html><head><meta http-equiv=\"content-type\" content=\"text/html;charset=ISO-8859-1\"><meta http-equiv=\"imagetoolbar\" conten"| __truncated__
