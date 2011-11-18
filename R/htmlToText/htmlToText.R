# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
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
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


# ###--- EXAMPLES ---###
# # Example 1: url input
# input <- "http://www.google.co.uk/search?gcx=c&sourceid=chrome&ie=UTF-8&q=r+project#pq=%22hello+%3C+world%22&hl=en&cp=5&gs_id=3r&xhr=t&q=phd+comics&pf=p&sclient=psy-ab&source=hp&pbx=1&oq=phd+c&aq=0&aqi=g4&aql=&gs_sm=&gs_upl=&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb&fp=27ff09b2758eb4df&biw=1599&bih=904"
# txt <- htmlToText(input)
# txt
# 
# #r project - Google Search Web Images Videos Maps News Shopping Gmail More Translate Books Finance Scholar Blogs YouTube Calendar Photos Documents Sites Groups Reader Even more » Account Options Sign in Search settings Web History Advanced Search Results  1  -  10  of about  336,000,000  for  r project . Everything More Search Options Show options... Web The  R Project  for Statistical Computing R , also called GNU S, is a strongly functional language and environment to    statistically explore data sets, make many graphical displays of data from custom  ... www. r - project .org/  -  Cached  -  Similar [Trunc...]
