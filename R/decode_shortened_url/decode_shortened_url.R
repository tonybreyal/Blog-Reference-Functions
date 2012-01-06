# Author: Tony Breyal
# Date: 2011-12-13
# Modified: 2011-12-13
# Description: Decodes a shortened URL
# Packages Used: RCurl   
# Blog Reference: http://tonybreyal.wordpress.com/2011/12/13/unshorten-any-url-created-using-url-shortening-services-decode_shortened_url/

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


decode_short_url <- function(url, ...) {
  # PACKAGES #
  require(RCurl)
  
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) )
    if(inherits(x, 'try-error') | length(grep(".*Location: (\\S+).*", x))<1) {
      return(u)
    } else {
      return(gsub(".*Location: (\\S+).*", "\\1", x))
    }
  }
  
  # MAIN #
  # return decoded URLs
  urls <- c(url, ...)
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}


# EXAMPLE #
decode_short_url("http://tinyurl.com/adcd", 
                 "http://www.google.com",
                 "http://1.cloudst.at/myeg") 



