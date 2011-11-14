# Filename: GScholarXScraper.R
# Author: Kay Cichini
# Modified by: Tony Breyal
# Contributations: Philipp Riemer - improvements to the xpathLVApply function code, see http://tonybreyal.wordpress.com/2011/11/11/web-scraping-yahoo-search-page-via-xpath/#comment-45
# Licence: Kay specified the original being non-comercial. As one specific licence wasn't specifially choosen, I choose the one below as the closest to what I beleve he meant.
# Description: Returns a word cloud for a Google Scholar web search based on a given field e.g. "title", "description", etc. and an input search string
# Notes: This is Kay Cichini's function but I modified it to use XPath expressions via the XML package instead of regular expresions alone. I also added some vectorisation, word stemming and restructured the function to make it easier for me to read.
# Blog Reference: http://tonybreyal.wordpress.com/2011/11/14/gscholarxscraper-hacking-the-gscholarscraper-function-with-xpath/
# Other References: http://thebiobucket.blogspot.com/2011/11/r-function-google-scholar-webscraper.html
#                   https://docs.google.com/document/d/1w_7niLqTUT0hmLxMfPEB7pGiA6MXoZBy6qPsKsEe_O0/edit?hl=en_US

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


GScholarXScraper <- function(search.str, field = "title", write.table = FALSE, stem = TRUE) {
  ###--- REQUIRED PACKAGES ---###
  # load packages
  require(Rcpp) # not sure if this is really needed? It was included in Kay;s original function but this modification seems to run fine without it.
  require(RCurl) 
  require(XML)
  require(stringr)
  require(tm)
  require(wordcloud)
  require(Rstem) # I couldn't get the Snowball package to install because of java conflicts, but this does the same job.
  
  
  ###--- LOCAL FUNCTIONS ---###
  # function to get a list of Google Scholar webpages for STEP ONE
  get_GS_webpages <- function(search.str) {    
    # Initial URL
    url <- URLencode(paste("http://scholar.google.com/scholar?start=0&q=", search.str, "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1", sep = "") )
      
    # ...we’re using urls like: http://scholar.google.com/scholar?start=0&q=allintitle:+amphibians+richness+OR+diversity&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1
    html_str <- getURL(url)

    # Pull the number of search results.
    # (!) Strangely Google Scholar gives different numbers of results
    # dependent on start value.. i.e., a change from 900 to 980 results
    # when changing start = 0 to start = 800
    doc <- htmlParse(html_str)
    no.res <- xpathSApply(doc, "/html/body/form/table/tr/td[2]/font/b[3]", xmlValue)
    no.res <- as.integer(gsub(",", "", no.res))
      
    # If there are no results, stop and throw an error message:
    if(length(no.res) == 0 | is.na(no.res)){stop("\n\n...There is no result for the submitted search string!")}
      
    # Define number of pages with results to be used subsequently
    # pages.max = maximum number of pages (chunk with 100 results each)
    # to be submitted subsequently.
    # Above it was said that no.res varies, depending on start value.
    # However, we use ceiling and the change will very unlikely be greater
    # than 100, so we may also add one page plus, to be save:
    pages.max <- ceiling(no.res/100)+1
      
    # "start" as used in url, defines the i-th result to start the page with
    # start = 0 was already used above so we need 100, 200, ...
    start <- c(100*1:(pages.max-1))
      
    # Construct Google Scholar URLs (one for each pages of resutls)
    urls <- paste("http://scholar.google.com/scholar?start=", start[(2:pages.max)-1],
                  "&q=", search.str,
                  "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1", 
                  sep = "")
    
    # Encode URLs to be valid
    urls <- as.vector(sapply(urls, URLencode, USE.NAMES = FALSE))
    
    # Collect webpages as list; first webpage 'html_str' has already been retrieved above, no need to fetch it again!
    webpages <- list(c(html_str, lapply(urls, getURL)))
      
    # Set up return value, assigning number of results to first element
    mylist <- list()
    mylist[[1]] <- no.res
    mylist[[2]] <- webpages
    
    # Return results and webpages in a list structure
    return(mylist)
  }
  
  # I added a wrapper around xpathSApply to deal with cases return NULL and are thus were removed during the list to vector conversion process. This function ensures the NULLs are replaced by NA
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


  # construct data frame from the html of a single Google Scholar search page
  get_google_scholar_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)
  
    # construct data frame
    xpath.base <- "/html/body/div[@class='gs_r']"
    df <- data.frame(
            footer = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']", xmlValue),
            title = xpathLVApply(doc, xpath.base, "/div[@class='gs_rt']/h3", xmlValue),
            type = xpathLVApply(doc, xpath.base, "/div[@class='gs_rt']/h3/span", xmlValue),
            publication = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_a']", xmlValue),
            description = xpathLVApply(doc, xpath.base, "/font", xmlValue),
            cited.by = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Cited by')]/text()", xmlValue),
            cited.ref = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Cited by')]", xmlAttrs),
            title.url = xpathLVApply(doc,  xpath.base, "/div[@class='gs_rt']/h3/a", xmlAttrs),
            view.as.html = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'View as HTML')]", xmlAttrs),
            view.all.versions = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,' versions')]", xmlAttrs),
            from.domain = xpathLVApply(doc, xpath.base, "/span[@class='gs_ggs gs_fl']/a", xmlValue),
            related.articles = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Related articles')]", xmlAttrs),
            library.search = xpathLVApply(doc, xpath.base, "/font/span[@class='gs_fl']/a[contains(.,'Library Search')]", xmlAttrs),
            result.set = xpathSApply(doc, "/html/body/form/table/tr/td[2]", xmlValue),
            stringsAsFactors = FALSE)
    # free doc from memory
    free(doc)
      
    # Clean up extracted text
    df$title <- sub(".*\\] ", "", df$title)
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$publication[i], "", df$description[i], fixed = TRUE))
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$footer[i], "", df$description[i], fixed = TRUE))
    df$type <- gsub("\\]", "", gsub("\\[", "", df$type))
    df$cited.by <- as.integer(gsub("Cited by ", "", df$cited.by, fixed = TRUE))
      
    # remove footer as it is now redundant after doing clean up  and return dataframe
    return(df[,-1])
  }
  
  # function to make a corpus out of the Google Scholar dataframe
  get_wordfreq_df <- function(GS.df, stem = FALSE) {
    # reference: http://stackoverflow.com/questions/7263478/snowball-stemmer-only-stems-last-word
    wordStem2 <- function(x) {
      mywords <- unlist(strsplit(x, " "))
      mycleanwords <- gsub("^\\W+|\\W+$", "", mywords, perl=T)
      mycleanwords <- mycleanwords[mycleanwords != ""]
      return(paste(wordStem(mycleanwords), collapse = " "))
    }
    
    # should words be stemed
    if(stem) GS.df[[1]] = sapply(1:dim(GS.df)[1], function(i) wordStem2(GS.df[i, ]))
    
    # create  corpus
    corpus <- Corpus(DataframeSource(GS.df))
    
    # clean corpus
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, function(x)removeWords(x, stopwords()))
    
    # reshape data 
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
    
    # Remove strings with numbers:
    df <- df[-grep("[1-9]", df$word), ]
    
    # Remove unwanted rubbish (..to be extended?) 
    rubbish <- c("htmls", "hellip", "amp", "quot")
    df <- df[df$word%in%rubbish == FALSE, ]
    
    # return word-frequency dataframe
    return(df)
  }
  
  ###--- Main ---###
  
  # STEP 1: Get Google Scholar webpages
  mylist <- get_GS_webpages(search.str)
  no.res <- mylist[[1]]
  webpages <- unlist(mylist[[2]])
  rm(mylist)
  
  # STEP 2: Get Google Scholar dataframe
  GS.df <- do.call("rbind", lapply(webpages, get_google_scholar_df))
  
  # STEP 3: reshape GS.df to be the same as original function
  GS.df <- data.frame(field = GS.df[field], row.names = paste("GS_Result", row.names(GS.df), sep = "_"), stringsAsFactors = FALSE)
  
  # STEP 4: get word frequency dataframe
  if(dim(GS.df)[1] < 20) { stop("\n\nThere are less than 20 Results, a word cloud may be useless!") }
  wf.df <- get_wordfreq_df(GS.df, stem = TRUE)
  
  # STEP 5: plot wordcloud!
  wordcloud(wf.df$word, wf.df$freq, random.order = F)

  # STEP 6: Meta information for user
  # Show only frequencies larger than 5:
  print(wf.df[wf.df$freq > 5, ])
  cat(paste("\n\nNumber of titles submitted =", dim(GS.df)[1]))
  
  # Compare retrieved titles and no. of results pulled from first webpage:
  cat(paste("\n\nNumber of results as retrieved from first webpage =", no.res))
  cat("\n\nBe aware that sometimes titles in Google Scholar outputs are truncated - that is why, i.e., some mandatory intitle-search strings may not be contained in all titles\n")
}


# ###--- EXAMPLES ---###
# 
# # EXAMPLE 1: produces a word cloud based the 'title'' field of Google Scholar search results and an input search string
# GScholarXScraper(search.str = "Baldur's Gate", field = "title", write.table = FALSE, stem = TRUE)
# 
# #              word freq
# # game         game   71
# # comput     comput   22
# # video       video   13
# # learn       learn   11
# # [TRUNC...]
# # 
# # 
# # Number of titles submitted = 210
# # 
# # Number of results as retrieved from first webpage = 267
# # 
# # Be aware that sometimes titles in Google Scholar outputs are truncated - that is why, i.e., some mandatory intitle-search strings may not be contained in all titles
# 
# # EXAMPLE 2: produces a word cloud based the 'description' field of Google Scholar search results and an input search string
# GScholarXScraper(search.str = "Baldur's Gate", field = "description", write.table = FALSE, stem = TRUE)
# 
# #                word freq
# # page           page  147
# # gate           gate  132
# # game           game  130
# # baldur       baldur  129
# # roleplay   roleplay   21
# # [TRUNC...]
# # 
# # Number of titles submitted = 210
# # 
# # Number of results as retrieved from first webpage = 267
# # 
# # Be aware that sometimes titles in Google Scholar outputs are truncated - that is why, i.e., some mandatory intitle-search strings may not be contained in all titles
