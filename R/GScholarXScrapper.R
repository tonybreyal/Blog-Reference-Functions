# Author: Kay Cichini
# Modified by: Tony Breyal
# Description: Returns a wordcloud for a Google Scholar web search base on title.
# Notes: This is Kay Cichini's function but I modified it to use XPath expressions via the XML package (see get_google_scholar_df.R) instead of just regular expresions alone. I also added some vectorisation and restructured the function to make it easier for me to read and understand what is happening.
# References: http://thebiobucket.blogspot.com/2011/11/r-function-google-scholar-webscraper.html
#             https://docs.google.com/document/d/1w_7niLqTUT0hmLxMfPEB7pGiA6MXoZBy6qPsKsEe_O0/edit?hl=en_US


GScholarXScraper <- function(search.str, field = "title", write.table = FALSE, stem = TRUE) {
  ###--- REQUIRED PACKAGES ---###
  # load packages
  require(Rcpp)
  require(RCurl)
  require(XML)
  require(stringr)
  require(tm)
  require(wordcloud)
  require(Rstem)
  
  
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
      
    # Collect webpages as list, the first was already retrieved and is assigned to first
    # list-element. the rest will be assigned in th below for loop:
    urls <- paste("http://scholar.google.com/scholar?start=", start[(2:pages.max)-1],
                  "&q=", search.str,
                  "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1", 
                  sep = "")
    urls <- as.vector(sapply(urls, URLencode, USE.NAMES = FALSE))
    webpages <- lapply(urls, getURL)
      
    # return webpages
    mylist <- list()
    mylist[[1]] <- no.res 
    mylist[[2]] <- list(c(html_str, webpages))
    return(mylist)
  }

  # function to convert all results on a Google Schoolar webpage into a dataframe
  get_google_scholar_df <- function(html) {
    # parse HTML into tree structure
    doc <- htmlParse(html)
    
    # I hacked my own version of xpathSApply to deal with cases that return NULL which were causing me problems
    GS_xpathSApply <- function(doc, path, FUN) {
      path.base <- "/html/body/div[@class='gs_r']"
      nodes.len <- length(xpathSApply(doc, "/html/body/div[@class='gs_r']"))
      paths <- sapply(1:nodes.len, function(i) gsub( "/html/body/div[@class='gs_r']", paste("/html/body/div[@class='gs_r'][", i, "]", sep = ""), path, fixed = TRUE))
      xx <- sapply(paths, function(xpath) xpathSApply(doc, xpath, FUN), USE.NAMES = FALSE)
      xx[sapply(xx, length)<1] <- NA
      xx <- as.vector(unlist(xx))
      return(xx)
    }
    
    # construct data frame
    df <- data.frame(
            footer = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']", xmlValue),
            title = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/div[@class='gs_rt']/h3", xmlValue),
            type = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/div[@class='gs_rt']/h3/span", xmlValue),
            publication = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_a']", xmlValue),
            description = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font", xmlValue),
            cited_by = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,'Cited by')]/text()", xmlValue),
            cited_ref = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,'Cited by')]", xmlAttrs),
            title_url = GS_xpathSApply(doc,  "/html/body/div[@class='gs_r']/div[@class='gs_rt']/h3/a", xmlAttrs),
            view_as_html = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,'View as HTML')]", xmlAttrs),
            view_all_versions = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,' versions')]", xmlAttrs),
            from_domain = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/span[@class='gs_ggs gs_fl']/a", xmlValue),
            related_articles = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,'Related articles')]", xmlAttrs),
            library_search = GS_xpathSApply(doc, "/html/body/div[@class='gs_r']/font/span[@class='gs_fl']/a[contains(.,'Library Search')]", xmlAttrs),
            result_set = xpathSApply(doc, "/html/body/form/table/tr/td[2]", xmlValue),
            stringsAsFactors = FALSE)
    
    # Clean up extracted text
    df$title <- sub(".*\\] ", "", df$title)
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$publication[i], "", df$description[i], fixed = TRUE))
    df$description <- sapply(1:dim(df)[1], function(i) gsub(df$footer[i], "", df$description[i], fixed = TRUE))
    df$type <- gsub("\\]", "", gsub("\\[", "", df$type))
    df$cited_by <- as.integer(gsub("Cited by ", "", df$cited_by, fixed = TRUE))
    
    # remove footer as it is now redundant after doing clean up
    df <- df[,-1]
    
    # free doc from memory
    free(doc)
    
    # return data frame
    return(df)
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
    
    # create corpus
    corpus <- Corpus(DataframeSource(GS.df))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, function(x)removeWords(x, stopwords()))
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
    
    # Remove strings with numbers:
    df <- df[-grep("[1-9]", df$word), ]
    
    # Remove unwanted rubbish (..to be extended?):
    rubbish <- c("htmls", "hellip", "amp", "quot")
    df <- df[df$word%in%rubbish == FALSE, ]
    
    # return word-frequency dataframe
    return(df)
  }
  
  ###--- Main ---###
  
  # STEP 1: Get Google Scholar webpages
  mylist <- get_GS_webpages(search.str)
  no.res <- mylist[[1]]
  webpages <- mylist[[2]]
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


GScholarXScraper(search.str = "Baldur's Gate", field = "title", write.table = FALSE, stem = TRUE)
GScholarXScraper(search.str = "Baldur's Gate", field = "description", write.table = FALSE, stem = TRUE)