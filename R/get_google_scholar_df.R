# Author: Tony Breyal
# Date: 2011-11-08
# Description: This function will retrieve as information as it can about each result on a Google Scholar search page
# Reference: http://tonybreyal.wordpress.com/2011/11/08/web-scraping-google-scholar-part-2-complete-success/

get_google_scholar_df <- function(u) {
  # load packages
  require(RCurl)
  require(XML)
  
  # get web page html
  html <- getURL(u)

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


###--- EXAMPLE ---###

u <- "http://scholar.google.com/scholar?as_q=baldur%27s+gate+2&num=20&btnG=Search+Scholar&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=&as_ylo=&as_yhi=&as_sdt=1.&as_sdtp=on&as_sdtf=&as_sdts=5&hl=en"
df <- get_google_scholar_df(u)
t(df[1, ])

# title             "Baldur's gate and history: Race and alignment in digital role playing games"
# type              "PDF"
# publication       "C Warnes - Digital Games Research Conference (DiGRA), 2005 - digra.org"
# description       "... It is argued that games like Baldur's Gate I and II cannot be properly understood without\nreference to the fantasy novels that inform them. ... Columbia University Press, New York, 2003.\npp 2-3. 12. 8. Hess, Rhyss. Baldur's Gate and Tales of the Sword Coast. ... \n"
# cited_by          "8"
# cited_ref         "/scholar?cites=13835674724285845934&as_sdt=2005&sciodt=0,5&hl=en&oe=ASCII&num=20"
# title_url         "http://digra.org:8080/Plone/dl/db/06276.04067.pdf"
# view_as_html      "http://scholar.googleusercontent.com/scholar?q=cache:rpHocNswAsAJ:scholar.google.com/+baldur%27s+gate+2&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# view_all_versions "/scholar?cluster=13835674724285845934&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# from_domain       "[PDF] from digra.org"
# related_articles  "/scholar?q=related:rpHocNswAsAJ:scholar.google.com/&hl=en&oe=ASCII&num=20&as_sdt=0,5"
# library_search    NA
# result_set        "Results 1 - 20 of about 404.   (0.29 sec)Â "