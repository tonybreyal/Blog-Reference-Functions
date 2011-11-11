# Author: Tony Breyal
# Date: 2011-11-10
# Description: This function uses the FB Graph API Explorer to get posts and meta-data for a given Facebook Wall Page. 
# Notes: Currently only works on Windows because I don't know of a cross-platform equivalent of winDialogueString.
# Reference: http://tonybreyal.wordpress.com/2011/11/10/facebook-graph-api-explorer-with-r/



Facebook_Graph_API_Explorer <- function() {
  # load packages
  require(RCurl)
  require(RJSONIO)
  
  # Constructs a data frame from json
  get_json_df <- function(data) {
    l <- list(
        post.id = lapply(data, function(post) post$id),
        from.name = lapply(data, function(post) post$to$data[[1]]$name),
        from.id = lapply(data, function(post) post$to$data[[1]]$id),
        to.name = lapply(data, function(post) post$to$data[[1]]$name),
        to.id = lapply(data, function(post) post$to$data[[1]]$id),
        to.category = lapply(data, function(post) post$to$data[[1]]$category),
        created.time = lapply(data, function(post) as.character(as.POSIXct(post$created_time, origin="1970-01-01", tz="GMT"))),
        message = lapply(data, function(post) post$message),
        type = lapply(data, function(post) post$type),
        likes.count = lapply(data, function(post) post$likes$count),
        comments.count = lapply(data, function(post) post$comments$count),
        sample.comments = lapply(data, function(post) paste(sapply(post$comments$data, function(comment) comment$message), collapse = " [next>>] ")),
        sample.comments.from.name = lapply(data, function(post) paste(sapply(post$comments$data, function(comment) comment$from$name), collapse = " [next>>] ")),
        sample.comments.from.id = lapply(data, function(post) paste(sapply(post$comments$data, function(comment) comment$from$id), collapse = " [next>>] ")),
        sample.comments.created.time = lapply(data, function(post) paste(sapply(post$comments$data, function(comment) as.character(as.POSIXct(comment$created_time, origin="1970-01-01", tz="GMT"))), collapse = " [next>>] ")) )
    
    # replace all occurances of NULL with NA and return data frame
    df = data.frame(do.call("cbind", lapply(l, function(x) sapply(x, function(xx) ifelse(is.null(xx), NA, xx)))))
    return(df)
  }

  # STEP 1: Get certs so we can access https links (we'll delete it at the end of the script)
  if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")

  # STEP 2: Get fackebook token to access data. I need a crossplatform version of winDialog and winDialogString otherwise this only works on Windows
  winDialog(type = "ok", "Make sure you have already signed into Facebook.\n\nWhen  browser opens, please click 'Get Access Token' twice. You DO NOT need to select/check any boxes for a public feed.\n\n After pressing OK, swich over to your now open browser.")
  browseURL("http://developers.facebook.com/tools/explorer/?method=GET&path=100002667499585")
  token <- winDialogString("When  browser opens, please click 'Get Access Token' twice and copy/paste token below", "")

  # STEP 3: Get facebook ID. This can be a fanpage or whatever e.g. https://www.facebook.com/DoctorWho
  ID <- winDialogString("Please enter FB name id below:", "https://www.facebook.com/DoctorWho")
  ID <- gsub(".*com/", "", ID)

  # STEP 4: Construct Facebook Graph API URL
  u <- paste("https://graph.facebook.com/", ID, "/feed", "?date_format=U", "&access_token=", token, sep = "")

  # STEP 5: How far back do you want get data for? Format should be YYYY-MM-DD
  user.last.date <- try(as.Date(winDialogString("Please enter a date for how roughly far back to gather data from using this format: yyyy-mm-dd", "")), silent = TRUE)
  current.last.date <- user.last.date + 1

  # STEP 6: Get data
  df.list <- list()
  i <- 1
  while(current.last.date > user.last.date) {
    # Download the JSON feed
    json <- getURL(u, cainfo = "cacert.perm")
    json <- fromJSON(json, simplify = FALSE)
    data <- json$data
    stopifnot(!is.null(data))

    # Get json Data Frame
    df.list[[i]] <- get_json_df(data)
    i <- i + 1

    # variables for while loop
    current.last.date <- as.Date(as.POSIXct(json$data[[length(json$data)]]$created_time, origin="1970-01-01", tz="GMT"))
    print(paste("Current batch of dates being processed is:", current.last.date, "(loading more...)"))
    u <- json$paging$`next`
  }

  # STEP 7: delete security certificates we downloaded earlier.
  file.remove("cacert.perm")
  
  # STEP 8: Return merged data frame
  df <- do.call("rbind", df.list)
  return(df)
}


###--- EXAMPLE ---###

df <- Facebook_Graph_API_Explorer()
t(df[4,])

# post.id                      "127031120644257_319062954774405"
# from.name                    "Torchwood"
# from.id                      "119328091441982"
# to.name                      "Torchwood"
# to.id                        "119328091441982"
# to.category                  "Tv show"
# created.time                 "2011-11-10 12:05:21"
# message                      "If you're missing Torchwood & Doctor Who and are after some good, action-packed science fiction, why not check out FOX's awesome prehistoric romp, Terra Nova? It's carried in the UK on Sky TV and is well worth catching up with & following! The idea - The Earth is dying, it's in its final years. Life's intolerable & getting worse. Scientists take advantage of a rift in time & space to set up a 'fresh start' colony on Terra Nova - the earth, 60 million years ago. The adventure then begins..."
# type                         "link"
# likes.count                  NA
# comments.count               "0"
# sample.comments              ""
# sample.comments.from.name    ""
# sample.comments.from.id      ""
# sample.comments.created.time ""