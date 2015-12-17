#'@importFrom httr stop_for_status GET content user_agent
guardian_query <- function(path, ...){
  url <- paste0("https://content.guardianapis.com/", path)
  result <- httr::GET(url)
  httr::stop_for_status(result)
  return(httr::content(result))
}

date_convert <- function(date){
  
  if(any(c("POSIXlt", "POSIXct") %in% class(date))){
    date <- as.Date(date)
  }
  return(as.character(date))
}

# Merges multiple args
merge_multis <- function(args){
  return(paste0(args, collapse = ","))
}

collapse_content <- function(api_response){
  
  response_names <- names(api_response$results[[1]])
  results <- do.call("rbind", lapply(api_response$results, function(x){
    unlisted_data <- unname(unlist(x))
    return(as.data.frame(matrix(unlisted_data, nrow = 1, ncol = length(unlisted_data)), stringsAsFactors = FALSE))
  }))
  names(results) <- response_names
  if("webPublicationDate" %in% response_names){
    results$webPublicationDate <- as.POSIXct(gsub(x = results$webPublicationDate, pattern = "(T|%Z)",
                                                  replacement = ""), tz = "UTC")
  }
  return(results)
}
