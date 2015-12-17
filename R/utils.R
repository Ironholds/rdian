#'@importFrom httr stop_for_status GET content user_agent
guardian_query <- function(path, ...){
  url <- paste0("http://content.guardianapis.com/", path)
  result <- httr::GET(url, httr::user_agent("rdian - https://github.com/Ironholds/rdian"))
  httr::stop_for_status(result)
  return(content(result))
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
