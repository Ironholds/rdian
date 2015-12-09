#' @title Modern API client for The Guardian
#' @name rdian
#' @description This package provides a modern (httr/curl/jsonlite-based) API
#' client for The Guardian (https://guardian.co.uk), allowing useRs to retrieve
#' content and content metadata.
#' @docType package
#' @aliases rdian rdian-package
NULL

#'@importFrom httr stop_for_status GET content user_agent
guardian_query <- function(path, ...){
  url <- paste0("http://content.guardianapis.com/", path)
  result <- httr::GET(url, httr::user_agent("rdian - https://github.com/Ironholds/rdian"))
  httr::stop_for_status(result)
  return(content(result))
}
