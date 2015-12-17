
#'@title Search Guardian Content
#'@description \code{guardian_content} lets you directly search
#'The Guardian for content, including support for conditional
#'searches, date-range based filtering, and section or tag
#'based filtering.
#'
#'@param api_key A key to the Guardian API, which can be obtained
#'\href{http://open-platform.theguardian.com/access/}{here}.
#'
#'@param query Your search query. This can contain operators (\code{sausage AND mash}) or
#'phrases (\code{"sausage & mash"}); by default, searches work as an OR, looking for
#'the presence of any one individual word in the query.
#'
#'@param from the date to limit by. If set, the search will only cover data written after this date.
#'Optional (NULL) by default. Can be set in conjunction with \code{to}.
#'
#'@param to Another date to limit by. If set, the search will only cover data written before this date.
#'Optional (NULL) by default. Can be set in conjunction with \code{from}.
#'
#'@export
guardian_content <- function(api_key, query, from = NULL, to = NULL, section = NULL,
                             reference = NULL, reference_type = NULL, tag = NULL,
                             rights = NULL, ids = NULL, production_office = NULL, page = NULL,
                             page_size = 50, fields = NULL, ...){
  
  # Construct basic path
  path <- paste0("/search?q=", query, "&api-key=", api_key)
  
  # Check dates
  if(!is.null(from)){
    path <- paste0(path, "&from-date=", date_convert(from))
  }
  if(!is.null(to)){
    path <- paste0(path, "&to-date=", date_convert(to))
  }
  
  # Filter types
  if(!is.null(section)){
    path <- paste0(path, "&section=", section)
  }
  if(!is.null(reference)){
    path <- paste0(path, "&reference=", reference)
  }
  if(!is.null(reference_type)){
    path <- paste0(path, "&reference-type=", reference_type)
  }
  if(!is.null(tag)){
    path <- paste0(path, "&tag=", tag)
  }
  if(!is.null(rights)){
    path <- paste0(path, "&rights=", rights)
  }
  if(!is.null(rights)){
    path <- paste0(path, "&ids=", rights)
  }
  if(!is.null(production_office)){
    path <- paste0(path, "&production-office=", production_office)
  }
  
  # Pagination
  if(!is.null(page)){
    path <- paste0(path, "&page=", page)
  }
  if(!is.null(page_size)){
    path <- paste0(path, "&page-size=", page_size)
  }
  
  # Fields
  if(!is.null(fields)){
    path <- paste0(path, "&show-fields=", merge_multis(fields))
  }
  
  return(guardian_query(path))
}
