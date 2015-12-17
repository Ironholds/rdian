
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
#'@param section the section, or sections, of \emph{The Guardian} that you want to limit the search to. Multiple
#'sections may be concatenated together using boolean operators; see \code{\link{rdian_and}} and \code{\link{rdian_or}}.
#'
#'@param reference the references to limit the search to; only articles that include those references (and meet other
#'conditions) will be returned. Also accepts boolean operators.
#'
#'@param reference_type the type of reference (such as \code{isbn}). Also accepts boolean operators.
#'
#'@param tags the tags to limit the search to; only articles that contain these tags will be returned. Also accepts
#'boolean operators.
#'
#'@param rights limit a search to content with these rights (syndicatable content, for example). Options are
#'\code{syndicatable} and \code{subscription-database}. Does not accept boolean operators.
#'
#'@param ids limit a search to content with these IDs, an ID being the standard URL fragment after \code{guardian.com} (
#'or \code{co.uk}, or...). Does not accept boolean operators.
#'
#'@param production_office
#'
#'@param page
#'
#'@param page_size
#'
#'@param fields
#'
#'@param ... further arguments to pass to httr's \code{GET}.
#'
#'@export
#'@importFrom curl curl_escape
guardian_content <- function(api_key, query, from = NULL, to = NULL, section = NULL,
                             reference = NULL, reference_type = NULL, tags = NULL,
                             rights = NULL, ids = NULL, production_office = NULL, page = NULL,
                             page_size = 50, fields = NULL, ...){
  
  # Construct basic path
  path <- paste0("search?q=", curl::curl_escape(query), "&api-key=", api_key)
  
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
  if(!is.null(tags)){
    path <- paste0(path, "&tag=", tags)
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

  return(guardian_query(path, ...)[[1]])
}
