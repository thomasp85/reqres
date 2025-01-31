#' Format timestamps to match the HTTP specs
#'
#' Dates/times in HTTP headers needs a specific format to be valid, and is
#' furthermore always given in GMT time. These two functions aids in converting
#' back and forth between the required format.
#'
#' @param time A string or an object coercible to POSIXct
#' @param format In case `time` is not a POSIXct object a specification how the
#' string should be interpreted.
#'
#' @return `to_http_date()` returns a properly formatted string, while
#' `from_http_date()` returns a POSIXct object
#'
#' @rdname http_date
#' @export
#'
#' @examples
#' time <- to_http_date(Sys.time())
#' time
#' from_http_date(time)
to_http_date <- function(time, format = NULL) {
  if (!inherits(time, "POSIXt")) {
    time <- as.POSIXct(time, format = format)
  }
  format(
    time,
    tz = 'GMT',
    format = '%a, %d %b %Y %H:%M:%S %Z'
  )
}
#' @rdname http_date
#' @export
from_http_date <- function(time) {
  as.POSIXct(time, format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
}

#' Parse a query string
#'
#' This function facilitates the parsing of querystrings, either from the URL or
#' a POST or PUT body with `Content-Type` set to
#' `application/x-www-form-urlencoded`.
#'
#' @param query The query as a single string
#' @param delim Optional delimiter of array values. If omitted it is expected
#' that arrays are provided in exploded form (e.g. `arg1=3&arg1=7`)
#'
#' @return A named list giving the keys and values of the query. Values fron the
#' same key are combined if given multiple times
#'
#' @export
#'
#' @examples
#' # Using delimiter to provide array
#' query_parser("?name=Thomas%20Lin%20Pedersen&numbers=1 2 3", delim = " ")
#'
#' # No delimiter (exploded form)
#' query_parser("?name=Thomas%20Lin%20Pedersen&numbers=1&numbers=2&numbers=3")
#'
query_parser <- function(query = NULL, delim = NULL) {
  check_string(query, allow_null = TRUE)
  if (is.null(query) || query == '') return(list())
  query <- stringi::stri_replace_first_regex(query, '^\\?', '')
  query <- stringi::stri_split_fixed(query, '&')[[1]]
  query <- stringi::stri_split_fixed(query, '=')
  id <- rep(seq_along(query), lengths(query))
  query <- unlist(query)
  if (!is.null(delim)) {
    query <- stringi::stri_split_fixed(query, delim)
    id <- rep(id, lengths(query))
    query <- unlist(query)
  }
  query <- stringi::stri_replace_all_fixed(query, '+', ' ')
  query <- url_decode(query)
  key_ind <- which(!duplicated(id))
  key <- query[key_ind]
  final_keys <- unique(key)
  final <- structure(vector("list", length(final_keys)), names = final_keys)
  value_loc <- match(key, final_keys)[id[-key_ind]]
  final[unique(value_loc)] <- split(query[-key_ind], value_loc)
  final
}


req_headers <- c(
  'Accept',
  'Accept_Charset',
  'Accept_Encoding',
  'Accept_Language',
  'Authorization',
  'Expect',
  'From',
  'Host',
  'If_Match',
  'If_Modified_Since',
  'If_None_Match',
  'If_Range',
  'If_Unmodified_Since',
  'Max_Forwards',
  'Proxy_Authorization',
  'Range',
  'Referer',
  'TE',
  'User_Agent',
  'Cache-Control',
  'Connection',
  'Date',
  'Pragma',
  'Trailer',
  'Transfer_Encoding',
  'Upgrade',
  'Via',
  'Warning'
)
res_headers <- c(
  'Accept_Ranges',
  'Age',
  'ETag',
  'Location',
  'Proxy_Authenticate',
  'Retry_After',
  'Server',
  'Vary',
  'WWW_Authenticate',
  'Cache-Control',
  'Connection',
  'Date',
  'Pragma',
  'Trailer',
  'Transfer_Encoding',
  'Upgrade',
  'Via',
  'Warning'
)
split_headers <- function(headers) {
  request <- names(headers) %in% req_headers
  response <- names(headers) %in% res_headers
  entity <- !request & !response
  list(
    request = headers[request],
    response = headers[response],
    entity = headers[entity]
  )
}
cat_headers <- function(headers) {
  if (length(headers) == 0) return()
  names(headers) <- gsub('_', '-', names(headers))
  headers <- lapply(headers, paste, collapse = ', ')
  for(i in names(headers)) {
    cat(i, ': ', headers[[i]], '\n', sep = '')
  }
}

tri <- function(expr) try_fetch(expr, error = function(e, ...) e)
