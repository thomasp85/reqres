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
#' @importFrom assertthat is.time
#' @rdname http_date
#' @export
#'
#' @examples
#' time <- to_http_date(Sys.time())
#' time
#' from_http_date(time)
to_http_date <- function(time, format = NULL) {
  if (!is.time(time)) {
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
