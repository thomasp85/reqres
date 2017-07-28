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
#' `from_http_date()` returns a POSIXCct object
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
