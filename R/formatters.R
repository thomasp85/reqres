#' Pre-supplied formatting generators
#'
#' This set of functions can be used to construct formatting functions adhering
#' to the Response$format() requirements.
#'
#' @return A function accepting an R object
#'
#' @rdname formatters
#' @name formatters
#'
#' @seealso [parsers] for converting `Request` bodies into R objects
#' @seealso [default_formatters] for a list that maps the most common mime types
#' to their respective formatters
#'
#' @examples
#' fake_rook <- test <- fiery::fake_request(
#'   'http://example.com/test',
#'   content = '',
#'   headers = list(
#'     Content_Type = 'text/plain',
#'     Accept = 'application/json, text/csv'
#'   )
#' )
#'
#' req <- Request$new(fake_rook)
#' res <- req$respond()
#' res$body <- mtcars
#' res$format(json = format_json(), csv = format_table(sep=','))
#' res$body
#'
NULL

#' @rdname formatters
#'
#' @inheritParams jsonlite::toJSON
#'
#' @importFrom jsonlite toJSON
#' @export
format_json <- function(dataframe = 'rows', matrix = 'rowmajor', Date = 'ISO8601',
                        POSIXt = 'string', factor = 'string', complex = 'string',
                        raw = 'base64', null = 'list', na = 'null',
                        auto_unbox = FALSE, digits = 4, pretty = FALSE, force = FALSE) {
  function(x) {
    toJSON(x,dataframe = dataframe, matrix = matrix, Date = Date,
           POSIXt = POSIXt, factor = factor, complex = complex,
           raw = raw, null = null, na = na, auto_unbox = auto_unbox,
           digits = digits, pretty = pretty, force = force)
  }
}
#' @rdname formatters
#'
#' @param sep The line separator. Plain text will be split into multiple strings
#' based on this.
#'
#' @export
format_plain <- function(sep = '\n') {
  function(x) {
    paste(as.character(unlist(x)), collapse = sep)
  }
}
#' @rdname formatters
#'
#' @inheritParams xml2::write_xml
#'
#' @importFrom xml2 as_xml_document
#' @export
format_xml <- function(encoding = 'UTF-8', options = 'as_xml') {
  options <- union('as_xml', options)
  function(x) {
    as.character(as_xml_document(list(listify(x))), encoding = encoding, options = options)
  }
}
#' @rdname formatters
#'
#' @importFrom xml2 as_xml_document
#' @export
format_html <- function(encoding = 'UTF-8', options = 'as_html') {
  options <- union('as_html', options)
  function(x) {
    as.character(as_xml_document(list(listify(x))), encoding = encoding, options = options)
  }
}
#' @rdname formatters
#'
#' @param ... parameters passed on to [write.table()]
#'
#' @importFrom utils write.table capture.output
#' @export
format_table <- function(...) {
  function(x) {
    paste(capture.output(write.table(x, file = '', ...)), collapse = '\n')
  }
}
#' A list of default formatter mappings
#'
#' This list matches the most normal mime types with their respective formatters
#' using default arguments. For a no-frills request parsing this can be supplied
#' directly to `Response$format()`. To add or modify to this list simply supply
#' the additional parsers as second, third, etc, argument and they will
#' overwrite or add depending on whether it specifies a mime type already
#' present.
#'
#' @format NULL
#' @export
#'
#' @seealso [formatters] for an overview of the build in formatters in `reqres`
#'
#' @examples
#' \dontrun{
#' res$format(default_formatters, 'text/plain' = format_plain(sep = ' '))
#' }
#'
default_formatters <- list(
  `application/json` = format_json(),
  `text/plain` = format_plain(),
  `application/xml` = format_xml(),
  `text/xml` = format_xml(),
  `application/html` = format_html(),
  `text/html` = format_html(),
  `text/csv` = format_table(sep = ','),
  `text/tab-separated-values` = format_table(sep = '\t')
)

# Format R objects to xml2 compliant lists
listify <- function(x) {
  if (is.scalar(x)) return(structure(list(as.character(x))))
  if (!is.list(x)) x <- as.list(x)
  if (!has_attr(x, 'names')) names(x) <- vapply(x, function(x) class(x)[1], character(1))
  lapply(x, listify)
}
