#' Pre-supplied parsing generators
#'
#' This set of functions can be used to construct parsing functions adhering to
#' the Request$parse() requirements.
#'
#' @return A function accepting a raw vector and a named list of directives
#'
#' @rdname parsers
#' @name parsers
#'
#' @seealso [formatters] for converting `Response` bodies into compatible types
#' @seealso [default_parsers] for a list that maps the most common mime types
#' to their respective parsers
#'
#' @examples
#' fake_rook <- fiery::fake_request(
#'   'http://example.com/test',
#'   content = '[1, 2, 3, 4]',
#'   headers = list(
#'     Content_Type = 'application/json'
#'   )
#' )
#'
#' req <- Request$new(fake_rook)
#' req$parse(json = parse_json())
#' req$body
#'
#' # Cleaning up connections
#' rm(fake_rook, req)
#' gc()
#'
NULL

#' @rdname parsers
#'
#' @inheritParams jsonlite::fromJSON
#'
#' @importFrom jsonlite fromJSON
#' @export
parse_json <- function(
  simplifyVector = TRUE,
  simplifyDataFrame = simplifyVector,
  simplifyMatrix = simplifyVector,
  flatten = FALSE
) {
  function(raw, directives) {
    fromJSON(
      rawToChar(raw),
      simplifyVector = simplifyVector,
      simplifyDataFrame = simplifyDataFrame,
      simplifyMatrix = simplifyMatrix,
      flatten = flatten
    )
  }
}
#' @rdname parsers
#'
#' @param sep The line separator. Plain text will be split into multiple strings
#' based on this.
#'
#' @export
parse_plain <- function(sep = '\n') {
  function(raw, directives = list()) {
    strsplit(rawToChar(raw), split = sep)[[1]]
  }
}
#' @rdname parsers
#'
#' @inheritParams xml2::read_xml
#'
#' @importFrom xml2 read_xml
#' @export
parse_xml <- function(encoding = '', options = 'NOBLANKS', base_url = '') {
  function(raw, directives = list()) {
    xml2::as_list(read_xml(
      raw,
      encoding = encoding,
      options = options,
      base_url = base_url
    ))
  }
}
#' @rdname parsers
#'
#' @importFrom xml2 read_xml
#' @export
parse_html <- function(
  encoding = '',
  options = c('RECOVER', 'NOERROR', 'NOBLANKS'),
  base_url = ''
) {
  function(raw, directives = list()) {
    xml2::as_list(read_xml(
      raw,
      as_html = TRUE,
      encoding = encoding,
      options = options,
      base_url = base_url
    ))
  }
}
#' @rdname parsers
#'
#' @importFrom webutils parse_multipart
#' @export
parse_multiform <- function() {
  function(raw, directives) {
    parse_multipart(raw, directives$boundary)
  }
}
#' @rdname parsers
#'
#' @param delim The delimiter to use for parsing arrays in non-exploded form.
#' Either `NULL` (no delimiter) or one of `","`, `"|"`, or `" "`
#'
#' @export
parse_queryform <- function(delim = NULL) {
  force(delim)
  function(raw, directives) {
    query_parser(rawToChar(raw), delim)
  }
}
#' @rdname parsers
#'
#' @param ... parameters passed on to [read.table()]
#'
#' @importFrom utils read.table
#' @export
parse_table <- function(...) {
  function(raw, directives) {
    read.table(file = , text = rawToChar(raw), ...)
  }
}
#' A list of default parser mappings
#'
#' This list matches the most normal mime types with their respective parsers
#' using default arguments. For a no-frills request parsing this can be supplied
#' directly to `Request$parse()`. To add or modify to this list simply supply
#' the additional parsers as second, third, etc, argument and they will
#' overwrite or add depending on whether it specifies a mime type already
#' present.
#'
#' @format NULL
#' @export
#'
#' @seealso [parsers] for an overview of the build in parsers in `reqres`
#'
#' @examples
#' \dontrun{
#' req$parse(default_parsers, 'application/json' = parse_json(flatten = TRUE))
#' }
#'
default_parsers <- list(
  `application/json` = parse_json(),
  `text/plain` = parse_plain(),
  `application/xml` = parse_xml(),
  `text/xml` = parse_xml(),
  `application/html` = parse_html(),
  `text/html` = parse_html(),
  `multipart/form-data` = parse_multiform(),
  `application/x-www-form-urlencoded` = parse_queryform(),
  `text/csv` = parse_table(sep = ',', header = TRUE, stringsAsFactors = FALSE),
  `text/tab-separated-values` = parse_table(
    sep = '\t',
    header = TRUE,
    stringsAsFactors = FALSE
  )
)
