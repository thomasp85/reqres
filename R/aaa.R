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
  time <- as.POSIXct(time, format = format)
  .Call(fmt_http_time_c, as.integer(time), PACKAGE = "reqres")
}
current_time <- function() {
  .Call(fmt_http_time_c, NULL, PACKAGE = "reqres")
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
#' query_parser("?name=Thomas+Lin+Pedersen&numbers=1%202%203", delim = " ")
#'
#' # No delimiter (exploded form)
#' query_parser("?name=Thomas%20Lin%20Pedersen&numbers=1&numbers=2&numbers=3")
#'
query_parser <- function(query = NULL, delim = NULL) {
  if (is.null(query) || query == '') {
    return(list())
  }
  if (isTRUE(delim == " ")) {
    delim <- "%20"
  }
  check_string(query, allow_null = TRUE)
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

#' Get the mime type associated with a file based on its file extension
#'
#' While file extensions are not universally guaranteed to be tied to the
#' content of a file, they are often indicative of the content to the degree
#' that they can be used if the content type is missing. `mime_type_from_file`
#' gives access to the huge database of mime types and their file extensions
#' that reqres contains. `mime_type_info()` provides the same information but
#' rather than basing the search on a file, you provide the known mime type
#' directly
#'
#' @param filename The name of the file to query
#' @param type The mime type to get additional information on
#'
#' @return A data.frame with a row for each match and the columns:
#' * *name* The mime type
#' * *extensions* The extensions commonly associated with the mime type
#' * *charset* The character set used for the type, if any
#' * *compressible* Is the type known to be compressible
#' * *source* The source of the mime type information
#'
#' @export
#' @keywords internal
mime_type_from_file <- function(filename) {
  ext <- tolower(stringi::stri_match_first_regex(
    filename,
    "\\.([^\\.]+)$",
    cg_missing = ""
  )[, 2])
  mimes[mimes_ext$index[match(ext, mimes_ext$ext)], ]
}
#' @rdname mime_type_from_file
#' @export
mime_type_info <- function(type) {
  mimes[match(type, mimes$name), ]
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
  request <- names(headers) %in% tolower(req_headers)
  response <- names(headers) %in% tolower(res_headers)
  entity <- !request & !response
  list(
    request = headers[request],
    response = headers[response],
    entity = headers[entity]
  )
}
cat_headers <- function(headers) {
  if (length(headers) == 0) {
    return(invisible())
  }
  names(headers) <- gsub(
    "(^|-)([[:alpha:]])",
    "\\1\\U\\2",
    gsub('_', '-', names(headers)),
    perl = TRUE
  )
  headers <- lapply(headers, paste, collapse = ', ')
  for (i in names(headers)) {
    cat(i, ': ', headers[[i]], '\n', sep = '')
  }
  invisible()
}

tri <- function(expr) try_fetch(expr, error = function(e, ...) e)

#' Generate a random key compatible with encryption and decryption in requests and responses
#'
#' The encryption/decryption used in reqres is based on the [sodium](https://github.com/r-lib/sodium)
#' package and requires a 32-bit encryption key encoded as hexadecimal values.
#' While you can craft your own, this function will take care of creating a
#' compliant key using a cryptographically secure pseudorandom number generator
#' from `sodium::helpers()`.
#'
#'
#' Keep your encryption keys safe! Anyone with the key will be able to eavesdrop
#' on your communication and tamper with the information stored in encrypted
#' cookies through man-in-the-middle attacks. The best approach is to use the
#' keyring package to manage your keys, but as an alternative you can store it
#' as environment variables.
#'
#' **NEVER STORE THE KEY IN PLAIN TEXT.**
#'
#' **NEVER PUT THE KEY SOMEWHERE WHERE IT CAN ACCIDENTALLY BE COMMITTED TO GIT OR
#' OTHER VERSION CONTROL SOFTWARE**
#'
#' @return A 32-bit key as a hex-encoded string
#'
#' @export
#'
#' @examplesIf FALSE
#' # Store a key with keyring and use it
#' keyring::key_set_with_value("reqres_key", random_key())
#'
#' rook <- fiery::fake_request("http://example.com")
#'
#' Request$new(rook, key = keyring::key_get("reqres_key"))
#'
random_key <- function() {
  sodium::bin2hex(
    sodium::random(32)
  )
}

#' Collect settings for a session cookie
#'
#' A session cookie is just like any other cookie, but reqres treats this one
#' different, parsing it's value and making it available in the `$session`
#' field. However, the same settings as any other cookies applies and can be
#' given during request initialisation using this function.
#'
#' @note As opposed to regular cookies the session cookie is forced to be HTTP
#' only which is why this argument is missing.
#'
#' @param name The name of the cookie
#' @param expires A POSIXct object given the expiration time of the cookie
#' @param max_age The number of seconds to elapse before the cookie expires
#' @param path The URL path this cookie is related to
#' @param secure Should the cookie only be send over https
#' @param same_site Either `"Lax"`, `"Strict"`, or `"None"` indicating
#' how the cookie can be send during cross-site requests. If this is set to
#' `"None"` then `secure` *must* also be set to `TRUE`
#'
#' @return A `session_cookie_settings` object that can be used during request
#' initialisation. Can be cached and reused for all requests in a server
#'
#' @export
#'
#' @examples
#' session_cookie <- session_cookie()
#'
#' rook <- fiery::fake_request("http://example.com")
#'
#' # A key must be provided for session_cookie to be used
#' Request$new(rook, key = random_key(), session_cookie = session_cookie)
#'
session_cookie <- function(
  name = "reqres",
  expires = NULL,
  max_age = NULL,
  path = NULL,
  secure = NULL,
  same_site = NULL
) {
  check_string(name)
  opts <- cookie(
    "",
    expires = expires,
    http_only = TRUE,
    max_age = max_age,
    path = path,
    secure = secure,
    same_site = same_site
  )
  structure(
    list(
      name = name,
      options = sub("^=", "", opts)
    ),
    class = "session_cookie_settings"
  )
}
#' @rdname session_cookie
#' @param x An object to test
#' @export
is_session_cookie_settings <- function(x) inherits(x, "session_cookie_settings")
#' @export
print.session_cookie_settings <- function(x, ...) {
  cli::cli_text("Settings for a session cookie named {.field {x$name}}")
  cli::cli_text("{.emph Attributes: {sub('; ', '', x$options)}}")
}

status_phrase <- function(code) {
  status$message[match(code, status$code)]
}
status_link <- function(code) {
  status$link[match(code, status$code)]
}

check_scalar <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (length(x) != 1) {
    stop_input_type(x, "a scalar object", arg = arg, call = call)
  }
  invisible(NULL)
}
