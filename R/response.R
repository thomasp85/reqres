#' HTTP Response handling
#'
#' This class handles all functionality involved in crafting a http response.
#' Much of the functionality is inspired by the Request class in Express.js, so
#' [the documentation](https://expressjs.com/en/4x/api.html#res) for this will
#' complement this document. As `reqres` is build on top of the
#' [Rook specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
#' the `Response` object can be converted to a compliant list object to be
#' passed on to e.g. the `httpuv` handler.
#'
#' A `Response` object is always created
#' as a response to a `Request` object and contains a reference to the
#' originating `Request` object. A `Response` is always initialised with a
#' 404 Not Found code, an empty string as body and the `Content-Type` header set
#' to `text/plain`. As the `Content-Type` header is required for `httpuv` to
#' function, it will be inferred if missing when converting to a list. If the
#' body is a raw vector it will be set to `application/octet-stream` and
#' otherwise it will be set to `text/plain`. It is always advised to consciously
#' set the `Content-Type` header though. The only exception is when attaching a
#' standard file where the type is inferred from the file extension
#' automatically. Unless the body is a raw vector it will automatically be
#' converted to a character vector and collapsed to a single string with `"\n"`
#' separating the individual elements before the `Response` object is converted
#' to a list (that is, the body can exist as any type of object up until the
#' moment where the `Response` object is converted to a list). To facilitate
#' communication between different middleware the `Response` object contains
#' a data store where information can be stored during the lifetime of the
#' response.
#'
#' @format NULL
#' @usage NULL
#'
#' @section Initialization:
#' A new 'Response'-object is initialized using the \code{new()} method on the
#' generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{res <- Response$new(request)}
#' }
#'
#' But often it will be provided by the request using the `respond()` method,
#' which will provide the response, creating one if it doesn't exist
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{res <- request$respond()}
#' }
#'
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{request} \tab  \tab The `Request` object that the `Response` is responding to\cr
#' }
#'
#' @section Fields:
#' The following fields are accessible in a `Response` object:
#'
#' \describe{
#'  \item{`status`}{Gets or sets the status code of the response. Is initialised
#'  with `404L`}
#'  \item{`body`}{Set or get he body of the response. If it is a character
#'  vector with a single element named `'file'` it will be interpreted as the
#'  location of a file. It is better to use the `file` field for creating a
#'  response referencing a file as it will automatically set the correct
#'  headers.}
#'  \item{`file`}{Set or get the location of a file that should be used as the
#'  body of the response. If the body is not referencing a file (but contains
#'  something else) it will return `NULL`. The `Content-Type` header will
#'  automatically be inferred from the file extension, if known. If unknown it
#'  will defaults to `application/octet-stream`. If the file has no extension it
#'  will be `text/plain`. Existance of the file will be checked.}
#'  \item{`type`}{Get or sets the `Content-Type` header of the response based on
#'  a file extension or mime-type.}
#'  \item{`request`}{Get the original `Request` object that the object is
#'  responding to.}
#' }
#'
#' @section Methods:
#' The following methods are available in a `Response` object:
#'
#' \describe{
#'  \item{`set_header(name, value)`}{Sets the header given by `name`. `value`
#'  will be converted to character. A header will be added for each element in
#'  `value`. Use `append_header()` for setting headers without overwritting
#'  existing ones.}
#'  \item{`get_header(name)`}{Returns the header(s) given by `name`}
#'  \item{`remove_header(name)`}{Removes all headers given by `name`}
#'  \item{`has_header(name)`}{Test for the existence of any header given by
#'  `name`}
#'  \item{`append_header(name, value)`}{Adds an additional header given by
#'  `name` with the value given by `value`. If the header does not exist yet it
#'  will be created.}
#'  \item{`set_data(key, value)`}{Adds `value` to the internal data store and
#'  stores it with `key`}
#'  \item{`get_data(key)`}{Retrieves the data stored under `key` in the internal
#'  data store.}
#'  \item{`remove_data(key)`}{Removes the data stored under `key` in the
#'  internal data store.}
#'  \item{`has_data(key)`}{Queries whether the data store has an entry given by
#'  `key`}
#'  \item{`attach(file, filename=basename(file), type=NULL)`}{Sets the body to
#'  the file given by `file` and marks the response as a download by setting the
#'  `Content-Disposition` to `attachment; filename=<filename>`. Use the `type`
#'  argument to overwrite the automatic type inference from the file extension.}
#'  \item{`status_with_text(code)`}{Sets the status to `code` and sets the body
#'  to the associated status code description (e.g. `Bad Gateway` for `502L`)}
#'  \item{`set_cookie(name, value, encode = TRUE, expires = NULL, http_only =
#'  NULL, max_age = NULL, path = NULL, secure = NULL, same_site = NULL)`}{Adds
#'  the cookie given by `name` to the given `value`, optionally url encoding it,
#'  along with any additional directives. See
#'  <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie> for a
#'  description of the different directives. If the cookie already exists it
#'  will be overwritten. The validity of the directives will automatically be
#'  checked. `expires` expects a POSIXct object, `http_only` and `secure` expect
#'  a logical, `max_age` expects an integer, `path` a string, and `same_site`
#'  either `"Lax"` or `"Strict"`}
#'  \item{`remove_cookie(name)`}{Removes the cookie named `name` from the
#'  response.}
#'  \item{`has_cookie(name)`}{Queries whether the response contains a cookie
#'  named `name`}
#'  \item{`set_links(links)`}{Sets the `Link` header based on the named list
#'  `links`. The names will be used for the `rel` directive.}
#'  \item{`as_list()`}{Converts the object to a list for further processing by
#'  a Rook compliant server such as `httpuv`. Will set `Content-Type` header if
#'  missing and convert a non-raw body to a single character string.}
#' }
#'
#' @seealso [`Request`] for handling http requests
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.count is.string
#' @importFrom tools file_path_as_absolute file_ext
#' @importFrom urltools url_encode
#' @importFrom brotli brotli_compress
#'
#' @export
#'
#' @examples
#' fake_rook <- test <- fiery::fake_request(
#'   'http://example.com/test?id=34632&question=who+is+hadley',
#'   content = 'This is elaborate ruse',
#'   headers = list(
#'     Accept = 'application/json; text/*',
#'     Content_Type = 'text/plain'
#'   )
#' )
#'
#' req <- Request$new(fake_rook)
#' res <- Response$new(req)
#' res
#'
#' # Set the body to the associated status text
#' res$status_with_text(200L)
#' res$body
#'
#' # Infer Content-Type from file extension
#' res$type <- 'json'
#' res$type
#'
#' # Prepare a file for download
#' res$attach(system.file('DESCRIPTION', package = 'reqres'))
#' res$type
#' res$body
#' res$get_header('Content-Disposition')
#'
Response <- R6Class('Response',
    public = list(
        # Methods
        initialize = function(request) {
            if (!is.null(request$response)) {
                stop('Response already created for this request. Access it using the `response` field', call. = FALSE)
            }
            private$REQUEST = request
            private$STATUS = 404L
            private$HEADERS = new.env(parent = emptyenv())
            private$COOKIES = new.env(parent = emptyenv())
            private$BODY = ''
            private$DATA = new.env(parent = emptyenv())
            self$type <- 'text/plain'
            request$response <- self
        },
        print = function(...) {
            cat('A HTTP response\n')
            cat('===============\n')
            cat('        Status: ', self$status, ' - ', status$Description[match(self$status, status$Code)], '\n', sep = '')
            cat('  Content type: ', self$type, '\n', sep = '')
            cat('\n')
            cat('In response to: ', private$REQUEST$url, '\n', sep = '')
            invisible(self)
        },
        set_header = function(name, value) {
            assert_that(is.string(name))
            assign(as.character(name), as.character(value), envir = private$HEADERS)
            invisible(self)
        },
        get_header = function(name) {
            assert_that(is.string(name))
            private$HEADERS[[name]]
        },
        remove_header = function(name) {
            assert_that(is.string(name))
            if (!self$has_header(name)) {
                warning('No header named ', name, call. = FALSE)
            } else {
                rm(list = name, envir = private$HEADERS)
            }
            invisible(self)
        },
        has_header = function(name) {
            assert_that(is.string(name))
            !is.null(private$HEADERS[[name]])
        },
        append_header = function(name, value) {
            if (self$has_header(name)) {
                value <- c(self$get_header(name), as.character(value))
            }
            self$set_header(name, value)
            invisible(self)
        },
        set_data = function(key, value) {
            assert_that(is.string(key))
            assign(key, value, envir = private$DATA)
            invisible(self)
        },
        get_data = function(key) {
            assert_that(is.string(key))
            private$DATA[[key]]
        },
        remove_data = function(key) {
            assert_that(is.string(key))
            if (!self$has_data(key)) {
                warning('No data named ', key, call. = FALSE)
            } else {
                rm(list = key, envir = private$DATA)
            }
            invisible(self)
        },
        has_data = function(key) {
            !is.null(self$get_data(key))
        },
        timestamp = function() {
            time <- Sys.time()
            self$set_header('Date', to_http_date(time))
            invisible(self)
        },
        attach = function(file, filename = basename(file), type = NULL) {
            self$file <- file
            assert_that(is.string(filename))
            if (!is.null(type)) self$type <- type
            self$set_header('Content-Disposition', paste0('attachment; filename=', filename))
            invisible(self)
        },
        status_with_text = function(code) {
            self$status <- code
            body <- status$Description[match(code, status$Code)]
            if (is.na(body)) body <- as.character(code)
            self$body <- body
            self$type <- 'txt'
            invisible(self)
        },
        set_cookie = function(name, value, encode = TRUE, expires = NULL, http_only = NULL, max_age = NULL, path = NULL, secure = NULL, same_site = NULL) {
            assert_that(is.string(name))
            assert_that(is.scalar(value))
            ascii <- iconv(c(name, value), to = 'ASCII')
            if (anyNA(ascii)) {
                warning('Cookie name and value must only use valid ASCII characters. Cookie "', name, '" not set', call. = FALSE)
            } else {
                if (encode) value <- url_encode(value)
                if (grepl('(^__Secure-)|(^__Host-)', name)) secure <- TRUE
                cookie <- cookie(value, expires, http_only, max_age, path, secure, same_site)
                assign(as.character(name), cookie, envir = private$COOKIES)
            }
            invisible(self)
        },
        remove_cookie = function(name) {
            assert_that(is.string(name))
            if (!self$has_cookie(name)) {
                warning('No cookie named ', name, call. = FALSE)
            } else {
                rm(list = name, envir = private$COOKIES)
            }
            invisible(self)
        },
        has_cookie = function(name) {
            assert_that(is.string(name))
            !is.null(private$COOKIES[[name]])
        },
        set_links = function(links) {
            assert_that(has_attr(links, 'names'))
            url <- paste0('<', unlist(links), '>')
            rel <- paste('rel="', names(links), '"')
            links <- paste(paste0(url, '; ', rel), collapse = ', ')
            self$set_header('Link', links)
            invisible(self)
        },
        format = function(..., autofail = TRUE, compress = TRUE) {
            if (!private$has_body()) return(TRUE)

            formatters <- list(...)
            if (is.list(..1)) {
                first_formatters <- names(formatters)[-1]
                formatters <- modifyList(..1, list(...)[-1])
                first_formatters <- names(formatters) %in% first_formatters
                formatters <- c(formatters[first_formatters], formatters[!first_formatters])
            }
            assert_that(has_attr(formatters, 'names'))

            format <- self$request$accepts(names(formatters))
            if (is.null(format)) {
                if (autofail) self$status_with_text(406L)
                return(FALSE)
            }
            content <- try(formatters[[format]](self$body))
            if (is.error(content)) {
                if (autofail) self$status_with_text(500L)
                return(FALSE)
            }
            self$body <- content
            self$type <- format
            if (compress) self$compress()
            return(TRUE)
        },
        compress = function(priority = c('gzip', 'deflate', 'br', 'identity')) {
            encoding <- self$request$accepts_encoding(priority)
            if (is.null(encoding)) return(FALSE)
            if (!is.string(self$body)) return(FALSE)
            content <- switch(
                encoding,
                identity = self$body,
                gzip = gzip(charToRaw(self$body)),
                deflate = memCompress(charToRaw(self$body)),
                br = brotli_compress(charToRaw(self$body))
            )
            self$body <- content
            self$set_header('Content-Encoding', encoding)
        },
        as_list = function() {
            list(
                status = private$STATUS,
                headers = private$format_headers(),
                body = private$format_body()
            )
        }
    ),
    active = list(
        status = function(code) {
            if (missing(code)) return(private$STATUS)
            if (is.count(code)) {
                if (code < 100L || code > 599L) {
                    stop('Response code out of range', call. = FALSE)
                }
            }
            if (is.string(code)) {
                ind <- match(tolower(code), tolower(status$Description))
                if (is.na(ind)) {
                    stop('Unknown status', call. = FALSE)
                }
                code <- status$Code[ind]
            }
            private$STATUS <- code
        },
        body = function(content) {
            if (missing(content)) return(private$BODY)
            private$BODY <- content
        },
        file = function(path) {
            if (missing(path)) {
                if (length(private$BODY) != 1 || names(private$BODY) != 'file') {
                    return(NULL)
                } else {
                    return(private$BODY[['file']])
                }
            }
            assert_that(is.string(path))
            file <- file_path_as_absolute(path)
            assert_that(file.exists(file))
            self$type <- file_ext(file)
            private$BODY <- c(file = file)
        },
        type = function(type) {
            if (missing(type)) return(self$get_header('Content-Type'))
            if (!grepl('/', type)) {
                content_index <- mimes_ext$index[match(tolower(type), mimes_ext$ext)]
                type <- if (!is.na(content_index)) {
                    mimes$name[content_index]
                } else if (type == '') {
                    'text/plain'
                } else {
                    'application/octet-stream'
                }
            }
            self$set_header('Content-Type', type)
        },
        request = function() {
            private$REQUEST
        }
    ),
    private = list(
        # Data
        REQUEST = NULL,
        STATUS = NULL,
        HEADERS = NULL,
        COOKIES = NULL,
        BODY = NULL,
        DATA = NULL,

        format_headers = function() {
            headers <- as.list(private$HEADERS)
            if (is.null(headers[['Content-Type']])) {
                headers[['Content-Type']] <- if (is.raw(private$BODY)) {
                    'application/octet-stream'
                } else {
                    'text/plain'
                }
            }
            headers <- structure(
                as.list(unlist(headers)),
                names = rep(names(headers), lengths(headers))
            )
            cookies <- as.list(private$COOKIES)
            cookies <- paste0(names(cookies), unlist(cookies))
            c(headers, structure(
                as.list(cookies),
                names = rep('Set-Cookie', length(cookies))
            ))
        },
        format_body = function() {
            if (is.raw(private$BODY)) {
                private$BODY
            } else if (is.scalar(private$BODY) &&
                       'file' %in% names(private$BODY)) {
                private$BODY
            } else {
                paste(as.character(private$BODY), collapse = '\n')
            }
        },
        has_body = function() {
            !is.null(private$BODY) && length(private$BODY) != 0 && !identical(private$BODY, '')
        }
    )
)
#' @rdname Response
#'
#' @usage \method{as.list}{Response}(x, ...)
#' @param x A `Response` object
#' @param ... Ignored
#'
#' @return A rook-compliant list-response (in case of `as.list()`) or a logical
#' indicating whether the object is a `Response` (in case of `is.Response()`)
#'
#' @export
as.list.Response <- function(x, ...) {
    x$as_list()
}
#' @rdname Response
#'
#' @usage is.Response(x)
#'
#' @export
is.Response <- function(x) inherits(x, 'Response')

cookie <- function(value, expires = NULL, http_only = NULL, max_age = NULL, path = NULL, secure = NULL, same_site = NULL) {
    opts <- paste0('=', value)
    if (!is.null(expires)) {
        assert_that(is.scalar(expires))
        opts <- c(opts, paste0('Expires=', to_http_date(expires)))
    }
    if (!is.null(http_only)) {
        assert_that(is.flag(http_only))
        if (http_only) opts <- c(opts, 'HttpOnly')
    }
    if (!is.null(max_age)) {
        assert_that(is.count(max_age))
        opts <- c(opts, paste0('Max-Age=', max_age))
    }
    if (!is.null(path)) {
        assert_that(is.string(path))
        opts <- c(opts, paste0('Path=', path))
    }
    if (!is.null(secure)) {
        assert_that(is.flag(secure))
        if (secure) opts <- c(opts, 'Secure')
    }
    if (!is.null(same_site)) {
        assert_that(is.string(same_site))
        stopifnot(same_site %in% c('Lax', 'Strict'))
        opts <- c(opts, paste0('SameSite=', same_site))
    }
    paste(opts, collapse = '; ')
}
gzip <- function(x) {
    f <- tempfile()
    con <- gzcon(file(f, open = 'wb'))
    writeBin(x, con)
    close(con)
    content <- readBin(f, raw(), file.info(f)$size)
    unlink(f)
    content
}
