#' HTTP Request Handling
#'
#' This class wraps all functionality related to extracting information from a
#' http request. Much of the functionality is inspired by the Request class in
#' Express.js, so [the documentation](https://expressjs.com/en/4x/api.html#req)
#' for this will complement this document. As `reqres` is build on top of the
#' [Rook specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
#' the `Request` object is initialised from a Rook-compliant object. This will
#' often be the request object provided by the `httpuv` framework. While it
#' shouldn't be needed, the original Rook object is always accessible and can be
#' modified, though any modifications will not propagate to derived values in
#' the `Request` object (e.g. changing the `HTTP_HOST` element of the Rook
#' object will not change the `host` field of the `Request` object). Because of
#' this, direct manipulation of the Rook object is generally discouraged.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'Request'-object is initialized using the \code{new()} method on the
#' generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{req <- Request$new(rook, trust = FALSE)}
#' }
#'
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{rook} \tab  \tab The rook request that the new object should wrap\cr
#'  \code{trust} \tab  \tab Is this request trusted blindly. If `TRUE` `X-Forwarded-*` headers will be returned when quering host, ip, and protocol
#' }
#'
#' @section Fields:
#' The following fields are accessible in a `Request` object:
#'
#' \describe{
#'  \item{`trust`}{A logical indicating whether the request is trusted. *Mutable*}
#'  \item{`method`}{A string indicating the request method (in lower case, e.g.
#'  'get', 'put', etc.). *Immutable*}
#'  \item{`body`}{An object holding the body of the request. This is an empty
#'  string by default and needs to be populated using the `set_body()` method
#'  (this is often done using a body parser that accesses the Rook$input
#'  stream). *Immutable*}
#'  \item{`cookies`}{Access a named list of all cookies in the request. These
#'  have been URI decoded. *Immutable*}
#'  \item{`headers`}{Access a named list of all headers in the request. In order
#'  to follow R variable naming standards `-` have been substituted with `_`.
#'  Use the `get_header()` method to lookup based on the correct header name.
#'  *Immutable*}
#'  \item{`host`}{Return the domain of the server given by the "Host" header if
#'  `trust == FALSE`. If `trust == true` returns the `X-Forwarded-Host` instead.}
#'  \item{`ip`}{Returns the remote address of the request if `trust == FALSE`.
#'  if `trust == TRUE` it will instead return the first value of the
#'  `X-Forwarded-For` header. *Immutable*}
#'  \item{`ips`}{If `trust == TRUE` it will return the full list of ips in the
#'  `X-Forwarded-For` header. If `trust == FALSE` it will return an empty
#'  vector. *Immutable*}
#'  \item{`protocol`}{Returns the protocol (e.g. 'http') used for the request.
#'  If `trust == TRUE` it will use the value of the `X-Forwarded-Proto` header.
#'  *Immutable*}
#'  \item{`root`}{The mount point of the application recieving this request. Can
#'  be empty if the application is mounted on the server root. *Immutable*}
#'  \item{`path`}{The part of the url following the root. Defines the local
#'  target of the request (irrespectible of where it is mounted). *Immutable*}
#'  \item{`url`}{The full URL of the request. *Immutable*}
#'  \item{`query`}{The query string of the request (anything following "?" in
#'  the URL) parsed into a named list. The query has been url decoded and "+"
#'  has been substituted with space. Multiple queries are expected to be
#'  separated by either "&" or "|". *Immutable*}
#'  \item{`xhr`}{A logical indicating whether the `X-Requested-With` header
#'  equals `XMLHttpRequest` thus indicating that the request was performed using
#'  a JavaScript library such as jQuery. *Immutable*}
#'  \item{`secure`}{A logical indicating whether the request was performed using
#'  a secure connection, i.e. `protocol == 'https'`. *Immutable*}
#'  \item{`rook`}{The original rook object used to create the `Request` object.
#'  *Immutable*, though the content of the rook object itself might be
#'  manipulated as it is an environment.}
#' }
#'
#' @section Methods:
#' The following methods are available in a `Request` object:
#'
#' \describe{
#'  \item{`set_body(content)`}{Sets the content of the request body. This method
#'  should mainly be used in concert with a body parser that reads the
#'  `rook$input` stream}
#'  \item{`set_cookies(cookies)`}{Sets the cookies of the request. The cookies
#'  are automatically parsed and populated, so this method is mainly available
#'  to facilitate cookie signing and encryption}
#'  \item{`get_header(name)`}{Get the header of the specified name.}
#'  \item{`accepts(types)`}{Given a vector of response content types it returns
#'  the preferred one based on the `Accept` header.}
#'  \item{`accepts_charsets(charsets)`}{Given a vector of possible character
#'  encodings it returns the preferred one based on the `Accept-Charset`
#'  header.}
#'  \item{`accepts_encoding(encoding)`}{Given a vector of possible content
#'  encodings (usually compression algorithms) it selects the preferred one
#'  based on the `Accept-Encoding` header.}
#'  \item{`accepts_language(language)`}{Given a vector of possible content
#'  languages it selects the best one based on the `Accept-Language` header.}
#'  \item{`is(type)`}{Queries whether the body of the request is in a given
#'  format by looking at the `Content-Type` header. Used for selecting the best
#'  parsing method.}
#' }
#'
#' @seealso [`Response`] for handling http responses
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.flag has_attr
#' @importFrom stringi stri_match_first_regex
#' @importFrom urltools url_decode
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
#'
#' # Get full URL
#' req$url
#'
#' # Get list of query parameters
#' req$queries
#'
#' # Test if content is text
#' req$is('txt')
#'
#' # Perform content negotiation for the response
#' req$accepts(c('html', 'json', 'txt'))
#'
Request <- R6Class('Request',
    public = list(
        initialize = function(rook, trust = FALSE) {
            self$trust <- trust
            private$ROOK <- rook
            private$METHOD <- tolower(rook$REQUEST_METHOD)
            private$HEADERS <- private$get_headers(rook)
            if (is.null(rook$HTTP_HOST)) {
                private$HOST <- paste(rook$SERVER_NAME, rook$SERVER_PORT, sep = ':')
            } else {
                private$HOST <- rook$HTTP_HOST
            }
            private$IP <- rook$REMOTE_ADDR
            private$URL <- paste0(rook$rook.url_scheme, '://',
                                  private$HOST,
                                  rook$SCRIPT_NAME,
                                  rook$PATH_INFO,
                                  if(rook$QUERY_STRING == '') ''
                                  else paste0('?', rook$QUERY_STRING))
            private$QUERY <- private$parse_query(rook$QUERY_STRING)

            private$COOKIES <- private$parse_cookies()
        },
        print = function(...) {
            cat('A HTTP request\n')
            cat('==============\n')
            cat('Trusted: ', if (self$trust) 'Yes' else 'No', '\n', sep = '')
            cat(' Method: ', self$method, '\n', sep = '')
            cat('    URL: ', self$url, '\n', sep = '')
        },
        set_body = function(content) {
            private$BODY <- content
        },
        set_cookies = function(cookies) {
            private$COOKIES <- cookies
        },
        accepts = function(types) {
            accept <- private$format_mimes(self$headers$Accept)
            if (is.null(accept)) return(types[1])
            full_types <- private$format_types(types)
            ind <- private$get_format_spec(full_types, accept)
            if (is.null(ind)) return(NULL)
            types[ind]
        },
        accepts_charsets = function(charsets) {
            accept <- private$format_charsets(self$headers$Accept_Charset)
            if (is.null(accept)) return(charsets[1])
            ind <- private$get_charset_spec(tolower(charsets), accept)
            if (is.null(ind)) return(NULL)
            charsets[ind]
        },
        accepts_encoding = function(encoding) {
            accept <- private$format_encodings(self$headers$Accept_Encoding)
            if (is.null(accept)) return(encoding[1])
            ind <- private$get_encoding_spec(tolower(encoding), accept)
            if (is.null(ind)) return(NULL)
            encoding[ind]
        },
        accepts_language = function(language) {
            accept <- private$format_languages(self$headers$Accept_Language)
            if (is.null(accept)) return(language[1])
            ind <- private$get_language_spec(tolower(language), accept)
            if (is.null(ind)) return(NULL)
            language[ind]
        },
        is = function(type) {
            content <- private$format_mimes(self$headers$Content_Type)
            full_type <- private$format_types(type)
            if (nrow(full_type) == 0) return(FALSE)
            !is.null(private$get_format_spec(full_type, content))
        },
        get_header = function(name) {
            self$headers[[gsub('-', '_', name)]]
        }
    ),
    active = list(
        trust = function(value) {
            if (missing(value)) return(private$TRUST)
            assert_that(is.flag(value))
            private$TRUST <- value
        },
        method = function() {
            private$METHOD
        },
        body = function() {
            private$BODY
        },
        cookies = function() {
            private$COOKIES
        },
        headers = function() {
            private$HEADERS
        },
        host = function() {
            if (self$trust && !is.null(self$headers$X_Forwarded_Host)) {
                self$headers$X_Forwarded_Host
            } else {
                private$HOST
            }
        },
        ip = function() {
            if (self$trust && !is.null(self$headers$X_Forwarded_For)) {
                self$headers$X_Forwarded_For[1]
            } else {
                private$IP
            }
        },
        ips = function() {
            if (self$trust && !is.null(self$headers$X_Forwarded_For)) {
                self$headers$X_Forwarded_For
            } else {
                character(0)
            }
        },
        protocol = function() {
            if (self$trust && !is.null(self$headers$X_Forwarded_Proto)) {
                self$headers$X_Forwarded_Proto
            } else {
                self$rook$rook.url_scheme
            }
        },
        root = function() {
            self$rook$SCRIPT_NAME
        },
        path = function() {
            self$rook$PATH_INFO
        },
        url = function() {
            private$URL
        },
        query = function() {
            private$QUERY
        },
        xhr = function() {
            self$headers$X_Requested_With == 'XMLHttpRequest'
        },
        secure = function() {
            self$protocol == 'https'
        },
        rook = function() {
            private$ROOK
        }
    ),
    private = list(
        TRUST = FALSE,
        ROOK = NULL,
        METHOD = NULL,
        HOST = NULL,
        IP = NULL,
        URL = NULL,
        QUERY = NULL,
        BODY = NULL,
        HEADERS = NULL,
        COOKIES = NULL,

        parse_cookies = function() {
            if (is.null(self$headers$Cookie)) return(list())
            cookies <- trimws(strsplit(self$headers$Cookie, ';')[[1]])
            cookies <- unlist(strsplit(cookies, '='))
            structure(
                as.list(url_decode(cookies[c(FALSE, TRUE)])),
                names = cookies[c(TRUE, FALSE)]
            )
        },
        parse_query = function(query) {
            if (query == '') return(list())
            query <- url_decode(query)
            query <- sub('^\\?', '', query)
            query <- gsub('\\+', ' ', query)
            query <- strsplit(unlist(strsplit(query, '&|;')), '=')
            ans <- list()
            for (i in query) {
                ans[[i[1]]] <- append(ans[[i[1]]], i[2])
            }
            lapply(ans, type.convert, as.is = TRUE)
        },
        get_headers = function(rook) {
            vars <- ls(rook)
            headers <- vars[grepl('^HTTP_', vars)]
            ans <- lapply(headers, function(head) {
                strsplit(rook[[head]], split = ',\\s?')[[1]]
            })
            names(ans) <- gsub("(^|_)([[:alpha:]])", "\\1\\U\\2",
                               tolower(sub('^HTTP_', '', headers)),
                               perl = TRUE)
            ans
        },
        format_mimes = function(type) {
            if (is.null(type) || length(type) == 0) return(NULL)
            type <- stri_match_first_regex(tolower(type), '^\\s*([^\\s\\/;]+)\\/([^;\\s]+)\\s*(?:;(.*))?$')
            type <- as.data.frame(type, stringsAsFactors = FALSE)
            names(type) <- c('full', 'main', 'sub', 'q')
            type$q <- get_quality(type$q)
            type
        },
        format_charsets = function(charsets) {
            if (is.null(charsets) || length(charsets) == 0) return(NULL)
            charsets <- stri_match_first_regex(tolower(charsets), '^\\s*([^\\s;]+)\\s*(?:;(.*))?$')
            charsets <- as.data.frame(charsets, stringsAsFactors = FALSE)
            names(charsets) <- c('full', 'main', 'q')
            charsets$q <- get_quality(charsets$q)
            charsets
        },
        format_encodings = function(encodings) {
            if (is.null(encodings) || length(encodings) == 0) return(NULL)
            encodings <- stri_match_first_regex(tolower(encodings), '^\\s*([^\\s;]+)\\s*(?:;(.*))?$')
            encodings <- as.data.frame(encodings, stringsAsFactors = FALSE)
            names(encodings) <- c('full', 'main', 'q')
            encodings$q <- get_quality(encodings$q)
            if (!'identity' %in% encodings$main) {
                encodings  <- rbind(
                    encodings,
                    data.frame(full = 'identity;q=0', main = 'identity', q = min(encodings$q), stringsAsFactors = FALSE)
                )
            }
            encodings
        },
        format_languages = function(lang) {
            if (is.null(lang) || length(lang) == 0) return(NULL)
            lang <- stri_match_first_regex(tolower(lang), '^\\s*([^\\s\\-;]+)(?:-([^\\s;]+))?\\s*(?:;(.*))?$')
            lang <- as.data.frame(lang, stringsAsFactors = FALSE)
            names(lang) <- c('full', 'main', 'sub', 'q')
            lang$q <- get_quality(lang$q)
            lang$complete <- paste0(lang$main, ifelse(is.na(lang$sub), '', paste0('-', lang$sub)))
            lang
        },
        format_types = function(formats) {
            ext <- !grepl('/', formats)
            format_ind <- rep(NA_integer_, length(formats))
            formats[ext] <- sub('^[.]', '', formats[ext])
            format_ind[ext] <- mimes_ext$index[match(formats[ext], mimes_ext$ext)]
            format_ind[!ext] <- match(formats[!ext], mimes$name)
            mimes[na.omit(format_ind), ]
        },
        get_format_spec = function(format, accepts) {
            f_split <- strsplit(format$name, '/')
            spec <- do.call(rbind, lapply(f_split, function(n) {
                spec <- ifelse(n[1] == accepts$main, 4, if (accepts$main == '*') 0 else NA)
                spec <- spec + ifelse(n[2] == accepts$sub, 2, if (accepts$sub == '*') 0 else NA)
                win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
                if (is.na(spec[win])) {
                    c(NA, NA)
                } else {
                    c(spec, win)
                }
            }))
            if (all(is.na(spec[, 1]))) return(NULL)
            order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(f_split), decreasing = TRUE)[1]
        },
        get_charset_spec = function(charset, accepts) {
            spec <- do.call(rbind, lapply(charset, function(n) {
                spec <- ifelse(n == accepts$main, 1, if (accepts$main == '*') 0 else NA)
                win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
                if (is.na(spec[win])) {
                    c(NA, NA)
                } else {
                    c(spec, win)
                }
            }))
            if (all(is.na(spec[, 1]))) return(NULL)
            order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(charset), decreasing = TRUE)[1]
        },
        get_encoding_spec = function(encoding, accepts) {
            spec <- do.call(rbind, lapply(encoding, function(n) {
                spec <- ifelse(n == accepts$main, 1, if (accepts$main == '*') 0 else NA)
                win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
                if (is.na(spec[win])) {
                    c(NA, NA)
                } else {
                    c(spec, win)
                }
            }))
            if (all(is.na(spec[, 1]))) return(NULL)
            order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(encoding), decreasing = TRUE)[1]
        },
        get_language_spec = function(lang, accepts) {
            l_split <- strsplit(lang, '-')
            spec <- do.call(rbind, lapply(seq_along(lang), function(i) {
                spec <- ifelse(lang[i] == accepts$complete, 4, NA)
                spec <- ifelse(is.na(spec) & lang[i] == accepts$main, 2, NA)
                spec <- ifelse(is.na(spec) & l_split[[i]][1] == accepts$complete, 1, NA)
                spec <- ifelse(is.na(spec) & accepts$complete == '*', 0, NA)
                win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
                if (is.na(spec[win])) {
                    c(NA, NA)
                } else {
                    c(spec, win)
                }
            }))
            if (all(is.na(spec[, 1]))) return(NULL)
            order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(lang), decreasing = TRUE)[1]
        }
    )
)
#' @importFrom stringi stri_extract_first_regex
get_quality <- function(q) {
    q <- stri_extract_first_regex(q, 'q=([0-9]*[.])?[0-9]+')
    q <- as.numeric(sub('q=', '', q))
    q[is.na(q)] <- 1
    q
}
