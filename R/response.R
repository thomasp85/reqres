#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.count is.string
#' @importFrom tools file_path_as_absolute file_ext
#' @importFrom urltools url_encode
Response <- R6Class('Response',
    public = list(
        # Methods
        initialize = function() {
            private$STATUS = 404L
            private$HEADERS = new.env(parent = emptyenv())
            private$COOKIES = new.env(parent = emptyenv())
            private$BODY = ''
            private$DATA = new.env(parent = emptyenv())
            self$type <- 'text/plain'
        },
        set_header = function(name, value) {
            assert_that(is.string(name))
            assign(as.character(name), as.character(value), envir = private$HEADERS)
        },
        get_header = function(name) {
            assert_that(is.string(name))
            private$HEADERS[[name]]
        },
        remove_header = function(name) {
            rm(name, envir = private$HEADERS)
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
        },
        set_data = function(key, value) {
            assert_that(is.string(key))
            assign(key, value, envir = private$DATA)
        },
        get_data = function(key) {
            assert_that(is.string(key))
            private$DATA[[key]]
        },
        remove_data = function(key) {
            assert_that(is.string(key))
            rm(key, envir = private$DATA)
        },
        has_data = function(key) {
            !is.null(self$get_data(key))
        },
        attach = function(file, filename = basename(file), type = NULL) {
            self$file <- file
            assert_that(is.string(filename))
            if (!is.null(type)) self$type <- type
            self$set_header('Content-Disposition', paste0('attachment; filename=', filename))
        },
        status_with_text = function(code) {
            self$status <- code
            body <- status$Description[match(code, status$Code)]
            if (is.na(body)) body <- as.character(code)
            self$body <- body
            self$type <- 'txt'
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
                cookie <- cookie(value, encode, expires, http_only, max_age, path, secure, same_site)
                assign(as.character(name), cookie, envir = private$COOKIES)
            }
        },
        set_links = function(links) {
            assert_that(has_attr(links, 'names'))
            url <- paste0('<', unlist(links), '>')
            rel <- paste('rel="', names(links), '"')
            links <- paste(paste0(url, '; ', rel), collapse = ', ')
            self$set_header('Link', links)
        },
        remove_cookie = function(name) {
            assert_that(is.string(name))
            rm(name, envir = private$COOKIES)
        },
        as_list = function() {
            list(
                status = private$STATUS,
                headers = private$format_headers(),
                body = private$BODY
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
            if (is.raw(content)) {
                private$BODY <- content
            } else {
                assert_that(is.scalar(content))
                private$BODY <- as.character(content)
            }
        },
        file = function(path) {
            assert_that(is.string(path))
            file <- file_path_as_absolute(path)
            assert_that(file.exists(file))
            self$type <- file_ext(file)
            private$BODY <- c(file = file)
        },
        type = function(type) {
            if (!grepl('/', type)) {
                content_index <- mimes_ext$index[match(tolower(type), mimes_ext$ext)]
                type <- if (is.na(content_index)) {
                    'application/octet-stream'
                } else {
                    mimes$name[content_index]
                }
            }
            self$set_header('Content-Type', type)
        }
    ),
    private = list(
        # Data
        STATUS = NULL,
        HEADERS = NULL,
        COOKIES = NULL,
        BODY = NULL,
        DATA = NULL,

        format_headers = function() {
            headers <- as.list(private$HEADERS)
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
        }
    )
)

cookie <- function(value, expires = NULL, http_only = NULL, max_age = NULL, path = NULL, secure = NULL, same_site = NULL) {
    opts <- paste0('=', value)
    if (!is.null(expires)) {
        assert_that(is.scalar(expires))
        opts <- c(opts, format(
            as.POSIXct(expires),
            tz = 'GMT',
            format = 'Expires=%a, %d %b %Y %H:%M:%S %Z'
        ))
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
