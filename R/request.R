#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.flag has_attr
#' @importFrom stringi stri_match_first_regex
#' @importFrom urltools url_decode
Request <- R6Class('Request',
    public = list(
        initialize = function(rook, trust = FALSE, bodyParser) {
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
            private$URL <- paste0(rook$rook.url_scheme,
                                  private$HOST,
                                  rook$SCRIPT_NAME,
                                  rook$PATH_INFO,
                                  rook$QUERY_STRING)
            private$QUERY <- private$parse_query(rook$QUERY_STRING)

            if (!missing(bodyParser)) {
                private$BODY <- private$parse_body(bodyParser)
            }
            private$COOKIES <- private$parse_cookies()
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
            content <- private$format_mime(self$headers$Content_Type)
            full_type <- private$format_types(type)
            !is.null(private$get_format_spec(full_type, content))
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
        BODY = NULL,
        COOKIES = NULL,

        parse_body = function(fun) {
            if (!is.list(fun)) {
                fun(self$rook$input)
            } else {
                assert_that(has_attr(fun, 'names'))
                for (i in names(fun)) {
                    if (self$is(i)) {
                        return(fun[[i]](self$rook$input))
                    }
                }
            }
        },
        parse_cookies = function() {
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
