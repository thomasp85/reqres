#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.count is.string
Response <- R6Class('Response',
    public = list(
        # Methods
        initialize = function() {
            private$STATUS = 404L
            private$HEADERS = new.env(parent = emptyenv())
            private$BODY = ''
            private$DATA = new.env(parent = emptyenv())
        },
        set_header = function(name, value) {
            assert_that(is.scalar(name))
            assert_that(is.scalar(value))
            assign(as.character(name), as.character(value), envir = private$HEADERS)
        },
        remove_header = function(name) {
            rm(name, envir = private$HEADERS)
        },
        as_list = function() {
            list(
                status = private$STATUS,
                headers = as.list(private$HEADERS),
                body = private$BODY
            )
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
            # TODO set Content-Length automatically
            if (missing(content)) return(private$BODY)
            if (is.raw(content)) {
                private$BODY <- content
            } else {
                assert_that(is.scalar(content))
                private$BODY <- as.character(content)
            }
        }
    ),
    private = list(
        # Data
        STATUS = NULL,
        HEADERS = NULL,
        BODY = NULL,
        DATA = NULL
    )
)
