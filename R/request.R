#' HTTP Request Handling
#'
#' This class wraps all functionality related to extracting information from a
#' http request. Much of the functionality is inspired by the Request class in
#' Express.js, so [the documentation](https://expressjs.com/en/4x/api.html#req)
#' for this will complement this document. As `reqres` is build on top of the
#' [Rook specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
#' the `Request` object is initialized from a Rook-compliant object. This will
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
#' @seealso [`Response`] for handling http responses
#'
#' @importFrom R6 R6Class
#' @importFrom stringi stri_match_first_regex stri_trim_both stri_split_fixed stri_split_regex
#' @importFrom urltools url_decode
#' @importFrom brotli brotli_decompress
#' @importFrom utils modifyList
#'
#' @export
#'
#' @examples
#' fake_rook <- fiery::fake_request(
#'   'http://example.com/test?id=34632&question=who+is+hadley',
#'   content = 'This is an elaborate ruse',
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
#' req$query
#'
#' # Test if content is text
#' req$is('txt')
#'
#' # Perform content negotiation for the response
#' req$accepts(c('html', 'json', 'txt'))
#'
#' # Cleaning up connections
#' rm(fake_rook, req)
#' gc()
#'
Request <- R6Class('Request',
  public = list(
    # Methods
    #' @description Create a new request from a rook object
    #' @param rook The [rook](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md) object to base the request on
    #' @param trust Is this request trusted blindly. If `TRUE` `X-Forwarded-*` headers will be returned when querying host, ip, and protocol
    #'
    initialize = function(rook, trust = FALSE) {
      self$trust <- trust
      private$ORIGIN <- rook
      private$METHOD <- tolower(rook$REQUEST_METHOD)
      delayedAssign("HEADERS", private$get_headers(rook), assign.env = private)
      if (is.null(rook$HTTP_HOST)) {
        private$HOST <- paste(rook$SERVER_NAME, rook$SERVER_PORT, sep = ':')
      } else {
        private$HOST <- rook$HTTP_HOST
      }
      private$PROTOCOL <- rook$rook.url_scheme
      private$ROOT <- rook$SCRIPT_NAME
      private$PATH <- rook$PATH_INFO
      private$QUERYSTRING <- rook$QUERY_STRING
      if (private$QUERYSTRING != '') {
        private$QUERYSTRING <- paste0('?', sub('^\\?', '', private$QUERYSTRING))
      }
      private$IP <- rook$REMOTE_ADDR
      delayedAssign("QUERY", private$parse_query(private$QUERYSTRING), assign.env = private)

      delayedAssign("COOKIES", private$parse_cookies(), assign.env = private)
    },
    #' @description Pretty printing of the object
    #' @param ... ignored
    #'
    print = function(...) {
      cli::cli_rule('An HTTP request')
      cli::cli_dl(c(
        Trusted = '{if (self$trust) cli::col_green("Yes") else cli::col_red("No")}',
        Method = self$method,
        URL = self$url
      ))
      invisible(self)
    },
    #' @description Sets the content of the request body. This method should
    #' mainly be used in concert with a body parser that reads the `rook$input`
    #' stream
    #' @param content An R object representing the body of the request
    #'
    set_body = function(content) {
      private$BODY <- content
      invisible(self)
    },
    #' @description Sets the cookies of the request. The cookies are
    #' automatically parsed and populated, so this method is mainly available
    #' to facilitate cookie signing and encryption
    #' @param cookies A named list of cookie values
    #'
    set_cookies = function(cookies) {
      private$COOKIES <- cookies
      invisible(self)
    },
    #' @description Given a vector of response content types it returns the
    #' preferred one based on the `Accept` header.
    #' @param types A vector of types
    #'
    accepts = function(types) {
      accept <- private$format_mimes(self$headers$Accept)
      if (is.null(accept)) return(types[1])
      full_types <- private$format_types(types)
      ind <- private$get_format_spec(full_types, accept)
      if (is.null(ind)) return(NULL)
      types[ind]
    },
    #' @description Given a vector of possible character encodings it returns
    #' the preferred one based on the `Accept-Charset` header.
    #' @param charsets A vector of charsets
    #'
    accepts_charsets = function(charsets) {
      accept <- private$format_charsets(self$headers$Accept_Charset)
      if (is.null(accept)) return(charsets[1])
      ind <- private$get_charset_spec(tolower(charsets), accept)
      if (is.null(ind)) return(NULL)
      charsets[ind]
    },
    #' @description Given a vector of possible content encodings (usually
    #' compression algorithms) it selects the preferred one based on the
    #' `Accept-Encoding` header. If there is no match it will return `"identity"`
    #' signaling no compression.
    #' @param encoding A vector of encoding names
    #'
    accepts_encoding = function(encoding) {
      acc_enc <- self$get_header('Accept-Encoding')
      if (is.null(acc_enc)) acc_enc <- 'identity'
      accept <- private$format_encodings(acc_enc)
      ind <- private$get_encoding_spec(tolower(encoding), accept)
      if (is.null(ind)) return('identity')
      encoding[ind]
    },
    #' @description Given a vector of possible content languages it selects the
    #' best one based on the `Accept-Language` header.
    #' @param language A vector of languages
    #'
    accepts_language = function(language) {
      accept <- private$format_languages(self$headers$Accept_Language)
      if (is.null(accept)) return(language[1])
      ind <- private$get_language_spec(tolower(language), accept)
      if (is.null(ind)) return(NULL)
      language[ind]
    },
    #' @description Queries whether the body of the request is in a given format
    #' by looking at the `Content-Type` header. Used for selecting the best
    #' parsing method.
    #' @param type A vector of content types to check for. Can be fully
    #' qualified MIME types, a file extension, or a mime type with wildcards
    #'
    is = function(type) {
      accept <- self$get_header('Content-Type')
      if (is.null(accept)) return(NULL)
      accept <- trimws(strsplit(accept, ';')[[1]])[1]
      content <- private$format_mimes(accept)
      ext <- !grepl('/', type)
      type[ext] <- mimes$name[mimes_ext$index[match(sub('^[.]', '', type[ext]), mimes_ext$ext)]]
      type <- strsplit(type, '/', TRUE)
      main <- vapply(type, `[[`, character(1), 1L)
      sub <- vapply(type, `[[`, character(1), 2L)
      match <- (main == content$main | main == "*") & (sub == content$sub | sub == "*")
      priority <- order(main == "*", sub == "*", ext)
      attr(match, "pick") <- priority[which(match[priority])[1]]
      match
    },
    #' @description Get the header of the specified name.
    #' @param name The name of the header to get
    #'
    get_header = function(name) {
      self$headers[[gsub('-', '_', name)]]
    },
    #' @description Creates a new `Response` object from the request
    #'
    respond = function() {
      if (is.null(self$response)) {
        Response$new(self)
      } else {
        self$response
      }
    },
    #' @description Based on provided parsers it selects the appropriate one by
    #' looking at the `Content-Type` header and assigns the result to the
    #' request body. A parser is a function accepting a raw vector, and a named
    #' list of additional directives, and returns an R object of any kind (if
    #' the parser knows the input to be plain text, simply wrap it in
    #' [rawToChar()]). If the body is compressed, it will be decompressed based
    #' on the `Content-Encoding` header prior to passing it on to the parser.
    #' See [parsers] for a list of pre-supplied parsers. Parsers are either
    #' supplied in a named list or as named arguments to the parse method. The
    #' names should correspond to mime types or known file extensions. If
    #' `autofail = TRUE` the response will be set with the correct error code if
    #' parsing fails. `parse()` returns `TRUE` if parsing was successful and
    #' `FALSE` if not
    #' @param ... A named set of parser functions
    #' @param autofail Automatically populate the response if parsing fails
    #'
    parse = function(..., autofail = TRUE) {
      if (!private$has_body()) return(TRUE)

      parsers <- list2(...)
      if (is.list(parsers[[1]])) {
        lifecycle::deprecate_soft("0.3", I("Request$parse(list(...))"), I("Request$parse(!!!list(...))"))
        first_parsers <- names(parsers)[-1]
        parsers <- modifyList(parsers[[1]], list2(...)[-1])
        first_parsers <- names(parsers) %in% first_parsers
        parsers <- c(parsers[first_parsers], parsers[!first_parsers])
      }
      if (!is_named2(parsers)) {
        cli::cli_abort("Provided parsers must be named")
      }

      type <- self$get_header('Content-Type')
      if (is.null(type)) return(FALSE)

      parser_match <- self$is(names(parsers))

      if (!any(parser_match)) {
        if (autofail) {
          self$respond()$status_with_text(415L)
        }
        return(FALSE)
      }

      content <- private$get_body()
      content <- tri(private$unpack(content))
      if (is_condition(content)) {
        if (autofail) self$respond()$status_with_text(400L)
        return(FALSE)
      }

      parser <- parsers[attr(parser_match, "pick")]

      directives <- trimws(strsplit(type, ';')[[1]])[-1]
      directives <- strsplit(directives, '=')
      directives <- structure(
        lapply(directives, `[`, 2),
        names = lapply(directives, `[`, 1)
      )

      content <- tri(parser(content, directives))
      if (is_condition(content)) {
        if (autofail) self$respond()$status_with_text(400L)
        return(FALSE)
      }

      private$BODY <- content
      return(TRUE)
    },
    #' @description This is a simpler version of the `parse()` method. It will
    #' attempt to decompress the body and set the `body` field to the resulting
    #' raw vector. It is then up to the server to decide how to handle the
    #' payload. It returns `TRUE` if successful and `FALSE` otherwise.
    #' @param autofail Automatically populate the response if parsing fails
    #'
    parse_raw = function(autofail = TRUE) {
      content <- private$get_body()
      content <- tri(private$unpack(content))
      if (is_condition(content)) {
        if (autofail) self$response$status_with_text(400L)
        return(FALSE)
      }
      private$BODY <- content
      TRUE
    },
    #' @description Prints a HTTP representation of the request to the output
    #' stream.
    #'
    as_message = function() {
      cat(toupper(self$method), ' ', self$root, self$path, self$querystring, ' ', toupper(self$protocol), '/1.1\n', sep = '')
      if (is.null(self$get_header('Host'))) {
        cat('Host: ', self$host, '\n', sep = '')
      }
      headers <- split_headers(self$headers)
      cat_headers(headers$request)
      cat_headers(headers$entity)
      body <- rawToChar(private$get_body())
      cat('\n')
      if (body == '') {
        cat('<No Body>\n')
      } else {
        body <- gsub('\n', '\\\\n', body)
        body <- gsub('\t', '\\\\t', body)
        cat(substr(body, 1, 77), if (nchar(body) > 77) '...\n' else '\n', sep = '')
      }
    }
  ),
  active = list(
    #' @field trust A logical indicating whether the request is trusted. *Mutable*
    #'
    trust = function(value) {
      if (missing(value)) return(private$TRUST)
      check_bool(value)
      private$TRUST <- value
    },
    #' @field method A string indicating the request method (in lower case, e.g.
    #' 'get', 'put', etc.). *Immutable*
    #'
    method = function() {
      private$METHOD
    },
    #' @field body An object holding the body of the request. This is an empty
    #' string by default and needs to be populated using the `set_body()` method
    #' (this is often done using a body parser that accesses the Rook$input
    #' stream). *Immutable*
    #'
    body = function() {
      private$BODY
    },
    #' @field cookies Access a named list of all cookies in the request. These
    #' have been URI decoded. *Immutable*
    #'
    cookies = function() {
      private$COOKIES
    },
    #' @field headers Access a named list of all headers in the request. In
    #' order to follow R variable naming standards `-` have been substituted
    #' with `_`. Use the `get_header()` method to lookup based on the correct
    #' header name. *Immutable*
    #'
    headers = function() {
      private$HEADERS
    },
    #' @field host Return the domain of the server given by the "Host" header if
    #' `trust == FALSE`. If `trust == true` returns the `X-Forwarded-Host`
    #' instead. *Immutable*
    #'
    host = function() {
      if (self$trust && !is.null(self$headers$X_Forwarded_Host)) {
        self$headers$X_Forwarded_Host
      } else {
        private$HOST
      }
    },
    #' @field ip Returns the remote address of the request if `trust == FALSE`.
    #' If `trust == TRUE` it will instead return the first value of the
    #' `X-Forwarded-For` header. *Immutable*
    #'
    ip = function() {
      if (self$trust && !is.null(self$headers$X_Forwarded_For)) {
        self$headers$X_Forwarded_For[1]
      } else {
        private$IP
      }
    },
    #' @field ips If `trust == TRUE` it will return the full list of ips in the
    #' `X-Forwarded-For` header. If `trust == FALSE` it will return an empty
    #' vector. *Immutable*
    #'
    ips = function() {
      if (self$trust && !is.null(self$headers$X_Forwarded_For)) {
        self$headers$X_Forwarded_For
      } else {
        character(0)
      }
    },
    #' @field protocol Returns the protocol (e.g. 'http') used for the request.
    #' If `trust == TRUE` it will use the value of the `X-Forwarded-Proto` header.
    #' *Immutable*
    #'
    protocol = function() {
      if (self$trust && !is.null(self$headers$X_Forwarded_Proto)) {
        self$headers$X_Forwarded_Proto
      } else {
        private$PROTOCOL
      }
    },
    #' @field root The mount point of the application receiving this request.
    #' Can be empty if the application is mounted on the server root.
    #' *Immutable*
    #'
    root = function() {
      private$ROOT
    },
    #' @field path The part of the url following the root. Defines the local
    #' target of the request (independent of where it is mounted). *Immutable*
    #'
    path = function() {
      private$PATH
    },
    #' @field url The full URL of the request. *Immutable*
    url = function() {
      paste0(self$protocol, '://',
             self$host,
             self$root,
             self$path,
             self$querystring)
    },
    #' @field query The query string of the request (anything following "?" in
    #' the URL) parsed into a named list. The query has been url decoded and "+"
    #' has been substituted with space. Multiple queries are expected to be
    #' separated by either "&" or "|". *Immutable*
    #'
    query = function() {
      private$QUERY
    },
    #' @field query_delim The delimiter used for specifying multiple values in a
    #' query. If `NULL` then queries are expected to contain multiple key-value
    #' pairs for the same key in order to provide an array, e.g.
    #' `?arg1=3&arg1=7`. If setting it to `",""`, `"|"`, or `" "` then an array
    #' can be provided in a single key-value pair, e.g. `?arg1=3|7`
    #'
    query_delim = function(value) {
      if (missing(value)) return(private$QUERYDELIM)
      if (!is.null(value)) value <- arg_match0(value, c(",", "|", " "))
      private$QUERYDELIM <- value
      delayedAssign("QUERY", private$parse_query(private$QUERYSTRING), assign.env = private)
    },
    #' @field querystring The unparsed query string of the request, including
    #' "?". If no query string exists it will be `""` rather than `"?"`
    #'
    querystring = function() {
      private$QUERYSTRING
    },
    #' @field xhr A logical indicating whether the `X-Requested-With` header
    #' equals `XMLHttpRequest` thus indicating that the request was performed
    #' using JavaScript library such as jQuery. *Immutable*
    #'
    xhr = function() {
      xhr <- self$get_header('X-Requested-With')
      !is.null(xhr) && xhr == 'XMLHttpRequest'
    },
    #' @field secure A logical indicating whether the request was performed
    #' using a secure connection, i.e. `protocol == 'https'`. *Immutable*
    secure = function() {
      self$protocol == 'https'
    },
    #' @field origin The original object used to create the `Request` object. As
    #' `reqres` currently only works with rook this will always return the
    #' original rook object. *Immutable*, though the content of the rook object
    #' itself might be manipulated as it is an environment.
    #'
    origin = function() {
      private$ORIGIN
    },
    #' @field response If a `Response` object has been created for this request
    #' it is accessible through this field. *Immutable*
    #'
    response = function(res) {
      if (missing(res)) return(private$RESPONSE)
      if (!is.null(private$RESPONSE)) {
        cli::cli_abort('Response can only be assigned once')
      }
      if (!inherits(res, 'Response')) {
        cli::cli_abort('{.arg res} must be a {.cls Response} object')
      }
      if (!identical(self, res$request)) {
        cli::cli_abort('{.arg res} must be a Response responding to this request')
      }
      private$RESPONSE <- res
    }
  ),
  private = list(
    TRUST = FALSE,
    ORIGIN = NULL,
    METHOD = NULL,
    HOST = NULL,
    PROTOCOL = NULL,
    ROOT = NULL,
    PATH = NULL,
    QUERYSTRING = NULL,
    QUERYDELIM = NULL,
    IP = NULL,
    QUERY = NULL,
    BODY = NULL,
    HEADERS = NULL,
    COOKIES = NULL,
    RESPONSE = NULL,

    parse_cookies = function() {
      if (is.null(self$headers$Cookie)) return(list())
      cookies <- stri_trim_both(stri_split_regex(self$headers$Cookie, ";(?=\\s*[a-zA-Z0-9!#$%&'()*+-.\\/:<>?@\\[\\]^_`{|}~]{1,})")[[1]])
      cookies <- unlist(stri_split_fixed(cookies, '=', n = 2))
      structure(
        as.list(url_decode(cookies[c(FALSE, TRUE)])),
        names = cookies[c(TRUE, FALSE)]
      )
    },
    parse_query = function(query) {
      query_parser(query, private$QUERYDELIM)
    },
    get_headers = function(rook) {
      vars <- ls(rook)
      headers <- vars[grepl('^HTTP_', vars)]
      ans <- lapply(headers, function(head) {
        # FIXME: This doesn't detect if the escape has been escaped
        literals <- stringi::stri_locate_all_regex(rook[[head]], "(?<!\\\\)\".*?[^\\\\]\"", omit_no_match = TRUE)[[1]]
        if (length(literals) == 0) {
          stringi::stri_split_regex(rook[[head]], "(?<!Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s?")[[1]]
        } else {
          splits <- stringi::stri_locate_all_regex(rook[[head]], "(?<!Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s?", omit_no_match = TRUE)[[1]]
          if (length(splits) == 0) {
            return(rook[[head]])
          }
          split_ind <- rep(seq_len(nrow(splits)), each = nrow(literals))
          splits2 <- splits[split_ind, ]
          inside <- splits2[,1] > literals[,1] & splits2[,2] < literals[,2]
          splits <- splits[-split_ind[inside],, drop = FALSE]
          if (length(splits) == 0) {
            return(rook[[head]])
          }
          splits <- c(0, t(splits) + c(-1, 1), stringi::stri_length(rook[[head]]))
          stringi::stri_sub_all(rook[[head]], splits[c(TRUE, FALSE)], splits[c(FALSE, TRUE)])[[1]]
        }
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
        spec <- ifelse(n[1] == accepts$main, 4, ifelse(accepts$main == '*', 0, NA))
        spec <- spec + ifelse(n[2] == accepts$sub, 2, ifelse(accepts$sub == '*', 0, NA))
        win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
        if (is.na(spec[win])) {
          c(NA, NA)
        } else {
          c(spec[win], win)
        }
      }))
      if (all(is.na(spec[, 1]))) return(NULL)
      order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(f_split), decreasing = TRUE)[1]
    },
    get_charset_spec = function(charset, accepts) {
      spec <- do.call(rbind, lapply(charset, function(n) {
        spec <- ifelse(n == accepts$main, 1, ifelse(accepts$main == '*', 0, NA))
        win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
        if (is.na(spec[win])) {
          c(NA, NA)
        } else {
          c(spec[win], win)
        }
      }))
      if (all(is.na(spec[, 1]))) return(NULL)
      order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(charset), decreasing = TRUE)[1]
    },
    get_encoding_spec = function(encoding, accepts) {
      spec <- do.call(rbind, lapply(encoding, function(n) {
        spec <- ifelse(n == accepts$main, 1, ifelse(accepts$main == '*', 0, NA))
        win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
        if (is.na(spec[win])) {
          c(NA, NA)
        } else {
          c(spec[win], win)
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
          c(spec[win], win)
        }
      }))
      if (all(is.na(spec[, 1]))) return(NULL)
      order(accepts$q[spec[,2]], spec[,1], -spec[,2], -seq_along(lang), decreasing = TRUE)[1]
    },
    unpack = function(raw) {
      compression <- self$get_header('Content-Encoding')
      if (is.null(compression)) return(raw)
      compression <- rev(trimws(strsplit(compression, ',')[[1]]))
      Reduce(function(l, r) {
        switch(
          r,
          identity = l,
          br = brotli_decompress(l),
          gzip =,
          "x-gzip" = {
            con <- gzcon(rawConnection(l))
            on.exit(close(con), add = TRUE)
            readBin(con, "raw", length(l))
          },
          deflate = memDecompress(l, type = 'gzip'),
          cli::cli_abort('Unsupported compression {.val {r}}')
        )
      }, x = compression, init = raw)
    },
    has_body = function() {
      first <- private$ORIGIN$rook.input$read(1)
      private$ORIGIN$rook.input$rewind()
      length(first) != 0
    },
    get_body = function() {
      body <- private$ORIGIN$rook.input$read()
      private$ORIGIN$rook.input$rewind()
      body
    }
  )
)
#' @rdname Request
#'
#' @usage as.Request(x, ...)
#' @param x An object coercible to a `Request`.
#' @param ... Parameters passed on to `Request$new()`
#' @return A `Request` object (for `as.Request()`) or a logical indicating whether
#' the object is a `Request` (for `is.Request()`)
#' @export
as.Request <- function(x, ...) {
  UseMethod('as.Request')
}
#' @export
as.Request.Request <- function(x, ...) x
#' @export
as.Request.environment <- function(x, ...) {
  if (is.null(x[['rook.version']])) {
    cli::cli_abort('{.arg x} must be a Rook object')
  }
  Request$new(x, ...)
}
#' @rdname Request
#' @usage is.Request(x)
#' @export
is.Request <- function(x) inherits(x, 'Request')
#' @importFrom stringi stri_extract_first_regex
get_quality <- function(q) {
  q <- stri_extract_first_regex(q, 'q=([0-9]*[.])?[0-9]+')
  q <- as.numeric(sub('q=', '', q))
  q[is.na(q)] <- 1
  q
}
