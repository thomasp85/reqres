#' HTTP Request Handling
#'
#' @description
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
Request <- R6Class(
  'Request',
  lock_object = FALSE,
  public = list(
    # Methods
    #' @description Create a new request from a rook object
    #' @param rook The [rook](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md) object to base the request on
    #' @param trust Is this request trusted blindly. If `TRUE` `X-Forwarded-*`
    #' headers will be returned when querying host, ip, and protocol
    #' @param key A 32-bit secret key as a hex encoded string or a raw vector to
    #' use for `$encode_string()` and `$decode_string()` and by extension to
    #' encrypt a session cookie. It must be given to turn on session cookie
    #' support. A valid key can be generated using [random_key()]. NEVER STORE
    #' THE KEY IN PLAIN TEXT. Optimalle use the keyring package to store it or
    #' set it as an environment variable
    #' @param session_cookie Settings for the session cookie created using
    #' [session_cookie()]. Will be ignored if `key` is not provided to ensure
    #' session cookies are properly encrypted
    #' @param compression_limit The size threshold in bytes for trying to
    #' compress the response body (it is still dependant on content negotiation)
    #' @param query_delim The delimiter to split array-type query arguments by
    #' @param response_headers A list of headers the response should be
    #' prepopulated with. All names must be in lower case and all elements must
    #' be character vectors. This is not checked but assumed
    #' @param with_otel A boolean to indicate if otel instrumentation should be
    #' initiated with the creation of this request. Set to `FALSE` to avoid a
    #' span being started as well as metrics being recorded for this request. If
    #' `TRUE` you should call `request$clear()` as the last act of your request
    #' handling to ensure that the span is closed and that the duration metric
    #' is correctly reported.
    #'
    initialize = function(
      rook,
      trust = FALSE,
      key = NULL,
      session_cookie = NULL,
      compression_limit = 0,
      query_delim = NULL,
      response_headers = list(),
      with_otel = TRUE
    ) {
      private$START <- Sys.time()

      # otel support
      check_bool(with_otel)
      private$WITH_OTEL <- with_otel

      self$trust <- trust
      private$ORIGIN <- rook
      private$METHOD <- tolower(rook$REQUEST_METHOD)
      if (!is.null(key)) {
        if (!is.raw(key)) {
          check_string(key)
          key <- sodium::hex2bin(key)
        }
        if (length(key) != 32) {
          cli::cli_warn(c(
            "Ignoring key as it is not 32-bit",
            "i" = "Consider using {.fun random_key} to generate a key"
          ))
        }
      }
      private$KEY <- key
      if (
        !is.null(session_cookie) && !is_session_cookie_settings(session_cookie)
      ) {
        cli::cli_warn("Ignoring malformed {.arg session_cookie} argument")
        session_cookie <- NULL
      }
      if (is.null(key) && !is.null(session_cookie)) {
        cli::cli_warn(
          "Ignoring {.arg session_cookie} argument when {.arg key} is {.val {factor('NULL')}}"
        )
        session_cookie <- NULL
      }
      private$SESSION_COOKIE_SETTINGS <- session_cookie
      private$HAS_SESSION_COOKIE <- !is.null(session_cookie) &&
        isTRUE(grepl(
          paste0("(^|; )", session_cookie$name, "="),
          rook$HTTP_COOKIE
        ))
      delayedAssign("HEADERS", get_headers(rook), assign.env = private)
      if (is.null(rook$HTTP_HOST)) {
        private$HOST <- paste(rook$SERVER_NAME, rook$SERVER_PORT, sep = ':')
      } else {
        private$HOST <- rook$HTTP_HOST
      }
      check_number_decimal(compression_limit, min = 0, allow_infinite = TRUE)
      private$COMPRESSION_LIMIT <- compression_limit
      private$PROTOCOL <- rook$rook.url_scheme
      private$ROOT <- rook$SCRIPT_NAME
      private$PATH <- rook$PATH_INFO
      check_string(query_delim, allow_null = TRUE)
      private$QUERYDELIM <- query_delim
      private$QUERYSTRING <- rook$QUERY_STRING
      if (private$QUERYSTRING != '') {
        private$QUERYSTRING <- paste0('?', sub('^\\?', '', private$QUERYSTRING))
      }
      private$IP <- rook$REMOTE_ADDR
      delayedAssign(
        "QUERY",
        private$parse_query(private$QUERYSTRING),
        assign.env = private
      )

      delayedAssign("COOKIES", private$parse_cookies(), assign.env = private)

      delayedAssign(
        "SESSION",
        private$get_session_cookie(),
        assign.env = private
      )

      if (!(is_bare_list(response_headers) && is_named2(response_headers))) {
        stop_input_type(response_headers, "a named list")
      }
      private$RESPONSE_HEADERS <- response_headers
      if (!is.null(private$RESPONSE)) {
        # This is a bit dirty to reach into the private env of the response
        private$RESPONSE$.__enclos_env__$private$HEADERS <- response_headers
      }

      rook$.__reqres_Request__ <- self

      tracer <- get_tracer()
      private$OSPAN <- if (with_otel && tracer$is_enabled()) {
        request_ospan(self, private$START, tracer)
      }
      if (with_otel) push_active_request(self)
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
      accept <- format_mimes(self$headers$accept)
      if (is.null(accept)) {
        return(types[1])
      }
      full_types <- format_types(types)
      ind <- get_format_spec(full_types, accept)
      if (is.null(ind)) {
        return(NULL)
      }
      types[ind]
    },
    #' @description Given a vector of possible character encodings it returns
    #' the preferred one based on the `Accept-Charset` header.
    #' @param charsets A vector of charsets
    #'
    accepts_charsets = function(charsets) {
      accept <- format_charsets(self$headers$accept_charset)
      if (is.null(accept)) {
        return(charsets[1])
      }
      ind <- get_charset_spec(tolower(charsets), accept)
      if (is.null(ind)) {
        return(NULL)
      }
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
      if (is.null(acc_enc)) {
        acc_enc <- 'identity'
      }
      accept <- format_encodings(acc_enc)
      ind <- get_encoding_spec(tolower(encoding), accept)
      if (is.null(ind)) {
        return('identity')
      }
      encoding[ind]
    },
    #' @description Given a vector of possible content languages it selects the
    #' best one based on the `Accept-Language` header.
    #' @param language A vector of languages
    #'
    accepts_language = function(language) {
      accept <- format_languages(self$headers$accept_language)
      if (is.null(accept)) {
        return(language[1])
      }
      ind <- get_language_spec(tolower(language), accept)
      if (is.null(ind)) {
        return(NULL)
      }
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
      if (is.null(accept)) {
        return(NULL)
      }
      accept <- trimws(stringi::stri_split_fixed(accept, ';', n = 2)[[1]])[1]
      content <- stringi::stri_split_fixed(accept, "/", n = 2)[[1]]
      type <- ifelse(type == "*", "*/*", type)
      ext <- !grepl('/', type)
      type[ext] <- mimes$name[mimes_ext$index[match(
        sub('^[.]', '', type[ext]),
        mimes_ext$ext
      )]]
      type <- stringi::stri_split_fixed(type, '/', n = 2)
      main <- vapply(type, `[[`, character(1), 1L)
      sub <- vapply(type, `[[`, character(1), 2L)
      match <- (main == content[1] | main == "*") &
        (sub == content[2] | sub == "*")
      priority <- order(main == "*", sub == "*", ext)
      attr(match, "pick") <- priority[which(match[priority])[1]]
      match
    },
    #' @description Get the header of the specified name.
    #' @param name The name of the header to get
    #'
    get_header = function(name) {
      self$headers[[tolower(gsub('-', '_', name))]]
    },
    #' @description Test for the existence of any header given by `name`
    #' @param name The name of the header to look for
    #'
    has_header = function(name) {
      !is.null(self$headers[[tolower(gsub('-', '_', name))]])
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
    #' `autofail = TRUE` the response will throw an appropriate abort code if
    #' failing to parse the body. `parse()` returns `TRUE` if parsing was
    #' successful and `FALSE` if not
    #' @param ... A named set of parser functions
    #' @param autofail Automatically populate the response if parsing fails
    #'
    parse = function(..., autofail = TRUE) {
      if (!private$has_body()) {
        return(TRUE)
      }

      parsers <- list2(...)
      if (is.list(parsers[[1]])) {
        lifecycle::deprecate_soft(
          "0.3",
          I("Request$parse(list(...))"),
          I("Request$parse(!!!list(...))")
        )
        first_parsers <- names(parsers)[-1]
        parsers <- modifyList(parsers[[1]], list2(...)[-1])
        first_parsers <- names(parsers) %in% first_parsers
        parsers <- c(parsers[first_parsers], parsers[!first_parsers])
      }
      if (!is_named2(parsers)) {
        cli::cli_abort("Provided parsers must be named")
      }

      type <- self$get_header('Content-Type')
      if (is.null(type)) {
        if (autofail) {
          abort_bad_request("Missing Content-Type header")
        }
        return(FALSE)
      }

      parser_match <- self$is(names(parsers))

      if (!any(parser_match)) {
        if (autofail) {
          if (self$method == "post") {
            self$response$set_header(
              "Accept-Post",
              paste0(format_types(names(parsers)), collapse = ", ")
            )
          } else if (self$method == "patch") {
            self$response$set_header(
              "Accept-Patch",
              paste0(format_types(names(parsers)), collapse = ", ")
            )
          }
          abort_status(415L)
        }
        return(FALSE)
      }

      content <- private$get_body()
      content <- tri(private$unpack(content))
      if (is_condition(content)) {
        if (autofail) {
          abort_bad_request(
            "Request body failed to be decoded",
            parent = content
          )
        }
        return(FALSE)
      }

      parser <- parsers[[attr(parser_match, "pick")]]

      directives <- trimws(strsplit(type, ';')[[1]])[-1]
      directives <- strsplit(directives, '=')
      directives <- structure(
        lapply(directives, `[`, 2),
        names = lapply(directives, `[`, 1)
      )

      content <- tri(parser(content, directives))
      if (is_reqres_problem(content)) {
        cnd_signal(content)
      } else if (is_condition(content)) {
        if (autofail) {
          abort_status(400L, "Error parsing the request body", parent = content)
        }
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
        if (autofail) {
          abort_bad_request(
            "Request body failed to be decoded",
            parent = content
          )
        }
        return(FALSE)
      }
      private$BODY <- content
      TRUE
    },
    #' @description Prints a HTTP representation of the request to the output
    #' stream.
    #'
    as_message = function() {
      cat(
        toupper(self$method),
        ' ',
        self$root,
        self$path,
        self$querystring,
        ' ',
        toupper(self$protocol),
        '/1.1\n',
        sep = ''
      )
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
        cat(
          substr(body, 1, 77),
          if (nchar(body) > 77) '...\n' else '\n',
          sep = ''
        )
      }
    },
    #' @description base64-encode a string. If a key has been provided during
    #' initialisation the string is first encrypted and the final result is a
    #' combination of the encrypted text and the nonce, both base64 encoded and
    #' combined with a `"_"`.
    #' @param val A single string to encrypt
    #'
    #' @importFrom base64enc base64encode
    encode_string = function(val) {
      check_string(val)
      if (length(val) == 0 || val == "") {
        return("")
      }
      val <- charToRaw(val)
      if (is.null(private$KEY)) {
        base64encode(val)
      } else {
        nonce <- sodium::random(24)
        val <- sodium::data_encrypt(val, private$KEY, nonce)
        paste0(base64encode(val), "_", base64encode(nonce))
      }
    },
    #' @description base64-decodes a string. If a key has been provided during
    #' initialisation the input is first split by `"_"` and then the two parts
    #' are base64 decoded and decrypted. Otherwise the input is base64-decoded
    #' as-is. It will always hold that
    #' `val == decode_string(encode_string(val))`.
    #' @param val A single string to encrypt
    #'
    #' @importFrom base64enc base64decode
    decode_string = function(val) {
      if (length(val) == 0 || val == "") {
        return("")
      }
      if (is.null(private$KEY)) {
        val <- base64decode(val)
      } else {
        val <- stringi::stri_split_fixed(val, "_", n = 2)[[1]]
        if (length(val) != 2) {
          cli::cli_warn("Failed to decode encrypted value")
          return(NULL)
        }
        val <- sodium::data_decrypt(
          base64decode(val[[1]]),
          private$KEY,
          base64decode(val[[2]])
        )
      }
      rawToChar(val)
    },
    #' @description Clears the content of the request and, if created, the
    #' related response. This method exists only to allow reuse of the request
    #' and response object to save a few milliseconds in latency. Use with
    #' caution and see e.g. how fiery maintains a poll of request objects
    clear = function() {
      # otel
      if (private$WITH_OTEL && get_meter()$is_enabled()) {
        attr <- metric_attributes(self, private$RESPONSE)
        record_request_body(self, attr)
        record_response_body(self, private$RESPONSE, attr)
        pop_active_request(self, attr)
        record_duration(self, attr)
      }
      if (!is.null(private$OSPAN)) {
        span <- private$OSPAN
        status <- private$RESPONSE$status
        span$set_attribute("http.response.status_code", status)
        if (status >= 500) {
          span$set_status("error")
          span$set_attribute("error.type", as.character(status))
        }
        for (header in names(private$RESPONSE$headers)) {
          span$set_attribute(
            paste0("http.response.header.", gsub("_", "-", header)),
            private$RESPONSE$headers[[header]]
          )
        }
        otel::end_span(span)
      }
      # end otel
      private$reset_hard()
    },
    #' @description Forward a request to a new url, optionally setting different
    #' headers, queries, etc. Uses curl and mirai under the hood and returns a
    #' promise
    #' @param url The url to forward to
    #' @param query Optional querystring to append to `url`. If `NULL` the query
    #' string of the current request will be used
    #' @param method The HTTP method to use. If `NULL` the method of the current
    #' request will be used
    #' @param headers A list of headers to add to the headers of the current
    #' request. You can remove a header from the current request by setting it
    #' to `NULL` here
    #' @param body The body to send with the forward. If `NULL` the body of the
    #' current request will be used
    #' @param return A function that takes in the fulfilled response object and
    #' whose return value is returned by the promise
    #' @param ... ignored
    #'
    #' @importFrom mirai mirai
    #' @importFrom promises then
    #'
    forward = function(
      url,
      query = NULL,
      method = NULL,
      headers = NULL,
      body = NULL,
      return = NULL,
      ...
    ) {
      return_fun <- return %||% identity
      if (!is.null(query) && substr(query, 1, 1) != "?") {
        query <- paste0("?", query)
      }
      url <- paste0(url, query %||% private$QUERYSTRING)
      opts <- list(
        accept_encoding = NULL
      )
      method <- toupper(method %||% private$METHOD)
      if (method == "HEAD") {
        opts$nobody <- TRUE
      } else {
        opts$customrequest <- method
      }
      body <- body %||% private$get_body() %||% raw(0)
      if (is.character(body)) {
        body <- charToRaw(body)
      }
      if (length(body) != 0) {
        opts$postfields <- body
        opts$postfieldsize <- length(body)
      }
      header_elem <- grep("^HTTP_", ls(private$ORIGIN), value = TRUE)
      cur_headers <- lapply(header_elem, function(x) private$ORIGIN[[x]])
      names(cur_headers) <- tolower(gsub(
        "_",
        "-",
        sub("^HTTP_", "", header_elem)
      ))
      if (!is.null(headers)) {
        names(headers) <- tolower(names(headers))
        headers <- modifyList(cur_headers, headers)
      } else {
        headers <- cur_headers
      }

      promise <- mirai::mirai(
        curl_call,
        list2env(list(opts = opts, headers = headers, url = url))
      )
      promises::then(
        promise,
        onFulfilled = function(response) {
          res <- self$respond()
          res$status <- response$status_code
          res$body <- response$content
          fwd_headers <- curl::parse_headers_list(response$headers)
          fwd_headers <- fwd_headers[
            !tolower(names(fwd_headers)) %in% excluded_headers
          ]
          for (name in names(fwd_headers)) {
            res$set_header(name, fwd_headers[[name]])
          }
          res$set_data("curl_response", response)
          return_fun(res)
        },
        onRejected = function(error) {
          cli::cli_abort("Failed to forward request", parent = error)
        }
      )
    }
  ),
  active = list(
    #' @field trust A logical indicating whether the request is trusted. *Mutable*
    #'
    trust = function(value) {
      if (missing(value)) {
        return(private$TRUST)
      }
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
    #' @field session The content of the session cookie. If session cookies has
    #' not been activated it will be an empty write-protected list. If session
    #' cookies are activated but the request did not contain one it will be an
    #' empty list. The content of this field will be send encrypted as part of
    #' the response according to the cookie settings in
    #' `$session_cookie_settings`. This field is reflected in the
    #' `Response$session` field and using either produces the same result
    #'
    session = function(value) {
      if (missing(value)) {
        return(private$SESSION %||% list())
      }
      if (is.null(private$SESSION_COOKIE_SETTINGS)) {
        cli::cli_warn(c(
          "Session cookie is not active",
          "i" = "Provide {.arg key} and {.arg session_cookie} values to turn on this feature"
        ))
      } else {
        private$SESSION <- value
      }
    },
    #' @field has_session_cookie Query whether the request came with a session
    #' cookie *Immutable*
    #'
    has_session_cookie = function() {
      private$HAS_SESSION_COOKIE
    },
    #' @field session_cookie_settings Get the settings for the session cookie as
    #' they were provided during initialisation
    #' cookie *Immutable*
    #'
    session_cookie_settings = function() {
      private$SESSION_COOKIE_SETTINGS
    },
    #' @field has_key Query whether the request was initialised with an
    #' encryption key *Immutable*
    #'
    has_key = function() {
      !is.null(private$KEY)
    },
    #' @field compression_limit Query the compression limit the request was
    #' initialized with *Immutable*
    #'
    compression_limit = function() {
      private$COMPRESSION_LIMIT
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
      if (self$trust && !is.null(self$headers$x_forwarded_host)) {
        self$headers$x_forwarded_host
      } else {
        private$HOST
      }
    },
    #' @field ip Returns the remote address of the request if `trust == FALSE`.
    #' If `trust == TRUE` it will instead return the first value of the
    #' `X-Forwarded-For` header. *Immutable*
    #'
    ip = function() {
      if (self$trust && !is.null(self$headers$x_forwarded_for)) {
        self$headers$x_forwarded_for[1]
      } else {
        private$IP
      }
    },
    #' @field ips If `trust == TRUE` it will return the full list of ips in the
    #' `X-Forwarded-For` header. If `trust == FALSE` it will return an empty
    #' vector. *Immutable*
    #'
    ips = function() {
      if (self$trust && !is.null(self$headers$x_forwarded_for)) {
        self$headers$x_forwarded_for
      } else {
        character(0)
      }
    },
    #' @field protocol Returns the protocol (e.g. 'http') used for the request.
    #' If `trust == TRUE` it will use the value of the `X-Forwarded-Proto` header.
    #' *Immutable*
    #'
    protocol = function() {
      if (self$trust && !is.null(self$headers$x_forwarded_proto)) {
        self$headers$x_forwarded_proto
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
      paste0(
        self$protocol,
        '://',
        self$host,
        self$root,
        self$path,
        self$querystring
      )
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
      if (missing(value)) {
        return(private$QUERYDELIM)
      }
      if (!is.null(value)) {
        value <- arg_match0(value, c(",", "|", " "))
      }
      private$QUERYDELIM <- value
      delayedAssign(
        "QUERY",
        private$parse_query(private$QUERYSTRING),
        assign.env = private
      )
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
    #' original rook object. Changing this will force the request to reparse
    #' itself.
    #'
    origin = function(value) {
      if (missing(value)) {
        return(private$ORIGIN)
      }
      if (!identical(private$ORIGIN, value)) {
        cli::cli_abort("It is not allowed to replace the origin store")
      }
      private$ORIGIN <- value
      private$reset()
    },
    #' @field response If a `Response` object has been created for this request
    #' it is accessible through this field. *Immutable*
    #'
    response = function(res) {
      if (missing(res)) {
        return(private$RESPONSE)
      }
      if (!is.null(private$RESPONSE)) {
        cli::cli_abort('Response can only be assigned once')
      }
      if (!inherits(res, 'Response')) {
        cli::cli_abort('{.arg res} must be a {.cls Response} object')
      }
      if (!identical(self, res$request)) {
        cli::cli_abort(
          '{.arg res} must be a Response responding to this request'
        )
      }
      private$RESPONSE <- res
    },
    #' @field locked Set the `locked` status on the request. This flag does not
    #' result in any different behaviour in the request but can be used by
    #' frameworks to signal that the request should not be altered in some way
    #'
    locked = function(value) {
      if (missing(value)) {
        return(private$LOCKED)
      }
      check_bool(value)
      private$LOCKED <- value
    },
    #' @field response_headers The list of headers the response is prepopulated
    #' with *Immutable*
    #'
    response_headers = function() {
      private$RESPONSE_HEADERS
    },
    #' @field otel_span An OpenTelemetry span to use as parent for any
    #' instrumentation happening during the handling of the request. If otel is
    #' not enabled then this will be NULL. The span is populated according to
    #' the HTTP Server semantics <https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server>,
    #' except for the `http.route` attribute, which must be set by the server
    #' implementation, along with a proper name for the span
    #' (`{method}_{route}`). The span is automatically closed when the response
    #' is converted to a list, unless asked not to. *Immutable*
    otel_span = function() {
      private$OSPAN
    },
    #' @field start_time The time point the Request was created
    start_time = function() {
      private$START
    },
    #' @field duration The time passed since the request was created
    duration = function() {
      as.numeric(Sys.time() - private$START)
    }
  ),
  private = list(
    TRUST = FALSE,
    ORIGIN = NULL,
    METHOD = NULL,
    KEY = NULL,
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
    SESSION = NULL,
    SESSION_COOKIE_SETTINGS = NULL,
    HAS_SESSION_COOKIE = FALSE,
    COMPRESSION_LIMIT = 0,
    RESPONSE_HEADERS = NULL,
    RESPONSE = NULL,
    LOCKED = FALSE,
    WITH_OTEL = FALSE,
    OSPAN = NULL,
    START = NULL,

    reset_hard = function() {
      private$TRUST <- FALSE
      private$ORIGIN <- NULL
      private$METHOD <- NULL
      private$KEY <- NULL
      private$HOST <- NULL
      private$PROTOCOL <- NULL
      private$ROOT <- NULL
      private$PATH <- NULL
      private$QUERYSTRING <- NULL
      private$QUERYDELIM <- NULL
      private$IP <- NULL
      private$QUERY <- NULL
      private$BODY <- NULL
      private$HEADERS <- NULL
      private$COOKIES <- NULL
      private$SESSION <- NULL
      private$SESSION_COOKIE_SETTINGS <- NULL
      private$HAS_SESSION_COOKIE <- FALSE
      private$COMPRESSION_LIMIT <- 0
      private$RESPONSE_HEADERS <- NULL
      private$LOCKED <- FALSE
      private$WITH_OTEL <- FALSE
      private$OSPAN <- NULL
      private$START <- NULL
      if (!is.null(private$RESPONSE)) {
        private$RESPONSE$reset()
      }
    },
    parse_cookies = function() {
      if (is.null(self$headers$cookie)) {
        return(list())
      }
      cookies <- stri_trim_both(stri_split_regex(
        self$headers$cookie,
        ";(?=\\s*[a-zA-Z0-9!#$%&'()*+-.\\/:<>?@\\[\\]^_`{|}~]{1,})"
      )[[1]])
      cookies <- unlist(stri_split_fixed(cookies, '=', n = 2))
      structure(
        as.list(url_decode(cookies[c(FALSE, TRUE)])),
        names = cookies[c(TRUE, FALSE)]
      )
    },
    parse_query = function(query) {
      query_parser(query, private$QUERYDELIM)
    },
    unpack = function(raw) {
      compression <- self$get_header('Content-Encoding')
      if (is.null(compression)) {
        return(raw)
      }
      compression <- rev(trimws(strsplit(compression, ',')[[1]]))
      Reduce(
        function(l, r) {
          switch(
            r,
            identity = l,
            br = brotli_decompress(l),
            gzip = ,
            "x-gzip" = {
              con <- gzcon(rawConnection(l))
              on.exit(close(con), add = TRUE)
              readBin(con, "raw", length(l))
            },
            deflate = memDecompress(l, type = 'gzip'),
            cli::cli_abort('Unsupported compression {.val {r}}')
          )
        },
        x = compression,
        init = raw
      )
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
    },
    get_session_cookie = function() {
      if (!private$HAS_SESSION_COOKIE) {
        return(list())
      }
      val <- self$decode_string(
        private$COOKIES[[private$SESSION_COOKIE_SETTINGS$name]]
      )
      if (is.null(val)) {
        return(list())
      }
      try_fetch(
        jsonlite::fromJSON(val),
        error = function(e) {
          cli::cli_warn("Failed to decode session cookie")
          list()
        }
      )
    },
    reset = function() {
      private$METHOD <- tolower(private$ORIGIN$REQUEST_METHOD)
      delayedAssign(
        "HEADERS",
        get_headers(private$ORIGIN),
        assign.env = private
      )
      if (is.null(private$ORIGIN$HTTP_HOST)) {
        private$HOST <- paste(
          private$ORIGIN$SERVER_NAME,
          private$ORIGIN$SERVER_PORT,
          sep = ':'
        )
      } else {
        private$HOST <- private$ORIGIN$HTTP_HOST
      }
      private$PROTOCOL <- private$ORIGIN$rook.url_scheme
      private$ROOT <- private$ORIGIN$SCRIPT_NAME
      private$PATH <- private$ORIGIN$PATH_INFO

      if (private$QUERYSTRING != private$ORIGIN$QUERY_STRING) {
        private$QUERYSTRING <- private$ORIGIN$QUERY_STRING
        if (private$QUERYSTRING != '') {
          private$QUERYSTRING <- paste0(
            '?',
            sub('^\\?', '', private$QUERYSTRING)
          )
        }
        private$IP <- private$ORIGIN$REMOTE_ADDR
        delayedAssign(
          "QUERY",
          private$parse_query(private$QUERYSTRING),
          assign.env = private
        )
      }

      delayedAssign("COOKIES", private$parse_cookies(), assign.env = private)

      delayedAssign(
        "SESSION",
        private$get_session_cookie(),
        assign.env = private
      )
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
  if (is.Request(x$.__reqres_Request__)) {
    return(x$.__reqres_Request__)
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

curl_call <- quote({
  curl_handle <- curl::new_handle()
  curl::handle_setopt(curl_handle, .list = opts)
  curl::handle_setheaders(curl_handle, .list = headers)
  curl::curl_fetch_memory(url, curl_handle)
})

excluded_headers <- c(
  "connection",
  "transfer-encoding",
  "keep-alive",
  "upgrade"
)

get_headers <- function(rook) {
  vars <- ls(rook)
  headers <- vars[grepl('^HTTP_', vars)]
  ans <- lapply(headers, function(head) {
    # FIXME: This doesn't detect if the escape has been escaped
    literals <- stringi::stri_locate_all_regex(
      rook[[head]],
      "(?<!\\\\)\".*?[^\\\\]\"",
      omit_no_match = TRUE
    )[[1]]
    if (length(literals) == 0) {
      stringi::stri_split_regex(
        rook[[head]],
        "(?<!Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s?"
      )[[1]]
    } else {
      splits <- stringi::stri_locate_all_regex(
        rook[[head]],
        "(?<!Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s?",
        omit_no_match = TRUE
      )[[1]]
      if (length(splits) == 0) {
        return(rook[[head]])
      }
      split_ind <- rep(seq_len(nrow(splits)), each = nrow(literals))
      splits2 <- splits[split_ind, ]
      inside <- splits2[, 1] > literals[, 1] & splits2[, 2] < literals[, 2]
      splits <- splits[-split_ind[inside], , drop = FALSE]
      if (length(splits) == 0) {
        return(rook[[head]])
      }
      splits <- c(0, t(splits) + c(-1, 1), stringi::stri_length(rook[[head]]))
      stringi::stri_sub_all(
        rook[[head]],
        splits[c(TRUE, FALSE)],
        splits[c(FALSE, TRUE)]
      )[[1]]
    }
  })
  names(ans) <- tolower(sub('^HTTP_', '', headers))
  ans
}
format_mimes <- function(type) {
  if (is.null(type) || length(type) == 0) {
    return(NULL)
  }
  type <- stri_match_first_regex(
    tolower(type),
    '^\\s*([^\\s\\/;]+)\\/([^;\\s]+)\\s*(?:;(.*))?$'
  )
  type <- as.data.frame(type, stringsAsFactors = FALSE)
  names(type) <- c('full', 'main', 'sub', 'q')
  type$q <- get_quality(type$q)
  type
}
format_charsets <- function(charsets) {
  if (is.null(charsets) || length(charsets) == 0) {
    return(NULL)
  }
  charsets <- stri_match_first_regex(
    tolower(charsets),
    '^\\s*([^\\s;]+)\\s*(?:;(.*))?$'
  )
  charsets <- as.data.frame(charsets, stringsAsFactors = FALSE)
  names(charsets) <- c('full', 'main', 'q')
  charsets$q <- get_quality(charsets$q)
  charsets
}
format_encodings <- function(encodings) {
  if (is.null(encodings) || length(encodings) == 0) {
    return(NULL)
  }
  encodings <- stri_match_first_regex(
    tolower(encodings),
    '^\\s*([^\\s;]+)\\s*(?:;(.*))?$'
  )
  encodings <- as.data.frame(encodings, stringsAsFactors = FALSE)
  names(encodings) <- c('full', 'main', 'q')
  encodings$q <- get_quality(encodings$q)
  if (!'identity' %in% encodings$main) {
    encodings <- rbind(
      encodings,
      data.frame(
        full = 'identity;q=0',
        main = 'identity',
        q = min(encodings$q),
        stringsAsFactors = FALSE
      )
    )
  }
  encodings
}
format_languages <- function(lang) {
  if (is.null(lang) || length(lang) == 0) {
    return(NULL)
  }
  lang <- stri_match_first_regex(
    tolower(lang),
    '^\\s*([^\\s\\-;]+)(?:-([^\\s;]+))?\\s*(?:;(.*))?$'
  )
  lang <- as.data.frame(lang, stringsAsFactors = FALSE)
  names(lang) <- c('full', 'main', 'sub', 'q')
  lang$q <- get_quality(lang$q)
  lang$complete <- paste0(
    lang$main,
    ifelse(is.na(lang$sub), '', paste0('-', lang$sub))
  )
  lang
}
format_types <- function(formats) {
  ext <- !grepl('/', formats)
  format_ind <- rep(NA_integer_, length(formats))
  formats[ext] <- sub('^[.]', '', formats[ext])
  format_ind[ext] <- mimes_ext$index[match(formats[ext], mimes_ext$ext)]
  format_ind[!ext] <- match(formats[!ext], mimes$name)
  mimes[format_ind[!is.na(format_ind)], ]
}
get_format_spec <- function(format, accepts) {
  f_split <- strsplit(format$name, '/')
  spec <- do.call(
    rbind,
    lapply(f_split, function(n) {
      spec <- ifelse(
        n[1] == accepts$main,
        4,
        ifelse(accepts$main == '*', 0, NA)
      )
      spec <- spec +
        ifelse(n[2] == accepts$sub, 2, ifelse(accepts$sub == '*', 0, NA))
      win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
      if (is.na(spec[win])) {
        c(NA, NA)
      } else {
        c(spec[win], win)
      }
    })
  )
  if (all(is.na(spec[, 1]))) {
    return(NULL)
  }
  order(
    accepts$q[spec[, 2]],
    spec[, 1],
    -spec[, 2],
    -seq_along(f_split),
    decreasing = TRUE
  )[1]
}
get_charset_spec <- function(charset, accepts) {
  spec <- do.call(
    rbind,
    lapply(charset, function(n) {
      spec <- ifelse(n == accepts$main, 1, ifelse(accepts$main == '*', 0, NA))
      win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
      if (is.na(spec[win])) {
        c(NA, NA)
      } else {
        c(spec[win], win)
      }
    })
  )
  if (all(is.na(spec[, 1]))) {
    return(NULL)
  }
  order(
    accepts$q[spec[, 2]],
    spec[, 1],
    -spec[, 2],
    -seq_along(charset),
    decreasing = TRUE
  )[1]
}
get_encoding_spec <- function(encoding, accepts) {
  spec <- do.call(
    rbind,
    lapply(encoding, function(n) {
      spec <- ifelse(n == accepts$main, 1, ifelse(accepts$main == '*', 0, NA))
      win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
      if (is.na(spec[win])) {
        c(NA, NA)
      } else {
        c(spec[win], win)
      }
    })
  )
  if (all(is.na(spec[, 1]))) {
    return(NULL)
  }
  order(
    accepts$q[spec[, 2]],
    spec[, 1],
    -spec[, 2],
    -seq_along(encoding),
    decreasing = TRUE
  )[1]
}
get_language_spec <- function(lang, accepts) {
  l_split <- strsplit(lang, '-')
  spec <- do.call(
    rbind,
    lapply(seq_along(lang), function(i) {
      spec <- ifelse(lang[i] == accepts$complete, 4, NA)
      spec <- ifelse(is.na(spec) & lang[i] == accepts$main, 2, spec)
      spec <- ifelse(is.na(spec) & l_split[[i]][1] == accepts$complete, 1, spec)
      spec <- ifelse(is.na(spec) & accepts$complete == '*', 0, spec)
      win <- order(spec, accepts$q, -seq_along(spec), decreasing = TRUE)[1]
      if (is.na(spec[win])) {
        c(NA, NA)
      } else {
        c(spec[win], win)
      }
    })
  )
  if (all(is.na(spec[, 1]))) {
    return(NULL)
  }
  order(
    accepts$q[spec[, 2]],
    spec[, 1],
    -spec[, 2],
    -seq_along(lang),
    decreasing = TRUE
  )[1]
}
