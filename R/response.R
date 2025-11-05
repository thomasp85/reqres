#' HTTP Response handling
#'
#' @description
#' This class handles all functionality involved in crafting a http response.
#' Much of the functionality is inspired by the Request class in Express.js, so
#' [the documentation](https://expressjs.com/en/4x/api.html#res) for this will
#' complement this document. As `reqres` is build on top of the
#' [Rook specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
#' the `Response` object can be converted to a compliant list object to be
#' passed on to e.g. the `httpuv` handler. A `Response` object is always created
#' as a response to a `Request` object and contains a reference to the
#' originating `Request` object. A `Response` is always initialized with a
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
#'  will be `text/plain`. Existence of the file will be checked.}
#'  \item{`type`}{Get or sets the `Content-Type` header of the response based on
#'  a file extension or mime-type.}
#'  \item{`request`}{Get the original `Request` object that the object is
#'  responding to.}
#' }
#'
#' @seealso [`Request`] for handling http requests
#'
#' @importFrom R6 R6Class
#' @importFrom tools file_path_as_absolute file_ext
#' @importFrom urltools url_encode
#' @importFrom brotli brotli_compress
#'
#' @export
#'
#' @examples
#' fake_rook <- fiery::fake_request(
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
#' # Cleaning up connections
#' rm(fake_rook, req, res)
#' gc()
#'
Response <- R6Class(
  'Response',
  lock_object = FALSE,
  public = list(
    # Methods
    #' @description Create a new response from a Request object
    #' @param request The `Request` object that the `Response` is responding to
    #'
    initialize = function(request) {
      if (!is.null(request$response)) {
        cli::cli_abort(
          'A response has already been created for this request. Access it using the {.field response} field'
        )
      }
      private$REQUEST = request
      private$STATUS = 404L
      private$HEADERS = request$response_headers
      private$COOKIES = list()
      private$BODY = ''
      private$DATA = list()
      self$type <- 'text/plain'
      request$response <- self
    },
    #' @description Pretty printing of the object
    #' @param ... ignored
    #'
    print = function(...) {
      cli::cli_rule('An HTTP response')
      cli::cli_dl(c(
        "Status" = '{self$status} - {status_phrase(self$status)}',
        "Content type" = self$type
      ))
      cli::cli_text(cli::style_italic(
        '{cli::symbol$arrow_right} Responding to: {private$REQUEST$url}'
      ))
      invisible(self)
    },
    #' @description Sets the header given by `name`. `value` will be converted
    #' to character. A header will be added for each element in `value`. Use
    #' `append_header()` for setting headers without overwriting existing ones.
    #' @param name The name of the header to set
    #' @param value The value to assign to the header
    #'
    set_header = function(name, value) {
      check_character(name)
      private$HEADERS[[tolower(name)]] <- as.character(value)
      invisible(self)
    },
    #' @description Returns the header(s) given by `name`
    #' @param name The name of the header to retrieve the value for
    #'
    get_header = function(name) {
      check_string(name)
      private$HEADERS[[tolower(name)]]
    },
    #' @description Removes all headers given by `name`
    #' @param name The name of the header to remove
    #'
    remove_header = function(name) {
      check_string(name)
      private$HEADERS[tolower(name)] <- NULL
      invisible(self)
    },
    #' @description Test for the existence of any header given by `name`
    #' @param name The name of the header to look for
    #'
    has_header = function(name) {
      check_string(name)
      !is.null(private$HEADERS[[tolower(name)]])
    },
    #' @description Adds an additional header given by `name` with the value
    #' given by `value`. If the header does not exist yet it will be created.
    #' @param name The name of the header to append to
    #' @param value The value to assign to the header
    #'
    append_header = function(name, value) {
      check_character(name)
      name <- tolower(name)
      private$HEADERS[[name]] <- c(private$HEADERS[[name]], value)
      invisible(self)
    },
    #' @description Adds `value` to the internal data store and stores it with
    #' `key`
    #' @param key The identifier of the data you set
    #' @param value An R object
    #'
    set_data = function(key, value) {
      check_string(key)
      private$DATA[[key]] <- value
      invisible(self)
    },
    #' @description Retrieves the data stored under `key` in the internal data
    #' store.
    #' @param key The identifier of the data you wish to retrieve
    #'
    get_data = function(key) {
      check_string(key)
      private$DATA[[key]]
    },
    #' @description Removes the data stored under `key` in the internal data
    #' store.
    #' @param key The identifier of the data you wish to remove
    #'
    remove_data = function(key) {
      check_string(key)
      private$DATA[[key]] <- NULL
      invisible(self)
    },
    #' @description Queries whether the data store has an entry given by `key`
    #' @param key The identifier of the data you wish to look for
    #'
    has_data = function(key) {
      !is.null(private$DATA[[key]])
    },
    #' @description Set the `Date` header to the current time
    #'
    timestamp = function() {
      private$HEADERS[["date"]] <- current_time()
      invisible(self)
    },
    #' @description Sets the body to the file given by `file` and marks the
    #' response as a download by setting the `Content-Disposition` to
    #' `attachment; filename=<filename>`. Use the `type` argument to overwrite
    #' the automatic type inference from the file extension.
    #' @param file The path to a file
    #' @param filename The name of the file as it will appear to the client
    #' @param type The file type. If not given it will be inferred
    #'
    attach = function(file, filename = basename(file), type = NULL) {
      self$file <- file
      check_string(filename)
      if (!is.null(type)) {
        self$type <- type
      }
      self$as_download(filename)
      invisible(self)
    },
    #' @description Marks the response as a downloadable file, rather than data
    #' to be shown in the browser
    #' @param filename Optional filename as hint for the client
    #'
    as_download = function(filename = NULL) {
      private$HEADERS[["content-disposition"]] <- if (is.null(filename)) {
        "attachment"
      } else {
        paste0('attachment; filename="', filename, '"')
      }
      invisible(self)
    },
    #' @description Sets the status to `code` and sets the body to the
    #' associated status code description (e.g. `Bad Gateway` for `502L`)
    #' @param code The status code to set
    #' @param clear_headers Should all currently set headers be cleared (useful
    #' for converting a response to an error halfway through processing)
    #'
    status_with_text = function(code, clear_headers = FALSE) {
      if (clear_headers) {
        private$HEADERS <- list()
      }
      self$status <- code
      body <- status_phrase(code)
      if (is.na(body)) {
        body <- as.character(code)
      }
      private$BODY <- body
      self$type <- 'txt'
      private$IS_FORMATTED <- TRUE
      invisible(self)
    },
    #' @description Signals an API problem using the HTTP Problems spec
    #' [RFC 9457](https://datatracker.ietf.org/doc/html/rfc9457). This should
    #' only be used in cases where returning a bare response code is
    #' insufficient to describe the issue.
    #' @param code The HTTP status code to use
    #' @param detail A string detailing the problem. Make sure the information
    #' given does not pose a security risk
    #' @param title A human-readable title of the issue. Should not vary from
    #' instance to instance of the specific issue. If `NULL` then the status
    #' code title is used
    #' @param type A URI that uniquely identifies this type of problem. The URI
    #' must resolve to an HTTP document describing the problem in human readable
    #' text. If `NULL`, the most recent link to the given status code definition
    #' is used
    #' @param instance A unique identifier of the specific instance of this
    #' problem that can be used for further debugging. Can be omitted.
    #' @param clear_headers Should all currently set headers be cleared
    #'
    problem = function(
      code,
      detail,
      title = NULL,
      type = NULL,
      instance = NULL,
      clear_headers = TRUE
    ) {
      if (clear_headers) {
        private$HEADERS <- list()
      }
      self$status <- code
      private$BODY <- list(
        type = type %||% status_link(code),
        title = title %||% status_phrase(code),
        status = code,
        detail = detail
      )
      if (!is.null(instance)) {
        private$BODY$instance <- instance
      }
      self$format(
        json = format_json(auto_unbox = TRUE),
        # We add this so that browser client doesn't end up defaulting to xml which initiates a download
        html = format_json(auto_unbox = TRUE),
        xml = format_xml(),
        default = "json"
      )
      if (isTRUE(self$type == "application/json")) {
        self$type <- "application/problem+json"
      }
      if (isTRUE(self$type == "application/xml")) {
        self$type <- "application/problem+xml"
      }
      invisible(self)
    },
    #' @description Sets a cookie on the response. See
    #' <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie>
    #' for a longer description
    #' @param name The name of the cookie
    #' @param value The value of the cookie
    #' @param encode Should `value` be url encoded
    #' @param expires A POSIXct object given the expiration time of the cookie
    #' @param http_only Should the cookie only be readable by the browser
    #' @param max_age The number of seconds to elapse before the cookie expires
    #' @param path The URL path this cookie is related to
    #' @param secure Should the cookie only be send over https
    #' @param same_site Either `"Lax"`, `"Strict"`, or `"None"` indicating
    #' how the cookie can be send during cross-site requests. If this is set to
    #' `"None"` then `secure` *must* also be set to `TRUE`
    set_cookie = function(
      name,
      value,
      encode = TRUE,
      expires = NULL,
      http_only = NULL,
      max_age = NULL,
      path = NULL,
      secure = NULL,
      same_site = NULL
    ) {
      check_string(name)
      if (length(value) != 1) {
        cli::cli_abort("{.arg value} must be scalar")
      }
      ascii <- iconv(c(name, value), to = 'ASCII')
      if (anyNA(ascii)) {
        cli::cli_warn(
          'Cookie name and value must only use valid ASCII characters. Cookie {.field {name}} not set'
        )
      } else {
        if (encode) {
          value <- url_encode(value)
        }
        if (grepl('(^__Secure-)|(^__Host-)', name)) {
          secure <- TRUE
        }
        private$COOKIES[[name]] <- cookie(
          value,
          expires,
          http_only,
          max_age,
          path,
          secure,
          same_site
        )
      }
      invisible(self)
    },
    #' @description Removes the cookie named `name` from the response.
    #' @param name The name of the cookie to remove
    #'
    remove_cookie = function(name) {
      check_string(name)
      private$COOKIES[[name]] <- NULL
      invisible(self)
    },
    #' @description Request the client to delete the given cookie
    #' @param name The name of the cookie to delete
    #'
    clear_cookie = function(name) {
      check_string(name)
      if (!is.null(private$REQUEST$cookies[[name]])) {
        secure <- grepl('(^__Secure-)|(^__Host-)', name)
        private$COOKIES[[name]] <- if (secure) {
          secure_cookie_clearer
        } else {
          cookie_clearer
        }
      }
      invisible(self)
    },
    #' @description Queries whether the response contains a cookie named `name`
    #' @param name The name of the cookie to look for
    #'
    has_cookie = function(name) {
      check_string(name)
      !is.null(private$COOKIES[[name]])
    },
    #' @description Sets the `Link` header based on the named arguments passed
    #' to `...`. The names will be used for the `rel` directive.
    #' @param ... key-value pairs for the links
    #'
    set_links = function(...) {
      links <- list2(...)
      if (is.list(links[[1]])) {
        lifecycle::deprecate_soft(
          "0.3",
          I("Response$set_links(list(...))"),
          I("Response$set_links(!!!list(...))")
        )
        links <- modifyList(links[[1]], list2(...)[-1])
      }
      if (!is_named2(links)) {
        stop_input_type(links, "a named list")
      }
      url <- paste0('<', unlist(links), '>')
      rel <- paste0('rel="', names(links), '"')
      links <- paste(paste0(url, '; ', rel), collapse = ', ')
      self$set_header('Link', links)
      invisible(self)
    },
    #' @description Based on the formatters passed in through `...` content
    #' negotiation is performed with the request and the preferred formatter is
    #' chosen and applied. The `Content-Type` header is set automatically. If
    #' `compress = TRUE` the `compress()` method will be called after formatting.
    #' If an error is encountered and `autofail = TRUE` the response will be set
    #' to `500`. If a formatter is not found and `autofail = TRUE` the response
    #' will be set to `406`. If formatting is successful it will return `TRUE`,
    #' if not it will return `FALSE`
    #' @param ... A range of formatters
    #' @param autofail Automatically populate the response if formatting fails
    #' @param compress Should `$compress()` be run in the end
    #' @param default The name of the default formatter, which will be used if
    #' none match. Setting this will avoid autofailing with 406 as a formatter
    #' is always selected
    #'
    format = function(..., autofail = TRUE, compress = TRUE, default = NULL) {
      if (self$is_formatted) {
        cli::cli_warn(
          "The response has already been formatted. Will not format again"
        )
        return(FALSE)
      }
      if (!private$has_body()) {
        return(TRUE)
      }

      formatters <- list2(...)
      if (is.list(formatters[[1]])) {
        lifecycle::deprecate_soft(
          "0.3",
          I("Response$format(list(...))"),
          I("Response$format(!!!list(...))")
        )
        first_formatters <- names(formatters)[-1]
        formatters <- modifyList(formatters[[1]], list2(...)[-1])
        first_formatters <- names(formatters) %in% first_formatters
        formatters <- c(
          formatters[first_formatters],
          formatters[!first_formatters]
        )
      }
      if (!is_named2(formatters)) {
        stop_input_type(formatters, "a named list")
      }

      if (isTRUE(names(formatters) == default)) {
        # No need for content negotiation as default will always be used
        format <- default
      } else {
        format <- self$request$accepts(names(formatters)) %||% default
        self$append_header('Vary', 'Accept')
      }
      if (is.null(format)) {
        if (autofail) {
          types <- format_types(names(formatters))$name
          n <- length(types)
          if (n > 1) {
            types <- paste0(
              paste0(types[-n], collapse = ", "),
              if (n == 2) " or " else ", or ",
              types[n]
            )
          }
          detail <- paste0(
            "Only ",
            types,
            " content type",
            if (n > 1) "s" else "",
            " supported."
          )
          abort_not_acceptable(detail = detail)
        }
        return(FALSE)
      }

      content <- tri(formatters[[format]](self$body))
      if (is_reqres_problem(content)) {
        cnd_signal(content)
      } else if (is_condition(content)) {
        if (autofail) {
          abort_status(
            500L,
            "Error formatting the response body",
            parent = content
          )
        }
        return(FALSE)
      }

      private$IS_FORMATTED <- TRUE
      private$BODY <- content
      self$type <- format
      if (compress) {
        self$compress()
      }
      return(TRUE)
    },
    #' @description Based on the formatters passed in through `...` content
    #' negotiation is performed with the request and the preferred formatter is
    #' chosen. The `Content-Type` header is set automatically. If a formatter is
    #' not found and `autofail = TRUE` the response will be set to `406`. The
    #' found formatter is registered with the response and will be applied just
    #' before handing off the response to httpuv, unless the response has been
    #' manually formatted.
    #' @param ... A range of formatters
    #' @param autofail Automatically populate the response if formatting fails
    #' @param default The name of the default formatter, which will be used if
    #' none match. Setting this will avoid autofailing with 406 as a formatter
    #' is always selected
    #'
    set_formatter = function(..., autofail = TRUE, default = NULL) {
      formatters <- list2(...)
      if (isTRUE(names(formatters) == default)) {
        # No need for content negotiation as default will always be used
        format <- default
      } else {
        format <- self$request$accepts(names(formatters)) %||% default
        self$append_header('Vary', 'Accept')
      }
      if (is.null(format)) {
        if (autofail) {
          types <- format_types(names(formatters))$name
          n <- length(types)
          if (n > 1) {
            types <- paste0(
              paste0(types[-n], collapse = ", "),
              if (n == 2) " or " else ", or ",
              types[n]
            )
          }
          detail <- paste0(
            "Only ",
            types,
            " content type",
            if (n > 1) "s" else "",
            " supported."
          )
          abort_not_acceptable(detail)
        }
        return(FALSE)
      }
      private$FORMATTER <- formatters[[format]]
      self$type <- format
      return(TRUE)
    },
    #' @description Based on the provided priority, an encoding is negotiated
    #' with the request and applied. The `Content-Encoding` header is set to the
    #' chosen compression algorithm.
    #' @param priority A vector of compression types ranked by the servers
    #' priority
    #' @param force Should compression be done even if the type is known to be
    #' uncompressible
    #' @param limit The size limit in bytes for performing compression. If
    #' `NULL` then the `compression_limit` setting from the initialization of
    #' the request is used
    #'
    compress = function(
      priority = c('gzip', 'deflate', 'br', 'identity'),
      force = FALSE,
      limit = NULL
    ) {
      if (!force) {
        type <- self$type
        if (!is.null(type) && isFALSE(mimes$compressible[mimes$name == type])) {
          return(FALSE)
        }
      }
      if (!is_string(self$body)) {
        return(FALSE)
      }
      limit <- limit %||% private$REQUEST$compression_limit
      if (limit > nchar(self$body, "bytes")) {
        return(FALSE)
      }
      encoding <- self$request$accepts_encoding(priority)
      if (is.null(encoding)) {
        return(FALSE)
      }
      content <- switch(
        encoding,
        identity = self$body,
        gzip = gzip(charToRaw(self$body)),
        deflate = memCompress(charToRaw(self$body)),
        br = brotli_compress(charToRaw(self$body))
      )
      private$BODY <- content
      self$set_header('Content-Encoding', encoding)
      self$append_header('Vary', 'Accept-Encoding')
      return(TRUE)
    },
    #' @description Calculates the length (in bytes) of the body. This is the
    #' number that goes into the `Content-Length` header. Note that the
    #' `Content-Length` header is set automatically by `httpuv` so this method
    #' should only be called if the response size is needed for other reasons.
    #'
    content_length = function() {
      body <- private$format_body()
      if (length(body) == 1L && has_name(body, 'file')) {
        file.size(body)
      } else if (is.raw(body)) {
        length(body)
      } else {
        nchar(body, type = 'bytes')
      }
    },
    #' @description Converts the object to a list for further processing by
    #' a Rook compliant server such as `httpuv`. Will set `Content-Type` header
    #' if missing and convert a non-raw body to a single character string. Will
    #' apply the formatter set by `set_formatter()` unless the body has already
    #' been formatted. Will add a Date header if none exist.
    #'
    as_list = function() {
      if (!self$is_formatted && !is.null(self$formatter)) {
        private$IS_FORMATTED <- TRUE
        content <- tri(self$formatter(self$body))
        if (is_reqres_problem(content)) {
          cnd_signal(content)
        } else if (is_condition(content)) {
          abort_status(
            500L,
            "Error formatting the response body",
            parent = content
          )
        } else {
          private$BODY <- content
          self$compress()
        }
      }
      if (!self$has_header("Date")) {
        self$timestamp()
      }
      list(
        status = private$STATUS,
        headers = private$format_headers(),
        body = private$format_body()
      )
    },
    #' @description Prints a HTTP representation of the response to the output
    #' stream.
    #'
    as_message = function() {
      response <- self$as_list()
      cat(
        toupper(self$request$protocol),
        '/1.1 ',
        response$status,
        ' ',
        status_phrase(response$status),
        '\n',
        sep = ''
      )
      headers <- split_headers(response$headers)
      cat_headers(headers$response)
      cat('Content-Length: ', self$content_length(), '\n', sep = '')
      cat_headers(headers$entity)

      if (is.raw(response$body)) {
        body <- rawToChar(response$body, multiple = TRUE)
        body <- paste0(
          paste(head(body, 77), collapse = ''),
          if (length(body) > 77) '...' else ''
        )
      } else if (has_name(response$body, 'file')) {
        f <- file(response$body, 'rb')
        body <- rawToChar(
          readBin(f, raw(), n = 180, endian = 'little'),
          multiple = TRUE
        )
        body <- paste0(
          paste(head(body, 77), collapse = ''),
          if (length(body) > 77) '...' else ''
        )
      } else {
        body <- response$body
        body <- paste0(substr(body, 1, 77), if (nchar(body) > 77) '...' else '')
      }
      cat('\n')
      if (body == '') {
        cat('<No Body>\n')
      } else {
        body <- gsub('\n', '\\\\n', body)
        body <- gsub('\t', '\\\\t', body)
        cat(body, '\n', sep = '')
      }
    },
    #' @description base64-encode a string. If a key has been provided during
    #' initialisation the string is first encrypted and the final result is a
    #' combination of the encrypted text and the nonce, both base64 encoded and
    #' combined with a `"_"`.
    #' @param val A single string to encrypt
    encode_string = function(val) {
      private$REQUEST$encode_string(val)
    },
    #' @description base64-decodes a string. If a key has been provided during
    #' initialisation the input is first split by `"_"` and then the two parts
    #' are base64 decoded and decrypted. Otherwise the input is base64-decoded
    #' as-is. It will always hold that
    #' `val == decode_string(encode_string(val))`.
    #' @param val A single string to encrypt
    decode_string = function(val) {
      private$REQUEST$decode_string(val)
    },
    #' @description Resets the content of the response. Is mainly used by the
    #' `clear()` method of the associated request, and should seldom be called
    #' directly
    reset = function() {
      private$STATUS = 404L
      private$HEADERS = list()
      private$COOKIES = list()
      private$BODY = ''
      private$DATA = list()
      private$FORMATTER <- NULL
      private$IS_FORMATTED <- FALSE
      self$type <- 'text/plain'
    }
  ),
  active = list(
    #' @field status Gets or sets the status code of the response. Is
    #' initialised with `404L`
    #'
    status = function(code) {
      if (missing(code)) {
        return(private$STATUS)
      }
      if (is_integerish(code, 1L, TRUE)) {
        if (code < 100L || code > 599L) {
          cli::cli_abort('Response code ({.val {code}}) out of range')
        }
      }
      if (is_string(code)) {
        ind <- match(tolower(code), tolower(status$message))
        if (is.na(ind)) {
          cli::cli_abort('Unknown status: {.val {code}}')
        }
        code <- status$code[ind]
      }
      private$STATUS <- code
    },
    #' @field body Set or get he body of the response. If it is a character
    #' vector with a single element named `'file'` it will be interpreted as the
    #' location of a file. It is better to use the `file` field for creating a
    #' response referencing a file as it will automatically set the correct
    #' headers.
    #'
    body = function(content) {
      if (missing(content)) {
        return(private$BODY)
      }
      private$BODY <- content
      private$IS_FORMATTED <- FALSE
    },
    #' @field file Set or get the location of a file that should be used as the
    #' body of the response. If the body is not referencing a file (but contains
    #' something else) it will return `NULL`. The `Content-Type` header will
    #' automatically be inferred from the file extension, if known. If unknown
    #' it will defaults to `application/octet-stream`. If the file has no
    #' extension it will be `text/plain`. Existence of the file will be checked.
    #'
    file = function(path) {
      if (missing(path)) {
        if (
          length(private$BODY) != 1 || isTRUE(names(private$BODY) != 'file')
        ) {
          return(NULL)
        } else {
          return(private$BODY[['file']])
        }
      }
      check_string(path)
      file <- file_path_as_absolute(path)
      if (!file.exists(file)) {
        cli::cli_abort("{.arg file} doesn't exist")
      }
      self$type <- file_ext(file)
      private$BODY <- c(file = file)
      self$set_header('Last-Modified', to_http_date(file.mtime(file)))
    },
    #' @field type Get or sets the `Content-Type` header of the response based
    #' on a file extension or mime-type.
    #'
    type = function(type) {
      if (missing(type)) {
        return(self$get_header('Content-Type'))
      }
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
    #' @field request Get the original `Request` object that the object is
    #' responding to.
    #'
    request = function() {
      private$REQUEST
    },
    #' @field formatter Get the registered formatter for the response body.
    #'
    formatter = function() {
      private$FORMATTER
    },
    #' @field is_formatted Has the body been formatted
    #'
    is_formatted = function() {
      private$IS_FORMATTED
    },
    #' @field data_store Access the environment that holds the response data store
    data_store = function(value) {
      if (missing(value)) {
        return(private$DATA)
      }
      if (!identical(private$DATA, value)) {
        cli::cli_abort("It is not allowed to replace the data store")
      }
      private$DATA <- value
    },
    #' @field session The content of the session cookie. If session cookies has
    #' not been activated it will be an empty write-protected list. If session
    #' cookies are activated but the request did not contain one it will be an
    #' empty list. The content of this field will be send encrypted as part of
    #' the response according to the cookie settings in
    #' `$session_cookie_settings`. This field is reflected in the
    #' `Request$session` field and using either produces the same result
    #'
    session = function(value) {
      if (missing(value)) {
        return(private$REQUEST$session)
      }
      private$REQUEST$session <- value
    },
    #' @field session_cookie_settings Get the settings for the session cookie as
    #' they were provided during initialisation of the request
    #' cookie *Immutable*
    #'
    session_cookie_settings = function() {
      private$REQUEST$session_cookie_settings
    },
    #' @field has_key Query whether the request was initialised with an
    #' encryption key *Immutable*
    #'
    has_key = function() {
      private$REQUEST$has_key
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
    FORMATTER = NULL,
    IS_FORMATTED = FALSE,

    format_headers = function() {
      headers <- as.list(private$HEADERS)
      if (is.null(headers[['content-type']])) {
        headers[['content-type']] <- if (is.raw(private$BODY)) {
          'application/octet-stream'
        } else {
          'text/plain'
        }
      }
      headers <- structure(
        as.list(unlist(headers)),
        names = rep(names(headers), lengths(headers))
      )
      session_cookie <- character()
      if (length(self$session) != 0 && !is.null(self$session_cookie_settings)) {
        session_cookie <- paste0(
          self$session_cookie_settings$name,
          "=",
          url_encode(self$encode_string(jsonlite::toJSON(self$session))),
          self$session_cookie_settings$options
        )
      } else if (private$REQUEST$has_session_cookie) {
        self$clear_cookie(self$session_cookie_settings$name)
      }
      cookies <- as.list(private$COOKIES)
      cookies <- c(paste0(names(cookies), unlist(cookies)), session_cookie)
      c(
        headers,
        structure(
          as.list(cookies),
          names = rep('set-cookie', length(cookies))
        )
      )
    },
    format_body = function() {
      if (is.raw(private$BODY)) {
        private$BODY
      } else if (
        length(private$BODY) == 1L &&
          'file' %in% names(private$BODY)
      ) {
        private$BODY
      } else {
        paste(as.character(private$BODY), collapse = '\n')
      }
    },
    has_body = function() {
      !is.null(private$BODY) &&
        length(private$BODY) != 0 &&
        !identical(private$BODY, '')
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

cookie <- function(
  value,
  expires = NULL,
  http_only = NULL,
  max_age = NULL,
  path = NULL,
  secure = NULL,
  same_site = NULL
) {
  opts <- paste0('=', value)
  if (!is.null(expires)) {
    if (length(expires) != 1) {
      stop_input_type(expires, "an object coercible to a single timepoint")
    }
    opts <- c(opts, paste0('Expires=', to_http_date(expires)))
  }
  if (!is.null(http_only)) {
    check_bool(http_only)
    if (isTRUE(http_only)) opts <- c(opts, 'HttpOnly')
  }
  if (!is.null(max_age)) {
    check_number_whole(max_age, min = 0)
    opts <- c(opts, paste0('Max-Age=', max_age))
  }
  if (!is.null(path)) {
    check_string(path)
    opts <- c(opts, paste0('Path=', path))
  }
  if (!is.null(secure)) {
    check_bool(secure)
    if (secure) opts <- c(opts, 'Secure')
  }
  if (!is.null(same_site)) {
    check_string(same_site)
    if (!same_site %in% c('Lax', 'Strict', 'None')) {
      cli::cli_abort(
        "{.arg same_site} must be {.or {.val {c('Lax', 'Strict', 'None')}}}"
      )
    }
    if (same_site == "None" && !isTRUE(secure)) {
      cli::cli_abort(
        "If {.arg same_site = \"None\"} then {.arg secure = TRUE} must also be used"
      )
    }
    opts <- c(opts, paste0('SameSite=', same_site))
  }
  paste(opts, collapse = '; ')
}
on_load(
  cookie_clearer <- cookie(
    "",
    expires = as.POSIXct(0, origin = "1970-01-01 00:00:00 GMT")
  )
)
on_load(
  secure_cookie_clearer <- cookie(
    "",
    expires = as.POSIXct(0, origin = "1970-01-01 00:00:00 GMT"),
    secure = TRUE
  )
)
gzip <- function(x) {
  f <- tempfile()
  con <- gzcon(file(f, open = 'wb'))
  writeBin(x, con)
  close(con)
  content <- readBin(f, raw(), file.info(f)$size)
  unlink(f)
  content
}
