problem_abort <- function(code) {
  force(code)
  function(detail, instance = NULL, ..., message = detail, call = caller_env()) {
    check_string(detail)
    rlang::error_cnd(
      class = "reqres_problem",
      status = code,
      detail = cli::ansi_strip(cli::format_inline(detail, .envir = call)),
      instance = instance,
      message = vapply(message, cli::format_inline, character(1), .envir = call),
      call = call,
      ...,
      use_cli_format = TRUE
    )
  }
}

#' Abort request processing with an HTTP problem response
#'
#' This set of functions throws a classed error indicating that the request
#' should be responded to with an HTTP problem according to the spec defined in
#' [RFC 9457](https://datatracker.ietf.org/doc/html/rfc9457) or a bare response
#' code. These conditions should be caught and handled by the `handle_problem()`
#' function.
#'
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
#' @inheritParams rlang::error_cnd
#' @inheritDotParams rlang::error_cnd
#'
#' @export
#'
abort_http_problem <- function(code, detail, title = NULL, type = NULL, instance = NULL, ..., message = detail, call = caller_env()) {
  check_string(detail)
  rlang::error_cnd(
    class = "reqres_problem",
    status = code,
    detail = cli::ansi_strip(cli::format_inline(detail, .envir = call)),
    instance = instance,
    title = title,
    type = type,
    message = vapply(message, cli::format_inline, character(1), .envir = call),
    call = call,
    ...,
    use_cli_format = TRUE
  )
}
#' @rdname abort_http_problem
#' @export
#'
abort_status <- function(code, message = status_phrase(code), ..., call = caller_env()) {
  rlang::error_cnd(
    class = "reqres_problem",
    status = code,
    call = call,
    message = vapply(message, cli::format_inline, character(1), .envir = call),
    ...,
    use_cli_format = TRUE
  )
}
#' @rdname abort_http_problem
#' @export
#'
abort_bad_request <- problem_abort(400L)
#' @rdname abort_http_problem
#' @export
#'
abort_unauthorized <- problem_abort(401L)
#' @rdname abort_http_problem
#' @export
#'
abort_forbidden <- problem_abort(403L)
#' @rdname abort_http_problem
#' @export
#'
abort_not_found <- problem_abort(404L)
#' @rdname abort_http_problem
#' @export
#'
abort_method_not_allowed <- problem_abort(405L)
#' @rdname abort_http_problem
#' @export
#'
abort_not_acceptable <- problem_abort(406L)
#' @rdname abort_http_problem
#' @export
#'
abort_conflict <- problem_abort(409L)
#' @rdname abort_http_problem
#' @export
#'
abort_gone <- problem_abort(410L)
#' @rdname abort_http_problem
#' @export
#'
abort_internal_error <- problem_abort(500L)

#' @rdname abort_http_problem
#'
#' @param response The Response object associated with the request that created
#' the condition
#' @param cnd The thrown condition
#'
#' @export
#'
handle_problem <- function(response, cnd) {
  if (is.null(cnd$detail)) {
    response$status_with_text(cnd$status)
  } else {
    response$problem(
      code = cnd$status,
      detail = cli::ansi_strip(cnd$detail),
      title = cnd$title,
      type = cnd$type,
      instance = cnd$instance
    )
  }
}

#' @rdname abort_http_problem
#' @export
#'
is_reqres_problem <- function(cnd) inherits(cnd, "reqres_problem")
