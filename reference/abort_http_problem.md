# Abort request processing with an HTTP problem response

This set of functions throws a classed error indicating that the request
should be responded to with an HTTP problem according to the spec
defined in [RFC 9457](https://datatracker.ietf.org/doc/html/rfc9457) or
a bare response code. These conditions should be caught and handled by
the `handle_problem()` function.

## Usage

``` r
abort_http_problem(
  code,
  detail,
  title = NULL,
  type = NULL,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_status(code, message = status_phrase(code), ..., call = caller_env())

abort_bad_request(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_unauthorized(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_forbidden(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_not_found(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_method_not_allowed(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_not_acceptable(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_conflict(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

abort_gone(detail, instance = NULL, ..., message = detail, call = caller_env())

abort_internal_error(
  detail,
  instance = NULL,
  ...,
  message = detail,
  call = caller_env()
)

handle_problem(response, cnd)

is_reqres_problem(cnd)
```

## Arguments

- code:

  The HTTP status code to use

- detail:

  A string detailing the problem. Make sure the information given does
  not pose a security risk

- title:

  A human-readable title of the issue. Should not vary from instance to
  instance of the specific issue. If `NULL` then the status code title
  is used

- type:

  A URI that uniquely identifies this type of problem. The URI must
  resolve to an HTTP document describing the problem in human readable
  text. If `NULL`, the most recent link to the given status code
  definition is used

- instance:

  A unique identifier of the specific instance of this problem that can
  be used for further debugging. Can be omitted.

- ...:

  Arguments passed on to
  [`rlang::error_cnd`](https://rlang.r-lib.org/reference/cnd.html)

  `class`

  :   The condition subclass.

  `use_cli_format`

  :   Whether to use the cli package to format `message`. See
      [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.html).

  `trace`

  :   A `trace` object created by
      [`trace_back()`](https://rlang.r-lib.org/reference/trace_back.html).

  `parent`

  :   A parent condition object.

- message:

  A default message to inform the user about the condition when it is
  signalled.

- call:

  A function call to be included in the error message. If an execution
  environment of a running function, the corresponding function call is
  retrieved.

- response:

  The Response object associated with the request that created the
  condition

- cnd:

  The thrown condition
