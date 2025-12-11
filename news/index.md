# Changelog

## reqres (development version)

## reqres 1.2.0

- Added
  [`unclass_request()`](https://reqres.data-imaginist.com/reference/maybe_request.md)
  and
  [`maybe_request()`](https://reqres.data-imaginist.com/reference/maybe_request.md)
  to allow you to strip off a bit of overhead by working with the
  objects as bare environments
- Added
  [`mock_request()`](https://reqres.data-imaginist.com/reference/mock_request.md)
  to create a mock request

## reqres 1.1.0

CRAN release: 2025-11-06

- Add support for otel OpenTelemetry by creating a span along with a
  request and close it once done. spans created during the handling of
  the request should use this span as a parent. Further, support the
  metrics defined in the HTTP semantics
- Added `body_raw` field to `Request` to read the raw, unparsed and
  unpacked, content of the request body
- `set_header()` and `append_header()` now allows vector input to set
  multiple values to a header

## reqres 1.0.0

CRAN release: 2025-08-20

- Use rlang native type checking instead of assertthat
- Avoid request parsing until needed (if ever)
- Fix bug that resulted in unintentional splitting of headers containing
  date-times ([\#11](https://github.com/thomasp85/reqres/issues/11))
- Improved query parsing that properly handles various forms of array
  notation (exploded and non-exploded with different delimiters).
- BREAKING: query values are no longer automatically type converted
  during parsing as this could lead to loss of information.
- DEPRECATED: `Request$parse()`, `Response$set_links()`, and
  `Response$format()` has soft deprecated passing in a list of values as
  the first element. Instead use `!!!` splicing
- `Response$status_with_text()` now has a `clear_headers` argument
- Added `Response$set_formatter()` method and `Response$formatter` and
  `Response$is_formatted` fields to allow delaying formatting until the
  response is sent off
- The `Date` header is now always added if missing
- `Request$is()` is now vectorised and provides the prefered choice as
  an attribute in the return
- New condition signalling based on the HTTP Problems Spec. Request and
  response objects will now throw classed errors where they are
  encounters so that server implementations can catch these and handle
  them sensibly. This is in opposition to the prior setup where reqres
  would handle any errors internally leaving the server implementation
  in the blank
- Added `Response$clear_cookie()` for requesting the client to remove
  the cookie
- `Response$set_cookie()` now also accepts `same_site = "None"` if
  `secure = TRUE` is also given
- Added `Request$encode_string()` and `Request$decode_string()` with the
  possibility of encrypting a string as well with a `key` given during
  initialisation
- Added facilities for maintaining a session data store through an
  encrypted session cookie. The data store is made available through the
  `session` field in both `Request` and `Response` and the content will
  automatically be send along with the response as an encrypted cookie.
- Added
  [`random_key()`](https://reqres.data-imaginist.com/reference/random_key.md)
  and
  [`session_cookie()`](https://reqres.data-imaginist.com/reference/session_cookie.md)
  helper functions to support the above
- Added `Response$data_store` field to suppleant `Response$get_data()`
  and friends. It provides direct acces to the response data store
- Fixed bug in
  [`format_xml()`](https://reqres.data-imaginist.com/reference/formatters.md)
  and
  [`format_html()`](https://reqres.data-imaginist.com/reference/formatters.md)
  that prevented standard lists to be converted
- Responses now correctly sets the `Vary` header when performing content
  negotiation
- Add `compression_limit` setting to control when compression is tried
- `Request` and `Response` objects are no longer locked, as it decreases
  construction time. For debugging purpose you can still change this in
  the constructor class in your debug session
- [`to_http_date()`](https://reqres.data-imaginist.com/reference/http_date.md)
  is now written in C as formatting POSIX values had an unacceptable
  overhead
- Added some interface functions to the Mime database
- Add functionality for resetting and reusing objects to avoid
  construction overhead
- Added `Request$forward()` to asynchronously forward a request to
  another url and populate the `Response` object with the response
- `Request$new()` now takes a `response_headers` argument that can be
  set to a list of headers the response should be prepopulated with

## reqres 0.2.5

CRAN release: 2022-08-19

- General upkeep
- Fix bug whith unnamed cookies
  ([\#12](https://github.com/thomasp85/reqres/issues/12))

## reqres 0.2.3

CRAN release: 2019-10-02

- Fixed bug in Cookie parsing when cookie strings would include `=`
- Added pkgdown site at <https://reqres.data-imaginist.com>

## reqres 0.2.2

CRAN release: 2018-11-12

- Fixed bug in querystring parsing where the first key would retain the
  `?`

## reqres 0.2.1

CRAN release: 2017-10-25

- Added `querystring` field to `Request`.
- Added `calculate_length()` method to `Response`.
- Added `as_message()` method to `Request` and `Response`.

## reqres 0.2.0

CRAN release: 2017-08-12

- Moved to a shallow dependency of Rook, making it easier to substitute
  or expand to other request formats
- Added content negotiation and body parsing and formatting

## reqres 0.1.0

- Migrated Request and Response classes from
  [`routr`](https://github.com/thomasp85/routr)
