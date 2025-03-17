# reqres (development version)

* Use rlang native type checking instead of assertthat
* Avoid request parsing until needed (if ever)
* Fix bug that resulted in unintentional splitting of headers containing
  date-times (#11)
* Improved query parsing that properly handles various forms of array notation
  (exploded and non-exploded with different delimiters).
* BREAKING: query values are no longer automatically type converted during
  parsing as this could lead to loss of information.
* DEPRECATED: `Request$parse()`, `Response$set_links()`, and `Response$format()`
  has soft deprecated passing in a list of values as the first element. Instead
  use `!!!` splicing
* `Response$status_with_text()` now has a `clear_headers` argument
* Added `Response$set_formatter()` method and `Response$formatter` and
  `Response$is_formatted` fields to allow delaying formatting until the
  response is sent off
* The `Date` header is now always added if missing
* `Request$is()` is now vectorised and provides the prefered choice as an
  attribute in the return
* New condition signalling based on the HTTP Problems Spec. Request and response
  objects will now throw classed errors where they are encounters so that server
  implementations can catch these and handle them sensibly. This is in
  opposition to the prior setup where reqres would handle any errors internally
  leaving the server implementation in the blank
* Added `Response$clear_cookie()` for requesting the client to remove the cookie
* `Response$set_cookie()` now also accepts `same_site = "None"` if
  `secure = TRUE` is also given
* Added `Request$encode_string()` and `Request$decode_string()` with the
  possibility of encrypting a string as well with a `key` given during
  initialisation
* Added facilities for maintaining a session data store through an encrypted
  session cookie. The data store is made available through the `session` field
  in both `Request` and `Response` and the content will automatically be send
  along with the response as an encrypted cookie.
* Added `random_key()` and `session_cookie()` helper functions to support the
  above
* Added `Response$data_store` field to suppleant `Response$get_data()` and
  friends. It provides direct acces to the response data store
* Fixed bug in `format_xml()` and `format_html()` that prevented standard lists
  to be converted
* Responses now correctly sets the `Vary` header when performing content
  negotiation
* Add `compression_limit` setting to control when compression is tried
* `Request` and `Response` objects are no longer locked, as it decreases
  construction time. For debugging purpose you can still change this in the
  constructor class in your debug session
* `to_http_date()` is now written in C as formatting POSIX values had an
  unacceptable overhead
* Added some interface functions to the Mime database
* Add functionality for resetting and reusing objects to avoid construction
  overhead

# reqres 0.2.5

* General upkeep
* Fix bug whith unnamed cookies (#12)

# reqres 0.2.3

* Fixed bug in Cookie parsing when cookie strings would include `=`
* Added pkgdown site at https://reqres.data-imaginist.com

# reqres 0.2.2

* Fixed bug in querystring parsing where the first key would retain the `?`

# reqres 0.2.1

* Added `querystring` field to `Request`.
* Added `calculate_length()` method to `Response`.
* Added `as_message()` method to `Request` and `Response`.

# reqres 0.2.0

* Moved to a shallow dependency of Rook, making it easier to substitute or
  expand to other request formats
* Added content negotiation and body parsing and formatting

# reqres 0.1.0

* Migrated Request and Response classes from
  [`routr`](https://github.com/thomasp85/routr)
