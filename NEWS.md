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
