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
