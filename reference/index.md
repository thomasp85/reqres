# Package index

## Request and response classes

The main raison d’être for reqres is to provide classes handling http
requests and responses. As both are coded as R6 classes with reference
semantics all functionality of the object are documented together with
the class.

- [`as.Request()`](https://reqres.data-imaginist.com/reference/Request.md)
  [`is.Request()`](https://reqres.data-imaginist.com/reference/Request.md)
  : HTTP Request Handling
- [`as.list(`*`<Response>`*`)`](https://reqres.data-imaginist.com/reference/Response.md)
  [`is.Response()`](https://reqres.data-imaginist.com/reference/Response.md)
  : HTTP Response handling

## Content parsing and formatting

A main part of working with http requests and responses is related to
getting the content of a request and setting the content of a response.
While the Request and Response classes handle content negotiation it is
up to the developer to tell how data of a certain type should be parsed
or formattet. reqres provides a range of parsers and formatters for
common exchange formats in order to ease the pain of this part.

- [`parse_json()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_plain()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_xml()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_html()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_multiform()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_queryform()`](https://reqres.data-imaginist.com/reference/parsers.md)
  [`parse_table()`](https://reqres.data-imaginist.com/reference/parsers.md)
  : Pre-supplied parsing generators
- [`default_parsers`](https://reqres.data-imaginist.com/reference/default_parsers.md)
  : A list of default parser mappings
- [`format_json()`](https://reqres.data-imaginist.com/reference/formatters.md)
  [`format_plain()`](https://reqres.data-imaginist.com/reference/formatters.md)
  [`format_xml()`](https://reqres.data-imaginist.com/reference/formatters.md)
  [`format_html()`](https://reqres.data-imaginist.com/reference/formatters.md)
  [`format_table()`](https://reqres.data-imaginist.com/reference/formatters.md)
  : Pre-supplied formatting generators
- [`default_formatters`](https://reqres.data-imaginist.com/reference/default_formatters.md)
  : A list of default formatter mappings

## Error handling

reqres provides a suite of helper functions to make it easy to report
problems to the client in a standard way. These are all based on the
HTTP Problems spec defined in [RFC
9457](https://datatracker.ietf.org/doc/html/rfc9457)

- [`abort_http_problem()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_status()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_bad_request()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_unauthorized()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_forbidden()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_not_found()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_method_not_allowed()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_not_acceptable()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_conflict()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_gone()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`abort_internal_error()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`handle_problem()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  [`is_reqres_problem()`](https://reqres.data-imaginist.com/reference/abort_http_problem.md)
  : Abort request processing with an HTTP problem response

## Utilities

reqres includes a small selection of utilities beyond the ones mentioned
above, which are listed here.

- [`to_http_date()`](https://reqres.data-imaginist.com/reference/http_date.md)
  [`from_http_date()`](https://reqres.data-imaginist.com/reference/http_date.md)
  : Format timestamps to match the HTTP specs
- [`query_parser()`](https://reqres.data-imaginist.com/reference/query_parser.md)
  : Parse a query string
- [`session_cookie()`](https://reqres.data-imaginist.com/reference/session_cookie.md)
  [`is_session_cookie_settings()`](https://reqres.data-imaginist.com/reference/session_cookie.md)
  : Collect settings for a session cookie
- [`random_key()`](https://reqres.data-imaginist.com/reference/random_key.md)
  : Generate a random key compatible with encryption and decryption in
  requests and responses
- [`mock_request()`](https://reqres.data-imaginist.com/reference/mock_request.md)
  : Create a mock request to use in testing
