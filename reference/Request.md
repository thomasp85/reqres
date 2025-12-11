# HTTP Request Handling

This class wraps all functionality related to extracting information
from a http request. Much of the functionality is inspired by the
Request class in Express.js, so [the
documentation](https://expressjs.com/en/4x/api.html#req) for this will
complement this document. As `reqres` is build on top of the [Rook
specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
the `Request` object is initialized from a Rook-compliant object. This
will often be the request object provided by the `httpuv` framework.
While it shouldn't be needed, the original Rook object is always
accessible and can be modified, though any modifications will not
propagate to derived values in the `Request` object (e.g. changing the
`HTTP_HOST` element of the Rook object will not change the `host` field
of the `Request` object). Because of this, direct manipulation of the
Rook object is generally discouraged.

## Usage

``` r
as.Request(x, ...)

is.Request(x)
```

## Arguments

- x:

  An object coercible to a `Request`.

- ...:

  Parameters passed on to `Request$new()`

## Value

A `Request` object (for `as.Request()`) or a logical indicating whether
the object is a `Request` (for `is.Request()`)

## Initialization

A new 'Request'-object is initialized using the `new()` method on the
generator:

**Usage**

|                                           |
|-------------------------------------------|
| `req <- Request$new(rook, trust = FALSE)` |

## See also

[`Response`](https://reqres.data-imaginist.com/reference/Response.md)
for handling http responses

## Active bindings

- `trust`:

  A logical indicating whether the request is trusted. *Mutable*

- `method`:

  A string indicating the request method (in lower case, e.g. 'get',
  'put', etc.). *Immutable*

- `body`:

  An object holding the body of the request. This is an empty string by
  default and needs to be populated using the `set_body()` method (this
  is often done using a body parser that accesses the Rook\$input
  stream). *Immutable*

- `body_raw`:

  The raw content of the request body as a raw vector. No unpacking or
  parsing has been performed on this, even if the request has been
  parsed.

- `session`:

  The content of the session cookie. If session cookies has not been
  activated it will be an empty write-protected list. If session cookies
  are activated but the request did not contain one it will be an empty
  list. The content of this field will be send encrypted as part of the
  response according to the cookie settings in
  `$session_cookie_settings`. This field is reflected in the
  `Response$session` field and using either produces the same result

- `has_session_cookie`:

  Query whether the request came with a session cookie *Immutable*

- `session_cookie_settings`:

  Get the settings for the session cookie as they were provided during
  initialisation cookie *Immutable*

- `has_key`:

  Query whether the request was initialised with an encryption key
  *Immutable*

- `compression_limit`:

  Query the compression limit the request was initialized with
  *Immutable*

- `cookies`:

  Access a named list of all cookies in the request. These have been URI
  decoded. *Immutable*

- `headers`:

  Access a named list of all headers in the request. In order to follow
  R variable naming standards `-` have been substituted with `_`. Use
  the `get_header()` method to lookup based on the correct header name.
  *Immutable*

- `host`:

  Return the domain of the server given by the "Host" header if
  `trust == FALSE`. If `trust == true` returns the `X-Forwarded-Host`
  instead. *Immutable*

- `ip`:

  Returns the remote address of the request if `trust == FALSE`. If
  `trust == TRUE` it will instead return the first value of the
  `X-Forwarded-For` header. *Immutable*

- `ips`:

  If `trust == TRUE` it will return the full list of ips in the
  `X-Forwarded-For` header. If `trust == FALSE` it will return an empty
  vector. *Immutable*

- `protocol`:

  Returns the protocol (e.g. 'http') used for the request. If
  `trust == TRUE` it will use the value of the `X-Forwarded-Proto`
  header. *Immutable*

- `root`:

  The mount point of the application receiving this request. Can be
  empty if the application is mounted on the server root. *Immutable*

- `path`:

  The part of the url following the root. Defines the local target of
  the request (independent of where it is mounted). *Immutable*

- `url`:

  The full URL of the request. *Immutable*

- `query`:

  The query string of the request (anything following "?" in the URL)
  parsed into a named list. The query has been url decoded and "+" has
  been substituted with space. Multiple queries are expected to be
  separated by either "&" or "\|". *Immutable*

- `query_delim`:

  The delimiter used for specifying multiple values in a query. If
  `NULL` then queries are expected to contain multiple key-value pairs
  for the same key in order to provide an array, e.g. `?arg1=3&arg1=7`.
  If setting it to `",""`, `"|"`, or `" "` then an array can be provided
  in a single key-value pair, e.g. `?arg1=3|7`

- `querystring`:

  The unparsed query string of the request, including "?". If no query
  string exists it will be `""` rather than `"?"`

- `xhr`:

  A logical indicating whether the `X-Requested-With` header equals
  `XMLHttpRequest` thus indicating that the request was performed using
  JavaScript library such as jQuery. *Immutable*

- `secure`:

  A logical indicating whether the request was performed using a secure
  connection, i.e. `protocol == 'https'`. *Immutable*

- `origin`:

  The original object used to create the `Request` object. As `reqres`
  currently only works with rook this will always return the original
  rook object. Changing this will force the request to reparse itself.

- `response`:

  If a `Response` object has been created for this request it is
  accessible through this field. *Immutable*

- `locked`:

  Set the `locked` status on the request. This flag does not result in
  any different behaviour in the request but can be used by frameworks
  to signal that the request should not be altered in some way

- `response_headers`:

  The list of headers the response is prepopulated with *Immutable*

- `otel_span`:

  An OpenTelemetry span to use as parent for any instrumentation
  happening during the handling of the request. If otel is not enabled
  then this will be NULL. The span is populated according to the HTTP
  Server semantics
  <https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server>,
  except for the `http.route` attribute, which must be set by the server
  implementation, along with a proper name for the span
  (`{method}_{route}`). The span is automatically closed when the
  response is converted to a list, unless asked not to. *Immutable*

- `start_time`:

  The time point the Request was created

- `duration`:

  The time passed since the request was created

## Methods

### Public methods

- [`Request$new()`](#method-Request-new)

- [`Request$print()`](#method-Request-print)

- [`Request$set_body()`](#method-Request-set_body)

- [`Request$set_cookies()`](#method-Request-set_cookies)

- [`Request$accepts()`](#method-Request-accepts)

- [`Request$accepts_charsets()`](#method-Request-accepts_charsets)

- [`Request$accepts_encoding()`](#method-Request-accepts_encoding)

- [`Request$accepts_language()`](#method-Request-accepts_language)

- [`Request$is()`](#method-Request-is)

- [`Request$get_header()`](#method-Request-get_header)

- [`Request$has_header()`](#method-Request-has_header)

- [`Request$respond()`](#method-Request-respond)

- [`Request$parse()`](#method-Request-parse)

- [`Request$parse_raw()`](#method-Request-parse_raw)

- [`Request$as_message()`](#method-Request-as_message)

- [`Request$encode_string()`](#method-Request-encode_string)

- [`Request$decode_string()`](#method-Request-decode_string)

- [`Request$clear()`](#method-Request-clear)

- [`Request$forward()`](#method-Request-forward)

- [`Request$clone()`](#method-Request-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new request from a rook object

#### Usage

    Request$new(
      rook,
      trust = FALSE,
      key = NULL,
      session_cookie = NULL,
      compression_limit = 0,
      query_delim = NULL,
      response_headers = list(),
      with_otel = TRUE
    )

#### Arguments

- `rook`:

  The
  [rook](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
  object to base the request on

- `trust`:

  Is this request trusted blindly. If `TRUE` `X-Forwarded-*` headers
  will be returned when querying host, ip, and protocol

- `key`:

  A 32-bit secret key as a hex encoded string or a raw vector to use for
  `$encode_string()` and `$decode_string()` and by extension to encrypt
  a session cookie. It must be given to turn on session cookie support.
  A valid key can be generated using
  [`random_key()`](https://reqres.data-imaginist.com/reference/random_key.md).
  NEVER STORE THE KEY IN PLAIN TEXT. Optimalle use the keyring package
  to store it or set it as an environment variable

- `session_cookie`:

  Settings for the session cookie created using
  [`session_cookie()`](https://reqres.data-imaginist.com/reference/session_cookie.md).
  Will be ignored if `key` is not provided to ensure session cookies are
  properly encrypted

- `compression_limit`:

  The size threshold in bytes for trying to compress the response body
  (it is still dependant on content negotiation)

- `query_delim`:

  The delimiter to split array-type query arguments by

- `response_headers`:

  A list of headers the response should be prepopulated with. All names
  must be in lower case and all elements must be character vectors. This
  is not checked but assumed

- `with_otel`:

  A boolean to indicate if otel instrumentation should be initiated with
  the creation of this request. Set to `FALSE` to avoid a span being
  started as well as metrics being recorded for this request. If `TRUE`
  you should call `request$clear()` as the last act of your request
  handling to ensure that the span is closed and that the duration
  metric is correctly reported.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty printing of the object

#### Usage

    Request$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `set_body()`

Sets the content of the request body. This method should mainly be used
in concert with a body parser that reads the `rook$input` stream

#### Usage

    Request$set_body(content)

#### Arguments

- `content`:

  An R object representing the body of the request

------------------------------------------------------------------------

### Method `set_cookies()`

Sets the cookies of the request. The cookies are automatically parsed
and populated, so this method is mainly available to facilitate cookie
signing and encryption

#### Usage

    Request$set_cookies(cookies)

#### Arguments

- `cookies`:

  A named list of cookie values

------------------------------------------------------------------------

### Method `accepts()`

Given a vector of response content types it returns the preferred one
based on the `Accept` header.

#### Usage

    Request$accepts(types)

#### Arguments

- `types`:

  A vector of types

------------------------------------------------------------------------

### Method `accepts_charsets()`

Given a vector of possible character encodings it returns the preferred
one based on the `Accept-Charset` header.

#### Usage

    Request$accepts_charsets(charsets)

#### Arguments

- `charsets`:

  A vector of charsets

------------------------------------------------------------------------

### Method `accepts_encoding()`

Given a vector of possible content encodings (usually compression
algorithms) it selects the preferred one based on the `Accept-Encoding`
header. If there is no match it will return `"identity"` signaling no
compression.

#### Usage

    Request$accepts_encoding(encoding)

#### Arguments

- `encoding`:

  A vector of encoding names

------------------------------------------------------------------------

### Method `accepts_language()`

Given a vector of possible content languages it selects the best one
based on the `Accept-Language` header.

#### Usage

    Request$accepts_language(language)

#### Arguments

- `language`:

  A vector of languages

------------------------------------------------------------------------

### Method `is()`

Queries whether the body of the request is in a given format by looking
at the `Content-Type` header. Used for selecting the best parsing
method.

#### Usage

    Request$is(type)

#### Arguments

- `type`:

  A vector of content types to check for. Can be fully qualified MIME
  types, a file extension, or a mime type with wildcards

------------------------------------------------------------------------

### Method `get_header()`

Get the header of the specified name.

#### Usage

    Request$get_header(name)

#### Arguments

- `name`:

  The name of the header to get

------------------------------------------------------------------------

### Method `has_header()`

Test for the existence of any header given by `name`

#### Usage

    Request$has_header(name)

#### Arguments

- `name`:

  The name of the header to look for

------------------------------------------------------------------------

### Method `respond()`

Creates a new `Response` object from the request

#### Usage

    Request$respond()

------------------------------------------------------------------------

### Method [`parse()`](https://rdrr.io/r/base/parse.html)

Based on provided parsers it selects the appropriate one by looking at
the `Content-Type` header and assigns the result to the request body. A
parser is a function accepting a raw vector, and a named list of
additional directives, and returns an R object of any kind (if the
parser knows the input to be plain text, simply wrap it in
[`rawToChar()`](https://rdrr.io/r/base/rawConversion.html)). If the body
is compressed, it will be decompressed based on the `Content-Encoding`
header prior to passing it on to the parser. See
[parsers](https://reqres.data-imaginist.com/reference/parsers.md) for a
list of pre-supplied parsers. Parsers are either supplied in a named
list or as named arguments to the parse method. The names should
correspond to mime types or known file extensions. If `autofail = TRUE`
the response will throw an appropriate abort code if failing to parse
the body. [`parse()`](https://rdrr.io/r/base/parse.html) returns `TRUE`
if parsing was successful and `FALSE` if not

#### Usage

    Request$parse(..., autofail = TRUE)

#### Arguments

- `...`:

  A named set of parser functions

- `autofail`:

  Automatically populate the response if parsing fails

------------------------------------------------------------------------

### Method `parse_raw()`

This is a simpler version of the
[`parse()`](https://rdrr.io/r/base/parse.html) method. It will attempt
to decompress the body and set the `body` field to the resulting raw
vector. It is then up to the server to decide how to handle the payload.
It returns `TRUE` if successful and `FALSE` otherwise.

#### Usage

    Request$parse_raw(autofail = TRUE)

#### Arguments

- `autofail`:

  Automatically populate the response if parsing fails

------------------------------------------------------------------------

### Method `as_message()`

Prints a HTTP representation of the request to the output stream.

#### Usage

    Request$as_message()

------------------------------------------------------------------------

### Method `encode_string()`

base64-encode a string. If a key has been provided during initialisation
the string is first encrypted and the final result is a combination of
the encrypted text and the nonce, both base64 encoded and combined with
a `"_"`.

#### Usage

    Request$encode_string(val)

#### Arguments

- `val`:

  A single string to encrypt

------------------------------------------------------------------------

### Method `decode_string()`

base64-decodes a string. If a key has been provided during
initialisation the input is first split by `"_"` and then the two parts
are base64 decoded and decrypted. Otherwise the input is base64-decoded
as-is. It will always hold that
`val == decode_string(encode_string(val))`.

#### Usage

    Request$decode_string(val)

#### Arguments

- `val`:

  A single string to encrypt

------------------------------------------------------------------------

### Method `clear()`

Clears the content of the request and, if created, the related response.
This method exists only to allow reuse of the request and response
object to save a few milliseconds in latency. Use with caution and see
e.g. how fiery maintains a poll of request objects

#### Usage

    Request$clear()

------------------------------------------------------------------------

### Method `forward()`

Forward a request to a new url, optionally setting different headers,
queries, etc. Uses curl and mirai under the hood and returns a promise

#### Usage

    Request$forward(
      url,
      query = NULL,
      method = NULL,
      headers = NULL,
      body = NULL,
      return = NULL,
      ...
    )

#### Arguments

- `url`:

  The url to forward to

- `query`:

  Optional querystring to append to `url`. If `NULL` the query string of
  the current request will be used

- `method`:

  The HTTP method to use. If `NULL` the method of the current request
  will be used

- `headers`:

  A list of headers to add to the headers of the current request. You
  can remove a header from the current request by setting it to `NULL`
  here

- `body`:

  The body to send with the forward. If `NULL` the body of the current
  request will be used

- `return`:

  A function that takes in the fulfilled response object and whose
  return value is returned by the promise

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Request$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
fake_rook <- fiery::fake_request(
  'http://example.com/test?id=34632&question=who+is+hadley',
  content = 'This is an elaborate ruse',
  headers = list(
    Accept = 'application/json; text/*',
    Content_Type = 'text/plain'
  )
)

req <- Request$new(fake_rook)

# Get full URL
req$url
#> [1] "http://example.com:80/test?id=34632&question=who+is+hadley"

# Get list of query parameters
req$query
#> $id
#> [1] "34632"
#> 
#> $question
#> [1] "who is hadley"
#> 

# Test if content is text
req$is('txt')
#> [1] TRUE
#> attr(,"pick")
#> [1] 1

# Perform content negotiation for the response
req$accepts(c('html', 'json', 'txt'))
#> [1] "json"

# Cleaning up connections
rm(fake_rook, req)
gc()
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1132794 60.5    2190065  117  2190065  117
#> Vcells 2105593 16.1    8388608   64  8388559   64
```
