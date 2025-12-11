# HTTP Response handling

This class handles all functionality involved in crafting a http
response. Much of the functionality is inspired by the Request class in
Express.js, so [the
documentation](https://expressjs.com/en/4x/api.html#res) for this will
complement this document. As `reqres` is build on top of the [Rook
specifications](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
the `Response` object can be converted to a compliant list object to be
passed on to e.g. the `httpuv` handler. A `Response` object is always
created as a response to a `Request` object and contains a reference to
the originating `Request` object. A `Response` is always initialized
with a 404 Not Found code, an empty string as body and the
`Content-Type` header set to `text/plain`. As the `Content-Type` header
is required for `httpuv` to function, it will be inferred if missing
when converting to a list. If the body is a raw vector it will be set to
`application/octet-stream` and otherwise it will be set to `text/plain`.
It is always advised to consciously set the `Content-Type` header
though. The only exception is when attaching a standard file where the
type is inferred from the file extension automatically. Unless the body
is a raw vector it will automatically be converted to a character vector
and collapsed to a single string with `"\n"` separating the individual
elements before the `Response` object is converted to a list (that is,
the body can exist as any type of object up until the moment where the
`Response` object is converted to a list). To facilitate communication
between different middleware the `Response` object contains a data store
where information can be stored during the lifetime of the response.

## Usage

``` r
# S3 method for class 'Response'
as.list(x, ...)

is.Response(x)
```

## Arguments

- x:

  A `Response` object

- ...:

  Ignored

## Value

A rook-compliant list-response (in case of
[`as.list()`](https://rdrr.io/r/base/list.html)) or a logical indicating
whether the object is a `Response` (in case of `is.Response()`)

## Initialization

A new 'Response'-object is initialized using the `new()` method on the
generator:

**Usage**

|                                |
|--------------------------------|
| `res <- Response$new(request)` |

But often it will be provided by the request using the `respond()`
method, which will provide the response, creating one if it doesn't
exist

**Usage**

|                            |
|----------------------------|
| `res <- request$respond()` |

**Arguments**

|           |     |                                                           |
|-----------|-----|-----------------------------------------------------------|
| `request` |     | The `Request` object that the `Response` is responding to |

## Fields

The following fields are accessible in a `Response` object:

- `status`:

  Gets or sets the status code of the response. Is initialised with
  `404L`

- `body`:

  Set or get he body of the response. If it is a character vector with a
  single element named `'file'` it will be interpreted as the location
  of a file. It is better to use the `file` field for creating a
  response referencing a file as it will automatically set the correct
  headers.

- `file`:

  Set or get the location of a file that should be used as the body of
  the response. If the body is not referencing a file (but contains
  something else) it will return `NULL`. The `Content-Type` header will
  automatically be inferred from the file extension, if known. If
  unknown it will defaults to `application/octet-stream`. If the file
  has no extension it will be `text/plain`. Existence of the file will
  be checked.

- `type`:

  Get or sets the `Content-Type` header of the response based on a file
  extension or mime-type.

- `request`:

  Get the original `Request` object that the object is responding to.

## See also

[`Request`](https://reqres.data-imaginist.com/reference/Request.md) for
handling http requests

## Active bindings

- `status`:

  Gets or sets the status code of the response. Is initialised with
  `404L`

- `body`:

  Set or get he body of the response. If it is a character vector with a
  single element named `'file'` it will be interpreted as the location
  of a file. It is better to use the `file` field for creating a
  response referencing a file as it will automatically set the correct
  headers.

- `file`:

  Set or get the location of a file that should be used as the body of
  the response. If the body is not referencing a file (but contains
  something else) it will return `NULL`. The `Content-Type` header will
  automatically be inferred from the file extension, if known. If
  unknown it will defaults to `application/octet-stream`. If the file
  has no extension it will be `text/plain`. Existence of the file will
  be checked.

- `type`:

  Get or sets the `Content-Type` header of the response based on a file
  extension or mime-type.

- `request`:

  Get the original `Request` object that the object is responding to.

- `formatter`:

  Get the registered formatter for the response body.

- `is_formatted`:

  Has the body been formatted

- `data_store`:

  Access the environment that holds the response data store

- `session`:

  The content of the session cookie. If session cookies has not been
  activated it will be an empty write-protected list. If session cookies
  are activated but the request did not contain one it will be an empty
  list. The content of this field will be send encrypted as part of the
  response according to the cookie settings in
  `$session_cookie_settings`. This field is reflected in the
  `Request$session` field and using either produces the same result

- `session_cookie_settings`:

  Get the settings for the session cookie as they were provided during
  initialisation of the request cookie *Immutable*

- `has_key`:

  Query whether the request was initialised with an encryption key
  *Immutable*

## Methods

### Public methods

- [`Response$new()`](#method-Response-new)

- [`Response$print()`](#method-Response-print)

- [`Response$set_header()`](#method-Response-set_header)

- [`Response$get_header()`](#method-Response-get_header)

- [`Response$remove_header()`](#method-Response-remove_header)

- [`Response$has_header()`](#method-Response-has_header)

- [`Response$append_header()`](#method-Response-append_header)

- [`Response$set_data()`](#method-Response-set_data)

- [`Response$get_data()`](#method-Response-get_data)

- [`Response$remove_data()`](#method-Response-remove_data)

- [`Response$has_data()`](#method-Response-has_data)

- [`Response$timestamp()`](#method-Response-timestamp)

- [`Response$attach()`](#method-Response-attach)

- [`Response$as_download()`](#method-Response-as_download)

- [`Response$status_with_text()`](#method-Response-status_with_text)

- [`Response$problem()`](#method-Response-problem)

- [`Response$set_cookie()`](#method-Response-set_cookie)

- [`Response$remove_cookie()`](#method-Response-remove_cookie)

- [`Response$clear_cookie()`](#method-Response-clear_cookie)

- [`Response$has_cookie()`](#method-Response-has_cookie)

- [`Response$set_links()`](#method-Response-set_links)

- [`Response$format()`](#method-Response-format)

- [`Response$set_formatter()`](#method-Response-set_formatter)

- [`Response$compress()`](#method-Response-compress)

- [`Response$content_length()`](#method-Response-content_length)

- [`Response$as_list()`](#method-Response-as_list)

- [`Response$as_message()`](#method-Response-as_message)

- [`Response$encode_string()`](#method-Response-encode_string)

- [`Response$decode_string()`](#method-Response-decode_string)

- [`Response$reset()`](#method-Response-reset)

- [`Response$clone()`](#method-Response-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new response from a Request object

#### Usage

    Response$new(request)

#### Arguments

- `request`:

  The `Request` object that the `Response` is responding to

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty printing of the object

#### Usage

    Response$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `set_header()`

Sets the header given by `name`. `value` will be converted to character.
A header will be added for each element in `value`. Use
`append_header()` for setting headers without overwriting existing ones.

#### Usage

    Response$set_header(name, value)

#### Arguments

- `name`:

  The name of the header to set

- `value`:

  The value to assign to the header

------------------------------------------------------------------------

### Method `get_header()`

Returns the header(s) given by `name`

#### Usage

    Response$get_header(name)

#### Arguments

- `name`:

  The name of the header to retrieve the value for

------------------------------------------------------------------------

### Method `remove_header()`

Removes all headers given by `name`

#### Usage

    Response$remove_header(name)

#### Arguments

- `name`:

  The name of the header to remove

------------------------------------------------------------------------

### Method `has_header()`

Test for the existence of any header given by `name`

#### Usage

    Response$has_header(name)

#### Arguments

- `name`:

  The name of the header to look for

------------------------------------------------------------------------

### Method `append_header()`

Adds an additional header given by `name` with the value given by
`value`. If the header does not exist yet it will be created.

#### Usage

    Response$append_header(name, value)

#### Arguments

- `name`:

  The name of the header to append to

- `value`:

  The value to assign to the header

------------------------------------------------------------------------

### Method `set_data()`

Adds `value` to the internal data store and stores it with `key`

#### Usage

    Response$set_data(key, value)

#### Arguments

- `key`:

  The identifier of the data you set

- `value`:

  An R object

------------------------------------------------------------------------

### Method `get_data()`

Retrieves the data stored under `key` in the internal data store.

#### Usage

    Response$get_data(key)

#### Arguments

- `key`:

  The identifier of the data you wish to retrieve

------------------------------------------------------------------------

### Method `remove_data()`

Removes the data stored under `key` in the internal data store.

#### Usage

    Response$remove_data(key)

#### Arguments

- `key`:

  The identifier of the data you wish to remove

------------------------------------------------------------------------

### Method `has_data()`

Queries whether the data store has an entry given by `key`

#### Usage

    Response$has_data(key)

#### Arguments

- `key`:

  The identifier of the data you wish to look for

------------------------------------------------------------------------

### Method [`timestamp()`](https://rdrr.io/r/utils/savehistory.html)

Set the `Date` header to the current time

#### Usage

    Response$timestamp()

------------------------------------------------------------------------

### Method [`attach()`](https://rdrr.io/r/base/attach.html)

Sets the body to the file given by `file` and marks the response as a
download by setting the `Content-Disposition` to
`attachment; filename=<filename>`. Use the `type` argument to overwrite
the automatic type inference from the file extension.

#### Usage

    Response$attach(file, filename = basename(file), type = NULL)

#### Arguments

- `file`:

  The path to a file

- `filename`:

  The name of the file as it will appear to the client

- `type`:

  The file type. If not given it will be inferred

------------------------------------------------------------------------

### Method `as_download()`

Marks the response as a downloadable file, rather than data to be shown
in the browser

#### Usage

    Response$as_download(filename = NULL)

#### Arguments

- `filename`:

  Optional filename as hint for the client

------------------------------------------------------------------------

### Method `status_with_text()`

Sets the status to `code` and sets the body to the associated status
code description (e.g. `Bad Gateway` for `502L`)

#### Usage

    Response$status_with_text(code, clear_headers = FALSE)

#### Arguments

- `code`:

  The status code to set

- `clear_headers`:

  Should all currently set headers be cleared (useful for converting a
  response to an error halfway through processing)

------------------------------------------------------------------------

### Method `problem()`

Signals an API problem using the HTTP Problems spec [RFC
9457](https://datatracker.ietf.org/doc/html/rfc9457). This should only
be used in cases where returning a bare response code is insufficient to
describe the issue.

#### Usage

    Response$problem(
      code,
      detail,
      title = NULL,
      type = NULL,
      instance = NULL,
      clear_headers = TRUE
    )

#### Arguments

- `code`:

  The HTTP status code to use

- `detail`:

  A string detailing the problem. Make sure the information given does
  not pose a security risk

- `title`:

  A human-readable title of the issue. Should not vary from instance to
  instance of the specific issue. If `NULL` then the status code title
  is used

- `type`:

  A URI that uniquely identifies this type of problem. The URI must
  resolve to an HTTP document describing the problem in human readable
  text. If `NULL`, the most recent link to the given status code
  definition is used

- `instance`:

  A unique identifier of the specific instance of this problem that can
  be used for further debugging. Can be omitted.

- `clear_headers`:

  Should all currently set headers be cleared

------------------------------------------------------------------------

### Method `set_cookie()`

Sets a cookie on the response. See
<https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie>
for a longer description

#### Usage

    Response$set_cookie(
      name,
      value,
      encode = TRUE,
      expires = NULL,
      http_only = NULL,
      max_age = NULL,
      path = NULL,
      secure = NULL,
      same_site = NULL
    )

#### Arguments

- `name`:

  The name of the cookie

- `value`:

  The value of the cookie

- `encode`:

  Should `value` be url encoded

- `expires`:

  A POSIXct object given the expiration time of the cookie

- `http_only`:

  Should the cookie only be readable by the browser

- `max_age`:

  The number of seconds to elapse before the cookie expires

- `path`:

  The URL path this cookie is related to

- `secure`:

  Should the cookie only be send over https

- `same_site`:

  Either `"Lax"`, `"Strict"`, or `"None"` indicating how the cookie can
  be send during cross-site requests. If this is set to `"None"` then
  `secure` *must* also be set to `TRUE`

------------------------------------------------------------------------

### Method `remove_cookie()`

Removes the cookie named `name` from the response.

#### Usage

    Response$remove_cookie(name)

#### Arguments

- `name`:

  The name of the cookie to remove

------------------------------------------------------------------------

### Method `clear_cookie()`

Request the client to delete the given cookie

#### Usage

    Response$clear_cookie(name)

#### Arguments

- `name`:

  The name of the cookie to delete

------------------------------------------------------------------------

### Method `has_cookie()`

Queries whether the response contains a cookie named `name`

#### Usage

    Response$has_cookie(name)

#### Arguments

- `name`:

  The name of the cookie to look for

------------------------------------------------------------------------

### Method `set_links()`

Sets the `Link` header based on the named arguments passed to `...`. The
names will be used for the `rel` directive.

#### Usage

    Response$set_links(...)

#### Arguments

- `...`:

  key-value pairs for the links

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Based on the formatters passed in through `...` content negotiation is
performed with the request and the preferred formatter is chosen and
applied. The `Content-Type` header is set automatically. If
`compress = TRUE` the `compress()` method will be called after
formatting. If an error is encountered and `autofail = TRUE` the
response will be set to `500`. If a formatter is not found and
`autofail = TRUE` the response will be set to `406`. If formatting is
successful it will return `TRUE`, if not it will return `FALSE`

#### Usage

    Response$format(..., autofail = TRUE, compress = TRUE, default = NULL)

#### Arguments

- `...`:

  A range of formatters

- `autofail`:

  Automatically populate the response if formatting fails

- `compress`:

  Should `$compress()` be run in the end

- `default`:

  The name of the default formatter, which will be used if none match.
  Setting this will avoid autofailing with 406 as a formatter is always
  selected

------------------------------------------------------------------------

### Method `set_formatter()`

Based on the formatters passed in through `...` content negotiation is
performed with the request and the preferred formatter is chosen. The
`Content-Type` header is set automatically. If a formatter is not found
and `autofail = TRUE` the response will be set to `406`. The found
formatter is registered with the response and will be applied just
before handing off the response to httpuv, unless the response has been
manually formatted.

#### Usage

    Response$set_formatter(..., autofail = TRUE, default = NULL)

#### Arguments

- `...`:

  A range of formatters

- `autofail`:

  Automatically populate the response if formatting fails

- `default`:

  The name of the default formatter, which will be used if none match.
  Setting this will avoid autofailing with 406 as a formatter is always
  selected

------------------------------------------------------------------------

### Method `compress()`

Based on the provided priority, an encoding is negotiated with the
request and applied. The `Content-Encoding` header is set to the chosen
compression algorithm.

#### Usage

    Response$compress(
      priority = c("gzip", "deflate", "br", "identity"),
      force = FALSE,
      limit = NULL
    )

#### Arguments

- `priority`:

  A vector of compression types ranked by the servers priority

- `force`:

  Should compression be done even if the type is known to be
  uncompressible

- `limit`:

  The size limit in bytes for performing compression. If `NULL` then the
  `compression_limit` setting from the initialization of the request is
  used

------------------------------------------------------------------------

### Method `content_length()`

Calculates the length (in bytes) of the body. This is the number that
goes into the `Content-Length` header. Note that the `Content-Length`
header is set automatically by `httpuv` so this method should only be
called if the response size is needed for other reasons.

#### Usage

    Response$content_length()

------------------------------------------------------------------------

### Method `as_list()`

Converts the object to a list for further processing by a Rook compliant
server such as `httpuv`. Will set `Content-Type` header if missing and
convert a non-raw body to a single character string. Will apply the
formatter set by `set_formatter()` unless the body has already been
formatted. Will add a Date header if none exist.

#### Usage

    Response$as_list()

------------------------------------------------------------------------

### Method `as_message()`

Prints a HTTP representation of the response to the output stream.

#### Usage

    Response$as_message()

------------------------------------------------------------------------

### Method `encode_string()`

base64-encode a string. If a key has been provided during initialisation
the string is first encrypted and the final result is a combination of
the encrypted text and the nonce, both base64 encoded and combined with
a `"_"`.

#### Usage

    Response$encode_string(val)

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

    Response$decode_string(val)

#### Arguments

- `val`:

  A single string to encrypt

------------------------------------------------------------------------

### Method `reset()`

Resets the content of the response. Is mainly used by the `clear()`
method of the associated request, and should seldom be called directly

#### Usage

    Response$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Response$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
fake_rook <- fiery::fake_request(
  'http://example.com/test?id=34632&question=who+is+hadley',
  content = 'This is elaborate ruse',
  headers = list(
    Accept = 'application/json; text/*',
    Content_Type = 'text/plain'
  )
)

req <- Request$new(fake_rook)
res <- Response$new(req)
res
#> ── An HTTP response ────────────────────────────────────────────────────────────
#> Status: 404 - Not Found
#> Content type: text/plain
#> → Responding to: http://example.com:80/test?id=34632&question=who+is+hadley

# Set the body to the associated status text
res$status_with_text(200L)
res$body
#> [1] "OK"

# Infer Content-Type from file extension
res$type <- 'json'
res$type
#> [1] "application/json"

# Prepare a file for download
res$attach(system.file('DESCRIPTION', package = 'reqres'))
res$type
#> [1] "text/plain"
res$body
#>                                                 file 
#> "/home/runner/work/_temp/Library/reqres/DESCRIPTION" 
res$get_header('Content-Disposition')
#> [1] "attachment; filename=\"DESCRIPTION\""

# Cleaning up connections
rm(fake_rook, req, res)
gc()
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1152573 61.6    2190082  117  2190082  117
#> Vcells 2150443 16.5    8388608   64  8388606   64
```
