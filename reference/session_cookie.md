# Collect settings for a session cookie

A session cookie is just like any other cookie, but reqres treats this
one different, parsing it's value and making it available in the
`$session` field. However, the same settings as any other cookies
applies and can be given during request initialisation using this
function.

## Usage

``` r
session_cookie(
  name = "reqres",
  expires = NULL,
  max_age = NULL,
  path = NULL,
  secure = NULL,
  same_site = NULL
)

is_session_cookie_settings(x)
```

## Arguments

- name:

  The name of the cookie

- expires:

  A POSIXct object given the expiration time of the cookie

- max_age:

  The number of seconds to elapse before the cookie expires

- path:

  The URL path this cookie is related to

- secure:

  Should the cookie only be send over https

- same_site:

  Either `"Lax"`, `"Strict"`, or `"None"` indicating how the cookie can
  be send during cross-site requests. If this is set to `"None"` then
  `secure` *must* also be set to `TRUE`

- x:

  An object to test

## Value

A `session_cookie_settings` object that can be used during request
initialisation. Can be cached and reused for all requests in a server

## Note

As opposed to regular cookies the session cookie is forced to be HTTP
only which is why this argument is missing.

## Examples

``` r
session_cookie <- session_cookie()

rook <- fiery::fake_request("http://example.com")

# A key must be provided for session_cookie to be used
Request$new(rook, key = random_key(), session_cookie = session_cookie)
#> ── An HTTP request ─────────────────────────────────────────────────────────────
#> Trusted: No
#> Method: get
#> URL: http://example.com:80/
```
