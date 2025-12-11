# Format timestamps to match the HTTP specs

Dates/times in HTTP headers needs a specific format to be valid, and is
furthermore always given in GMT time. These two functions aids in
converting back and forth between the required format.

## Usage

``` r
to_http_date(time, format = NULL)

from_http_date(time)
```

## Arguments

- time:

  A string or an object coercible to POSIXct

- format:

  In case `time` is not a POSIXct object a specification how the string
  should be interpreted.

## Value

`to_http_date()` returns a properly formatted string, while
`from_http_date()` returns a POSIXct object

## Examples

``` r
time <- to_http_date(Sys.time())
time
#> [1] "Thu, 11 Dec 2025 06:53:17 GMT"
from_http_date(time)
#> [1] "2025-12-11 06:53:17 GMT"
```
