# Create a mock request to use in testing

This function creates a new
[`Request`](https://reqres.data-imaginist.com/reference/Request.md) for
a specific resource defined by a URL.

## Usage

``` r
mock_request(
  url,
  method = "get",
  content = "",
  headers = list(),
  app_location = "",
  remote_address = "123.123.123.123"
)
```

## Arguments

- url:

  A complete url for the resource the request should ask for, including
  querystring if needed

- method:

  The request type (get, post, put, etc). Defaults to `"get"`

- content:

  The content of the request, either a raw vector or a string

- headers:

  A list of name-value pairs that defines the request headers

- app_location:

  A string giving the first part of the url path that should be stripped
  from the path

- remote_address:

  The IP address of the presumed sender

## Value

A [`Request`](https://reqres.data-imaginist.com/reference/Request.md)
object

## Examples

``` r
req <- mock_request(
    'http://www.my-fake-website.com/path/to/a/query/?key=value&key2=value2',
    content = 'Some important content'
)

# Get the main address of the URL
req$host
#> [1] "www.my-fake-website.com:80"

# Get the query string
req$query
#> $key
#> [1] "value"
#> 
#> $key2
#> [1] "value2"
#> 

# ... etc.

# Cleaning up connections
rm(req)
gc()
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1180060 63.1    2190065  117  2190065  117
#> Vcells 2199490 16.8    8388608   64  8388559   64
```
