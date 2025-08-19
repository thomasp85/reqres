
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reqres <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/thomasp85/reqres/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/reqres/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/reqres)](https://CRAN.R-project.org/package=reqres)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/reqres/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/reqres)
<!-- badges: end -->

While the http protocol is rather basic in essence, it can be a pain to
work with. `reqres` is here to soothe the pain somewhat by providing two
powerful classes for handling all parts of request and response handling
during a http exchange. *This is not a web server*, instead it focuses
on making life easier for developers of web servers by extracting the
complexity of cookies, headers, content negotiation, and the likes into
neat little classes. `reqres` builds upon the
[`rook`](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md)
specifications and is thus well suited for
[`httpuv`-based](https://github.com/rstudio/httpuv) webservers.

## Features

`reqres` draws a lot of inspiration from
[express.js](https://expressjs.com) and the `Request` and `Response`
classes is aiming for feature parity with those from express. The
`Request` class provides automatic parsing of the query string along
with parsing of the body based on the `Content-Type` header (with
decompression if `Content-Encoding` is provided). Further, it provides
content negotiation based on the `Accept(-*)` headers. The `Response`
class allows you to set headers and cookies easily, assign arbitrary
data for later use, and automatically format the body based on content
negotiation with the `Request` object that it is responding to (again,
it will compress automatically if the `Accept-Encoding` header allows
it). If any part of the content negotiation fails the correct response
status code will be set, making the response ready to send.

`reqres` comes with a range of parsers and formatters making it work out
of the box with json, xml, html, csv, tab, multipart, and
www-form-urlencoded payloads. It is easy to either modify these or
provide your own parsers and formatters if needed - `reqres` will take
care of the content negotiation and simply call your custom
parser/formatter if chosen.

## Installation

reqrescan be installed from CRAN with `install.packages('reqres')` or
the development version can be installed from github:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/reqres')
```

## Demo

Below is a quick demo of some of the features in `reqres`. It uses the
`fake_request()` in `fiery` to mock a rook request so it can be used
without setting up a webserver:

``` r
library(reqres)

# We start by mocking our request
rook <- fiery::fake_request(
    url = 'http://www.example.com/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '{"name":["Thomas Lin Pedersen"],"age":[31],"homepage":["www.data-imaginist.com","www.github.com/thomasp85"]}',
    headers = list(
        Content_Type = 'application/json',
        Accept = 'application/json, application/xml; q=0.5, text/*; q=0.3',
        Accept_Encoding = 'gzip, br'
    )
)

# A Request object can now be created
req <- Request$new(rook)
req
#> ── An HTTP request ─────────────────────────────────────────────────────────────
#> Trusted: No
#> Method: get
#> URL: http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen

# ... along with a response
res <- req$respond()
res
#> ── An HTTP response ────────────────────────────────────────────────────────────
#> Status: 404 - Not Found
#> Content type: text/plain
#> → Responding to:
#> http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen
```

### Request

A lot of information is already available, such as the query and other
parts of the url, but the body is not filled in automatically.

``` r
req$host
#> [1] "www.example.com:80"
req$query
#> $id
#> [1] "2347"
#> 
#> $user
#> [1] "Thomas Lin Pedersen"
req$body
#> NULL
```

The body can easily be parsed though, as long as a parser exists for the
provided content type.

``` r
req$is('json')
#> [1] TRUE
#> attr(,"pick")
#> [1] 1
req$parse(json = parse_json())
#> [1] TRUE
req$body
#> $name
#> [1] "Thomas Lin Pedersen"
#> 
#> $age
#> [1] 31
#> 
#> $homepage
#> [1] "www.data-imaginist.com"   "www.github.com/thomasp85"
```

Instead of inspecting it manually you can simply provide a range of
parsers and let the object choose the correct one itself

``` r
req$set_body(NULL)
req$parse(
    txt = parse_plain(),
    html = parse_html(),
    json = parse_json()
)
#> [1] TRUE
req$body
#> $name
#> [1] "Thomas Lin Pedersen"
#> 
#> $age
#> [1] 31
#> 
#> $homepage
#> [1] "www.data-imaginist.com"   "www.github.com/thomasp85"
```

In the case that none of the provided parsers fits the content type, the
response will automatically throw an exception that can be converted
into the right response

``` r
req$set_body(NULL)
req$parse(txt = parse_plain())
#> Error in `req$parse()`:
#> ! Unsupported Media Type
res
#> ── An HTTP response ────────────────────────────────────────────────────────────
#> Status: 404 - Not Found
#> Content type: text/plain
#> → Responding to:
#> http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen
```

To facilitate all this `reqres` comes with a mapping of standard mime
types to the provided parsers. This can simply be supplied to the parse
method

``` r
req$set_body(NULL)
req$parse(default_parsers)
#> Warning: Request$parse(list(...)) was deprecated in reqres 0.3.
#> ℹ Please use Request$parse(!!!list(...)) instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> [1] TRUE
req$body
#> $name
#> [1] "Thomas Lin Pedersen"
#> 
#> $age
#> [1] 31
#> 
#> $homepage
#> [1] "www.data-imaginist.com"   "www.github.com/thomasp85"
```

### Response

While the request is mainly intended to be read from, the response
should be written to. The `Response` class contains a slew of methods to
easily set headers, cookies, etc.

``` r
res$set_header('Date', to_http_date(Sys.time()))
res$get_header('Date')
#> [1] "Tue, 19 Aug 2025 07:39:38 GMT"
res$set_cookie('user', req$query$id, max_age = 9000L)
res$has_cookie('user')
#> [1] TRUE
```

Furthermore, it contains its own data store where arbitrary information
can be stored so as to pass it between middleware etc. This data will
never be part of the actual response.

``` r
res$set_data('alphabet', letters)
res$get_data('alphabet')
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
```

Files can be attached and marked for download, setting the relevant
headers automatically

``` r
res$attach(system.file('NEWS.md', package = 'reqres'))
res$get_header('Content-Type')
#> [1] "text/markdown"
res$get_header('Content-Disposition')
#> [1] "attachment; filename=\"NEWS.md\""
```

Often we need to provide a payload in the form of a body. This can be
any type of R object until the response is handed off to the server,
where it should be either a string or a raw vector.

``` r
res$remove_header('Content-Disposition')
res$body <- head(mtcars)
res$body
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

Based on the `Accept` header in the request it can be formatted
correctly thus making it ready to send back to the client. As this
request contains an `Accept-Encoding` header it will be compressed as
well.

``` r
res$format(json = format_json())
#> [1] TRUE
res$body
#>   [1] 1f 8b 08 00 00 00 00 00 00 03 9d d2 41 4f 83 30 14 07 f0 af 42 de b9 69 da
#>  [26] 47 29 a5 e7 1d bc 78 d1 44 4d 8c 31 dd 20 48 b2 01 16 36 a2 c6 ef 6e d9 a0
#>  [51] 6c 63 4b d4 db 4b d3 f6 fd de bf 7d fe 82 4d 9d 83 46 4e 60 f5 b1 06 2d 09
#>  [76] a4 45 53 83 e6 92 11 78 eb 0b ee 8a d4 9a 16 74 48 13 02 9d 2b 90 4a 24 f0
#> [101] de 64 ab 7e 23 15 ee d4 ae 01 ed 36 9a 8d 5b 21 90 67 c6 82 16 ee 52 63 97
#> [126] fb e2 d5 56 1d 68 b8 35 9f a9 09 ee 9e 04 7c 93 ff f6 56 71 e4 9b c7 94 e1
#> [151] df 9b 07 8f 26 3f 02 b8 2b 07 82 f0 04 a6 0e 84 24 9c 04 2a 1a 09 e1 34 be
#> [176] a2 92 1f 04 fc 9a 80 7b c1 c2 b4 cd b6 0c 62 ce 8e e7 a7 e2 3c 01 8c d4 85
#> [201] 04 7a 53 b7 af 90 4f 11 24 54 88 13 00 1b 01 e1 1c 70 53 d9 32 6b 03 11 2c
#> [226] 6c b1 cb 26 84 1b 23 1e 10 6a 44 84 fe 19 fa c0 47 04 8f 46 44 df f6 da 33
#> [251] cc 0d 78 6e b8 af 2b db 9a 65 b5 6d 4f 18 b3 df 80 18 0d 0c e6 19 48 63 e9
#> [276] 19 72 64 20 a3 88 bf 8e e2 c1 ac 0b 53 ba e6 2f 3f 40 6a d7 44 06 03 00 00
res$get_header('Content-Type')
#> [1] "application/json"
res$get_header('Content-Encoding')
#> [1] "gzip"
```

The content negotiation understands wildcards as well

``` r
res$body <- head(mtcars)
req$get_header('Accept')
#> [1] "application/json"       "application/xml; q=0.5" "text/*; q=0.3"
res$format(csv = format_table(sep = ','), compress = FALSE)
#> [1] TRUE
res$body
#> [1] "\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n\"Mazda RX4\",21,6,160,110,3.9,2.62,16.46,0,1,4,4\n\"Mazda RX4 Wag\",21,6,160,110,3.9,2.875,17.02,0,1,4,4\n\"Datsun 710\",22.8,4,108,93,3.85,2.32,18.61,1,1,4,1\n\"Hornet 4 Drive\",21.4,6,258,110,3.08,3.215,19.44,1,0,3,1\n\"Hornet Sportabout\",18.7,8,360,175,3.15,3.44,17.02,0,0,3,2\n\"Valiant\",18.1,6,225,105,2.76,3.46,20.22,1,0,3,1"
res$get_header('Content-Type')
#> [1] "text/csv"
```

A default formatter mapping exists in parallel to `default_parsers` for
the `Request$format()` method.

``` r
res$body <- head(mtcars)
res$format(default_formatters, compress = FALSE)
#> Warning: Response$format(list(...)) was deprecated in reqres 0.3.
#> ℹ Please use Response$format(!!!list(...)) instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> [1] TRUE
res$body
#> [{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.62,"qsec":16.46,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4"},{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.875,"qsec":17.02,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4 Wag"},{"mpg":22.8,"cyl":4,"disp":108,"hp":93,"drat":3.85,"wt":2.32,"qsec":18.61,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Datsun 710"},{"mpg":21.4,"cyl":6,"disp":258,"hp":110,"drat":3.08,"wt":3.215,"qsec":19.44,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Hornet 4 Drive"},{"mpg":18.7,"cyl":8,"disp":360,"hp":175,"drat":3.15,"wt":3.44,"qsec":17.02,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Hornet Sportabout"},{"mpg":18.1,"cyl":6,"disp":225,"hp":105,"drat":2.76,"wt":3.46,"qsec":20.22,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Valiant"}]
```

It is easy to define your own formatters and add them along the defaults

``` r
res$body <- head(mtcars)
res$format('text/yaml' = yaml::as.yaml, compress = FALSE)
#> [1] TRUE
res$body
#> [1] "mpg:\n- 21.0\n- 21.0\n- 22.8\n- 21.4\n- 18.7\n- 18.1\ncyl:\n- 6.0\n- 6.0\n- 4.0\n- 6.0\n- 8.0\n- 6.0\ndisp:\n- 160.0\n- 160.0\n- 108.0\n- 258.0\n- 360.0\n- 225.0\nhp:\n- 110.0\n- 110.0\n- 93.0\n- 110.0\n- 175.0\n- 105.0\ndrat:\n- 3.9\n- 3.9\n- 3.85\n- 3.08\n- 3.15\n- 2.76\nwt:\n- 2.62\n- 2.875\n- 2.32\n- 3.215\n- 3.44\n- 3.46\nqsec:\n- 16.46\n- 17.02\n- 18.61\n- 19.44\n- 17.02\n- 20.22\nvs:\n- 0.0\n- 0.0\n- 1.0\n- 1.0\n- 0.0\n- 1.0\nam:\n- 1.0\n- 1.0\n- 1.0\n- 0.0\n- 0.0\n- 0.0\ngear:\n- 4.0\n- 4.0\n- 4.0\n- 3.0\n- 3.0\n- 3.0\ncarb:\n- 4.0\n- 4.0\n- 1.0\n- 1.0\n- 2.0\n- 1.0\n"
```

## Code of Conduct

Please note that the ‘reqres’ project is released with a [Contributor
Code of
Conduct](https://reqres.data-imaginist.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
