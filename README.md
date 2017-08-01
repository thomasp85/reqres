<!-- README.md is generated from README.Rmd. Please edit that file -->
reqres
======

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/reqres.svg?branch=master)](https://travis-ci.org/thomasp85/reqres) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/reqres?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/reqres) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/reqres)](https://cran.r-project.org/package=reqres) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/reqres)](https://cran.r-project.org/package=reqres) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/reqres/master.svg)](https://codecov.io/github/thomasp85/reqres?branch=master)

While the http protocol is rather basic in essence, it can be a pain to work with. `reqres` is here to soothe the pain somewhat by providing two powerful classes for handling all parts of request and response handling during a http exchange. *This is not a web server*, instead it focuses on making life easier for developers of web servers by extracting the complexity of cookies, headers, content negotiation, and the likes into neat little classes. `reqres` builds upon the [`rook`](https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md) specifications and is thus well suited for [`httpuv`-based](https://github.com/rstudio/httpuv) webservers.

Features
--------

`reqres` draws a lot of inspiration from [express.js](https://expressjs.com) and the `Request` and `Response` classes is aiming for feature parity with those from express. The `Request` class provides automatic parsing of the query string along with parsing of the body based on the `Content-Type` header (with decompression if `Content-Encoding` is provided). Further, it provides content negotiation based on the `Accept(-*)` headers. The `Response` class allows you to set headers and cookies easily, assign arbitrary data for later use, and automatically format the body based on content negotiation with the `Request` object that it is responding to (again, it will compress automatically if the `Accept-Encoding` header allows it). If any part of the content negotiation fails the correct response status code will be set, making the response ready to send.

`reqres` comes with a range of parsers and formatters making it work out of the box with json, xml, html, csv, tab, multipart, and www-form-urlencoded payloads. It is easy to either modify these or provide your own parsers and formatters if needed - `reqres` will take care of the content negotiation and simply call your custom parser/formatter if chosen.

Demo
----

Below is a quick demo of some of the features in `reqres`. It uses the `fake_request()` in `fiery` to mock a rook request so it can be used without setting up a webserver:

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
#> A HTTP request
#> ==============
#> Trusted: No
#>  Method: get
#>     URL: http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen

# ... along with a response
res <- req$respond()
res
#> A HTTP response
#> ===============
#>         Status: 404 - Not Found
#>   Content type: text/plain
#> 
#> In response to: http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen

# A lot of information is already available, e.g.
req$host
#> [1] "www.example.com:80"
req$query
#> $id
#> [1] 2347
#> 
#> $user
#> [1] "Thomas Lin Pedersen"

# But the body is not filled in automatically
req$body
#> NULL

# This needs to be parsed. The Content-Type header gives the type
req$is('json')
#> [1] TRUE
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

# You don't need to do this manually though. Just supply multiple
# and the correct will be chosen
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

# If no parser fits the bill the correct error code will be set
# on the response
req$set_body(NULL)
req$parse(txt = parse_plain())
#> [1] FALSE
res
#> A HTTP response
#> ===============
#>         Status: 400 - Bad Request
#>   Content type: text/plain
#> 
#> In response to: http://www.example.com:80/summary?id=2347&user=Thomas+Lin+Pedersen

# To make it easy, reqres comes with a mapping of common mime types
req$set_body(NULL)
req$parse(default_parsers)
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

# The response allow you to easily set headers, cookies, etc
res$set_header('Date', to_http_date(Sys.time()))
res$get_header('Date')
#> [1] "Tue, 01 Aug 2017 11:59:31 GMT"
res$set_cookie('user', req$query$id, max_age = 9000L)
res$get_header('Set-Cookie')
#> NULL

# It also has a data store where arbitrary data can be passed along
# between different middleware. This data will never become part of
# the actual response
res$set_data('alphabet', letters)
res$get_data('alphabet')
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
#> [18] "r" "s" "t" "u" "v" "w" "x" "y" "z"

# Files can be attached for download and the relevant headers will be filled
# out for you
res$attach(system.file('NEWS.md', package = 'reqres'))
res$get_header('Content-Type')
#> [1] "text/markdown"
res$get_header('Content-Disposition')
#> [1] "attachment; filename=NEWS.md"

# Lastly we can manipulate the body
res$remove_header('Content-Disposition')
#> Warning in rm(name, envir = private$HEADERS): object 'name' not found
res$body <- head(mtcars)
res$body
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# Based on the Accept header in the request it can be formatted correctly
# As the request contains an Accept-Encoding header it will be compressed 
# as well
res$format(json = format_json())
#> [1] TRUE
res$body
#>   [1] 1f 8b 08 00 00 00 00 00 00 03 9d d2 41 4f 83 30 14 07 f0 af 42 de b9
#>  [24] 69 da 47 29 a5 e7 1d bc 78 d1 44 4d 8c 31 dd 20 48 b2 01 16 36 a2 c6
#>  [47] ef 6e d9 a0 6c 63 4b d4 db 4b d3 f6 fd de bf 7d fe 82 4d 9d 83 46 4e
#>  [70] 60 f5 b1 06 2d 09 a4 45 53 83 e6 92 11 78 eb 0b ee 8a d4 9a 16 74 48
#>  [93] 13 02 9d 2b 90 4a 24 f0 de 64 ab 7e 23 15 ee d4 ae 01 ed 36 9a 8d 5b
#> [116] 21 90 67 c6 82 16 ee 52 63 97 fb e2 d5 56 1d 68 b8 35 9f a9 09 ee 9e
#> [139] 04 7c 93 ff f6 56 71 e4 9b c7 94 e1 df 9b 07 8f 26 3f 02 b8 2b 07 82
#> [162] f0 04 a6 0e 84 24 9c 04 2a 1a 09 e1 34 be a2 92 1f 04 fc 9a 80 7b c1
#> [185] c2 b4 cd b6 0c 62 ce 8e e7 a7 e2 3c 01 8c d4 85 04 7a 53 b7 af 90 4f
#> [208] 11 24 54 88 13 00 1b 01 e1 1c 70 53 d9 32 6b 03 11 2c 6c b1 cb 26 84
#> [231] 1b 23 1e 10 6a 44 84 fe 19 fa c0 47 04 8f 46 44 df f6 da 33 cc 0d 78
#> [254] 6e b8 af 2b db 9a 65 b5 6d 4f 18 b3 df 80 18 0d 0c e6 19 48 63 e9 19
#> [277] 72 64 20 a3 88 bf 8e e2 c1 ac 0b 53 ba e6 2f 3f 40 6a d7 44 06 03 00
#> [300] 00
res$get_header('Content-Type')
#> [1] "application/json"
res$get_header('Content-Encoding')
#> [1] "gzip"

# The content negotiation understands wildcards etc
res$body <- head(mtcars)
req$get_header('Accept')
#> [1] "application/json"       "application/xml; q=0.5"
#> [3] "text/*; q=0.3"
res$format(csv = format_table(sep = ','), compress = FALSE)
#> [1] TRUE
res$body
#> [1] "\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n\"Mazda RX4\",21,6,160,110,3.9,2.62,16.46,0,1,4,4\n\"Mazda RX4 Wag\",21,6,160,110,3.9,2.875,17.02,0,1,4,4\n\"Datsun 710\",22.8,4,108,93,3.85,2.32,18.61,1,1,4,1\n\"Hornet 4 Drive\",21.4,6,258,110,3.08,3.215,19.44,1,0,3,1\n\"Hornet Sportabout\",18.7,8,360,175,3.15,3.44,17.02,0,0,3,2\n\"Valiant\",18.1,6,225,105,2.76,3.46,20.22,1,0,3,1"
res$get_header('Content-Type')
#> [1] "text/csv"

# As with request parsing, a default mapping exists for your use.
res$body <- head(mtcars)
res$format(default_formatters, compress = FALSE)
#> [1] TRUE
res$body
#> [{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.62,"qsec":16.46,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4"},{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.875,"qsec":17.02,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4 Wag"},{"mpg":22.8,"cyl":4,"disp":108,"hp":93,"drat":3.85,"wt":2.32,"qsec":18.61,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Datsun 710"},{"mpg":21.4,"cyl":6,"disp":258,"hp":110,"drat":3.08,"wt":3.215,"qsec":19.44,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Hornet 4 Drive"},{"mpg":18.7,"cyl":8,"disp":360,"hp":175,"drat":3.15,"wt":3.44,"qsec":17.02,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Hornet Sportabout"},{"mpg":18.1,"cyl":6,"disp":225,"hp":105,"drat":2.76,"wt":3.46,"qsec":20.22,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Valiant"}]
```
