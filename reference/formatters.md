# Pre-supplied formatting generators

This set of functions can be used to construct formatting functions
adhering to the Response\$format() requirements.

## Usage

``` r
format_json(
  dataframe = "rows",
  matrix = "rowmajor",
  Date = "ISO8601",
  POSIXt = "string",
  factor = "string",
  complex = "string",
  raw = "base64",
  null = "list",
  na = "null",
  auto_unbox = FALSE,
  digits = 4,
  pretty = FALSE,
  force = FALSE
)

format_plain(sep = "\n")

format_xml(root_name = "document", encoding = "UTF-8", options = "as_xml")

format_html(encoding = "UTF-8", options = "as_html")

format_table(...)
```

## Arguments

- dataframe:

  how to encode data.frame objects: must be one of 'rows', 'columns' or
  'values'

- matrix:

  how to encode matrices and higher dimensional arrays: must be one of
  'rowmajor' or 'columnmajor'.

- Date:

  how to encode Date objects: must be one of 'ISO8601' or 'epoch'

- POSIXt:

  how to encode POSIXt (datetime) objects: must be one of 'string',
  'ISO8601', 'epoch' or 'mongo'

- factor:

  how to encode factor objects: must be one of 'string' or 'integer'

- complex:

  how to encode complex numbers: must be one of 'string' or 'list'

- raw:

  how to encode raw objects: must be one of 'base64', 'hex' or 'mongo'

- null:

  how to encode NULL values within a list: must be one of 'null' or
  'list'

- na:

  how to print NA values: must be one of 'null' or 'string'. Defaults
  are class specific

- auto_unbox:

  automatically
  [`unbox()`](https://jeroen.r-universe.dev/jsonlite/reference/unbox.html)
  all atomic vectors of length 1. It is usually safer to avoid this and
  instead use the
  [`unbox()`](https://jeroen.r-universe.dev/jsonlite/reference/unbox.html)
  function to unbox individual elements. An exception is that objects of
  class `AsIs` (i.e. wrapped in
  [`I()`](https://rdrr.io/r/base/AsIs.html)) are not automatically
  unboxed. This is a way to mark single values as length-1 arrays.

- digits:

  max number of decimal digits to print for numeric values. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify significant
  digits. Use `NA` for max precision.

- pretty:

  adds indentation whitespace to JSON output. Can be TRUE/FALSE or a
  number specifying the number of spaces to indent (default is 2). Use a
  negative number for tabs instead of spaces.

- force:

  unclass/skip objects of classes with no defined JSON mapping

- sep:

  The line separator. Plain text will be split into multiple strings
  based on this.

- root_name:

  The name of the root element of the created xml

- encoding:

  The character encoding to use in the document. The default encoding is
  ‘UTF-8’. Available encodings are specified at
  <http://xmlsoft.org/html/libxml-encoding.html#xmlCharEncoding>.

- options:

  default: ‘format’. Zero or more of

  format

  :   Format output

  no_declaration

  :   Drop the XML declaration

  no_empty_tags

  :   Remove empty tags

  no_xhtml

  :   Disable XHTML1 rules

  require_xhtml

  :   Force XHTML rules

  as_xml

  :   Force XML output

  as_html

  :   Force HTML output

  format_whitespace

  :   Format with non-significant whitespace

- ...:

  parameters passed on to
  [`write.table()`](https://rdrr.io/r/utils/write.table.html)

## Value

A function accepting an R object

## See also

[parsers](https://reqres.data-imaginist.com/reference/parsers.md) for
converting `Request` bodies into R objects

[default_formatters](https://reqres.data-imaginist.com/reference/default_formatters.md)
for a list that maps the most common mime types to their respective
formatters

## Examples

``` r
fake_rook <- fiery::fake_request(
  'http://example.com/test',
  content = '',
  headers = list(
    Content_Type = 'text/plain',
    Accept = 'application/json, text/csv'
  )
)

req <- Request$new(fake_rook)
res <- req$respond()
res$body <- mtcars
res$format(json = format_json(), csv = format_table(sep=','))
#> [1] TRUE
res$body
#> [{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.62,"qsec":16.46,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4"},{"mpg":21,"cyl":6,"disp":160,"hp":110,"drat":3.9,"wt":2.875,"qsec":17.02,"vs":0,"am":1,"gear":4,"carb":4,"_row":"Mazda RX4 Wag"},{"mpg":22.8,"cyl":4,"disp":108,"hp":93,"drat":3.85,"wt":2.32,"qsec":18.61,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Datsun 710"},{"mpg":21.4,"cyl":6,"disp":258,"hp":110,"drat":3.08,"wt":3.215,"qsec":19.44,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Hornet 4 Drive"},{"mpg":18.7,"cyl":8,"disp":360,"hp":175,"drat":3.15,"wt":3.44,"qsec":17.02,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Hornet Sportabout"},{"mpg":18.1,"cyl":6,"disp":225,"hp":105,"drat":2.76,"wt":3.46,"qsec":20.22,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Valiant"},{"mpg":14.3,"cyl":8,"disp":360,"hp":245,"drat":3.21,"wt":3.57,"qsec":15.84,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Duster 360"},{"mpg":24.4,"cyl":4,"disp":146.7,"hp":62,"drat":3.69,"wt":3.19,"qsec":20,"vs":1,"am":0,"gear":4,"carb":2,"_row":"Merc 240D"},{"mpg":22.8,"cyl":4,"disp":140.8,"hp":95,"drat":3.92,"wt":3.15,"qsec":22.9,"vs":1,"am":0,"gear":4,"carb":2,"_row":"Merc 230"},{"mpg":19.2,"cyl":6,"disp":167.6,"hp":123,"drat":3.92,"wt":3.44,"qsec":18.3,"vs":1,"am":0,"gear":4,"carb":4,"_row":"Merc 280"},{"mpg":17.8,"cyl":6,"disp":167.6,"hp":123,"drat":3.92,"wt":3.44,"qsec":18.9,"vs":1,"am":0,"gear":4,"carb":4,"_row":"Merc 280C"},{"mpg":16.4,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":4.07,"qsec":17.4,"vs":0,"am":0,"gear":3,"carb":3,"_row":"Merc 450SE"},{"mpg":17.3,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":3.73,"qsec":17.6,"vs":0,"am":0,"gear":3,"carb":3,"_row":"Merc 450SL"},{"mpg":15.2,"cyl":8,"disp":275.8,"hp":180,"drat":3.07,"wt":3.78,"qsec":18,"vs":0,"am":0,"gear":3,"carb":3,"_row":"Merc 450SLC"},{"mpg":10.4,"cyl":8,"disp":472,"hp":205,"drat":2.93,"wt":5.25,"qsec":17.98,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Cadillac Fleetwood"},{"mpg":10.4,"cyl":8,"disp":460,"hp":215,"drat":3,"wt":5.424,"qsec":17.82,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Lincoln Continental"},{"mpg":14.7,"cyl":8,"disp":440,"hp":230,"drat":3.23,"wt":5.345,"qsec":17.42,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Chrysler Imperial"},{"mpg":32.4,"cyl":4,"disp":78.7,"hp":66,"drat":4.08,"wt":2.2,"qsec":19.47,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Fiat 128"},{"mpg":30.4,"cyl":4,"disp":75.7,"hp":52,"drat":4.93,"wt":1.615,"qsec":18.52,"vs":1,"am":1,"gear":4,"carb":2,"_row":"Honda Civic"},{"mpg":33.9,"cyl":4,"disp":71.1,"hp":65,"drat":4.22,"wt":1.835,"qsec":19.9,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Toyota Corolla"},{"mpg":21.5,"cyl":4,"disp":120.1,"hp":97,"drat":3.7,"wt":2.465,"qsec":20.01,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Toyota Corona"},{"mpg":15.5,"cyl":8,"disp":318,"hp":150,"drat":2.76,"wt":3.52,"qsec":16.87,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Dodge Challenger"},{"mpg":15.2,"cyl":8,"disp":304,"hp":150,"drat":3.15,"wt":3.435,"qsec":17.3,"vs":0,"am":0,"gear":3,"carb":2,"_row":"AMC Javelin"},{"mpg":13.3,"cyl":8,"disp":350,"hp":245,"drat":3.73,"wt":3.84,"qsec":15.41,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Camaro Z28"},{"mpg":19.2,"cyl":8,"disp":400,"hp":175,"drat":3.08,"wt":3.845,"qsec":17.05,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Pontiac Firebird"},{"mpg":27.3,"cyl":4,"disp":79,"hp":66,"drat":4.08,"wt":1.935,"qsec":18.9,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Fiat X1-9"},{"mpg":26,"cyl":4,"disp":120.3,"hp":91,"drat":4.43,"wt":2.14,"qsec":16.7,"vs":0,"am":1,"gear":5,"carb":2,"_row":"Porsche 914-2"},{"mpg":30.4,"cyl":4,"disp":95.1,"hp":113,"drat":3.77,"wt":1.513,"qsec":16.9,"vs":1,"am":1,"gear":5,"carb":2,"_row":"Lotus Europa"},{"mpg":15.8,"cyl":8,"disp":351,"hp":264,"drat":4.22,"wt":3.17,"qsec":14.5,"vs":0,"am":1,"gear":5,"carb":4,"_row":"Ford Pantera L"},{"mpg":19.7,"cyl":6,"disp":145,"hp":175,"drat":3.62,"wt":2.77,"qsec":15.5,"vs":0,"am":1,"gear":5,"carb":6,"_row":"Ferrari Dino"},{"mpg":15,"cyl":8,"disp":301,"hp":335,"drat":3.54,"wt":3.57,"qsec":14.6,"vs":0,"am":1,"gear":5,"carb":8,"_row":"Maserati Bora"},{"mpg":21.4,"cyl":4,"disp":121,"hp":109,"drat":4.11,"wt":2.78,"qsec":18.6,"vs":1,"am":1,"gear":4,"carb":2,"_row":"Volvo 142E"}] 

# Cleaning up connections
rm(fake_rook, req, res)
gc()
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1162605 62.1    2190065  117  2190065  117
#> Vcells 2169438 16.6    8388608   64  8388559   64
```
