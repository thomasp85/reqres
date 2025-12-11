# Pre-supplied parsing generators

This set of functions can be used to construct parsing functions
adhering to the Request\$parse() requirements.

## Usage

``` r
parse_json(
  simplifyVector = TRUE,
  simplifyDataFrame = simplifyVector,
  simplifyMatrix = simplifyVector,
  flatten = FALSE
)

parse_plain(sep = "\n")

parse_xml(encoding = "", options = "NOBLANKS", base_url = "")

parse_html(
  encoding = "",
  options = c("RECOVER", "NOERROR", "NOBLANKS"),
  base_url = ""
)

parse_multiform()

parse_queryform(delim = NULL)

parse_table(...)
```

## Arguments

- simplifyVector:

  coerce JSON arrays containing only primitives into an atomic vector

- simplifyDataFrame:

  coerce JSON arrays containing only records (JSON objects) into a data
  frame

- simplifyMatrix:

  coerce JSON arrays containing vectors of equal mode and dimension into
  matrix or array

- flatten:

  automatically
  [`flatten()`](https://jeroen.r-universe.dev/jsonlite/reference/flatten.html)
  nested data frames into a single non-nested data frame

- sep:

  The line separator. Plain text will be split into multiple strings
  based on this.

- encoding:

  Specify a default encoding for the document. Unless otherwise
  specified XML documents are assumed to be in UTF-8 or UTF-16. If the
  document is not UTF-8/16, and lacks an explicit encoding directive,
  this allows you to supply a default.

- options:

  Set parsing options for the libxml2 parser. Zero or more of

  RECOVER

  :   recover on errors

  NOENT

  :   substitute entities

  DTDLOAD

  :   load the external subset

  DTDATTR

  :   default DTD attributes

  DTDVALID

  :   validate with the DTD

  NOERROR

  :   suppress error reports

  NOWARNING

  :   suppress warning reports

  PEDANTIC

  :   pedantic error reporting

  NOBLANKS

  :   remove blank nodes

  SAX1

  :   use the SAX1 interface internally

  XINCLUDE

  :   Implement XInclude substitution

  NONET

  :   Forbid network access

  NODICT

  :   Do not reuse the context dictionary

  NSCLEAN

  :   remove redundant namespaces declarations

  NOCDATA

  :   merge CDATA as text nodes

  NOXINCNODE

  :   do not generate XINCLUDE START/END nodes

  COMPACT

  :   compact small text nodes; no modification of the tree allowed
      afterwards (will possibly crash if you try to modify the tree)

  OLD10

  :   parse using XML-1.0 before update 5

  NOBASEFIX

  :   do not fixup XINCLUDE xml:base uris

  HUGE

  :   relax any hardcoded limit from the parser

  OLDSAX

  :   parse using SAX2 interface before 2.7.0

  IGNORE_ENC

  :   ignore internal document encoding hint

  BIG_LINES

  :   Store big lines numbers in text PSVI field

- base_url:

  When loading from a connection, raw vector or literal html/xml, this
  allows you to specify a base url for the document. Base urls are used
  to turn relative urls into absolute urls.

- delim:

  The delimiter to use for parsing arrays in non-exploded form. Either
  `NULL` (no delimiter) or one of `","`, `"|"`, or `" "`

- ...:

  parameters passed on to
  [`read.table()`](https://rdrr.io/r/utils/read.table.html)

## Value

A function accepting a raw vector and a named list of directives

## See also

[formatters](https://reqres.data-imaginist.com/reference/formatters.md)
for converting `Response` bodies into compatible types

[default_parsers](https://reqres.data-imaginist.com/reference/default_parsers.md)
for a list that maps the most common mime types to their respective
parsers

## Examples

``` r
fake_rook <- fiery::fake_request(
  'http://example.com/test',
  content = '[1, 2, 3, 4]',
  headers = list(
    Content_Type = 'application/json'
  )
)

req <- Request$new(fake_rook)
req$parse(json = parse_json())
#> [1] TRUE
req$body
#> [1] 1 2 3 4

# Cleaning up connections
rm(fake_rook, req)
gc()
#>           used (Mb) gc trigger (Mb) max used (Mb)
#> Ncells 1181621 63.2    2190065  117  2190065  117
#> Vcells 2203335 16.9    8388608   64  8388559   64
```
