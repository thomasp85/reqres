# Get the mime type associated with a file based on its file extension

While file extensions are not universally guaranteed to be tied to the
content of a file, they are often indicative of the content to the
degree that they can be used if the content type is missing.
`mime_type_from_file` gives access to the huge database of mime types
and their file extensions that reqres contains. `mime_type_info()`
provides the same information but rather than basing the search on a
file, you provide the known mime type directly

## Usage

``` r
mime_type_from_file(filename)

mime_type_info(type)
```

## Arguments

- filename:

  The name of the file to query

- type:

  The mime type to get additional information on

## Value

A data.frame with a row for each match and the columns:

- *name* The mime type

- *extensions* The extensions commonly associated with the mime type

- *charset* The character set used for the type, if any

- *compressible* Is the type known to be compressible

- *source* The source of the mime type information
