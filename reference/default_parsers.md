# A list of default parser mappings

This list matches the most normal mime types with their respective
parsers using default arguments. For a no-frills request parsing this
can be supplied directly to `Request$parse()`. To add or modify to this
list simply supply the additional parsers as second, third, etc,
argument and they will overwrite or add depending on whether it
specifies a mime type already present.

## Usage

``` r
default_parsers
```

## See also

[parsers](https://reqres.data-imaginist.com/reference/parsers.md) for an
overview of the build in parsers in `reqres`

## Examples

``` r
if (FALSE) { # \dontrun{
req$parse(default_parsers, 'application/json' = parse_json(flatten = TRUE))
} # }
```
