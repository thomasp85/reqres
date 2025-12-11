# A list of default formatter mappings

This list matches the most normal mime types with their respective
formatters using default arguments. For a no-frills request parsing this
can be supplied directly to `Response$format()`. To add or modify to
this list simply supply the additional parsers as second, third, etc,
argument and they will overwrite or add depending on whether it
specifies a mime type already present.

## Usage

``` r
default_formatters
```

## See also

[formatters](https://reqres.data-imaginist.com/reference/formatters.md)
for an overview of the build in formatters in `reqres`

## Examples

``` r
if (FALSE) { # \dontrun{
res$format(default_formatters, 'text/plain' = format_plain(sep = ' '))
} # }
```
