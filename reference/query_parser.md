# Parse a query string

This function facilitates the parsing of querystrings, either from the
URL or a POST or PUT body with `Content-Type` set to
`application/x-www-form-urlencoded`.

## Usage

``` r
query_parser(query = NULL, delim = NULL)
```

## Arguments

- query:

  The query as a single string

- delim:

  Optional delimiter of array values. If omitted it is expected that
  arrays are provided in exploded form (e.g. `arg1=3&arg1=7`)

## Value

A named list giving the keys and values of the query. Values fron the
same key are combined if given multiple times

## Examples

``` r
# Using delimiter to provide array
query_parser("?name=Thomas+Lin+Pedersen&numbers=1%202%203", delim = " ")
#> $name
#> [1] "Thomas Lin Pedersen"
#> 
#> $numbers
#> [1] "1" "2" "3"
#> 

# No delimiter (exploded form)
query_parser("?name=Thomas%20Lin%20Pedersen&numbers=1&numbers=2&numbers=3")
#> $name
#> [1] "Thomas Lin Pedersen"
#> 
#> $numbers
#> [1] "1" "2" "3"
#> 
```
