# Check if object is request-like

This function checks an object without looking at the class. Due to a
slight overhead in R6 method dispatch there can be a small gain by
removing the class of the object and treating it as a bare environment.
However, in that case
[is.Request](https://reqres.data-imaginist.com/reference/Request.md)
will no longer work

## Usage

``` r
maybe_request(x)

unclass_request(x)
```

## Arguments

- x:

  An object to check

## Value

A boolean indicating with some certainty if the object is a request
