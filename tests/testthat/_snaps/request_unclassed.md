# trust works

    Code
      req$trust <- "test"
    Condition
      Error:
      ! `value` must be `TRUE` or `FALSE`, not the string "test".

# response can be generated

    Code
      req$response <- res2
    Condition
      Error:
      ! Response can only be assigned once

# body can be parsed

    Code
      req$parse(xml = parse_xml())
    Condition
      Error in `req$parse()`:
      ! Unsupported Media Type

# as.Request and is.Request work correctly

    Code
      as.Request(non_rook_env)
    Condition
      Error in `as.Request()`:
      ! `x` must be a Rook object

