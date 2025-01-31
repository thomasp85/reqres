# request gets created correctly

    Code
      print(req)
    Message
      -- An HTTP request -------------------------------------------------------------
      Trusted: No
      Method: get
      URL: http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen

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

