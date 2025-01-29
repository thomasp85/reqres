# response are created correctly

    Code
      Response$new(req)
    Condition
      Error in `initialize()`:
      ! A response has already been created for this request. Access it using the response field

# headers can be get, set, appended, and removed

    Code
      res$remove_header("Date")
    Condition
      Warning:
      No header named "Date"

# data can be get, set, and removed

    Code
      res$remove_data("test")
    Condition
      Warning:
      No data named "test"

# cookies can be get, set, and removed

    Code
      res$remove_cookie("test")
    Condition
      Warning:
      No cookie named "test"

# files are added correctly

    Code
      res$file <- "not_a_real_file"
    Condition
      Error in `file_path_as_absolute()`:
      ! file 'not_a_real_file' does not exist

# print functino works

    Code
      res$print()
    Output
      A HTTP response
      ===============
              Status: 404 - Not Found
        Content type: text/plain
      
      In response to: http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen

