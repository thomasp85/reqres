# response are created correctly

    Code
      Response$new(req)
    Condition
      Error in `initialize()`:
      ! A response has already been created for this request. Access it using the response field

# files are added correctly

    Code
      res$file <- "not_a_real_file"
    Condition
      Error in `file_path_as_absolute()`:
      ! file 'not_a_real_file' does not exist

# print functino works

    Code
      res$print()
    Message
      -- An HTTP response ------------------------------------------------------------
      Status: 404 - Not Found
      Content type: text/plain
      > Responding to: http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen

# set_formatter works correctly

    Code
      no_match_res$set_formatter(json = format_json(), xml = format_xml(), autofail = TRUE)
    Condition
      Error in `no_match_res$set_formatter()`:
      ! Only application/json or application/xml content types supported.

# data store functions work correctly

    Code
      res$data_store <- list()
    Condition
      Error:
      ! It is not allowed to replace the data store

