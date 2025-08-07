test_that("from_http_date works correctly", {
  # Test conversion from http date format to POSIXct
  http_date <- "Wed, 21 Oct 2015 07:28:00 GMT"
  expected_time <- as.POSIXct("2015-10-21 07:28:00", tz = "GMT")

  result <- from_http_date(http_date)

  expect_equal(result, expected_time)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2015-10-21 07:28:00")
})

test_that("query_parser handles edge cases", {
  # Test empty or NULL query
  expect_equal(query_parser(), list())
  expect_equal(query_parser(""), list())

  # Test leading question mark
  expect_equal(
    query_parser("?name=John&age=30"),
    query_parser("name=John&age=30")
  )

  # Test space delimiter
  expect_equal(
    query_parser("items=1%202%203", delim = " "),
    list(items = c("1", "2", "3"))
  )

  # Test URL encoded values
  expect_equal(
    query_parser("name=John%20Doe&location=New%20York"),
    list(name = "John Doe", location = "New York")
  )

  # Test plus sign as space
  expect_equal(
    query_parser("name=John+Doe&location=New+York"),
    list(name = "John Doe", location = "New York")
  )

  # Test array values without delimiter
  expect_equal(
    query_parser("tag=red&tag=green&tag=blue"),
    list(tag = c("red", "green", "blue"))
  )

  # Test array values with delimiter
  expect_equal(
    query_parser("tag=red|green|blue", delim = "|"),
    list(tag = c("red", "green", "blue"))
  )
})

test_that("mime_type_from_file returns correct mime types", {
  # Test common file extensions
  expect_equal(mime_type_from_file("test.html")$name, "text/html")
  expect_equal(mime_type_from_file("image.jpg")$name, "image/jpeg")
  expect_equal(mime_type_from_file("data.json")$name, "application/json")
  expect_equal(mime_type_from_file("script.js")$name, "application/javascript")
  expect_equal(mime_type_from_file("style.css")$name, "text/css")

  # Test case insensitivity
  expect_equal(mime_type_from_file("IMAGE.JPG")$name, mime_type_from_file("image.jpg")$name)

  # Test file with multiple extensions
  expect_equal(mime_type_from_file("archive.tar.gz")$name, "application/gzip")
})

test_that("mime_type_info returns correct information", {
  # Test getting info for known mime types
  json_info <- mime_type_info("application/json")
  expect_equal(json_info$name, "application/json")
  expect_true("json" %in% unlist(json_info$extensions))

  html_info <- mime_type_info("text/html")
  expect_equal(html_info$name, "text/html")
  expect_true("html" %in% unlist(html_info$extensions))
})

test_that("split_headers categorizes headers correctly", {
  headers <- list(
    accept = "text/html",
    content_type = "application/json",
    host = "example.com",
    location = "https://example.com/new",
    x_custom = "custom value"
  )

  result <- split_headers(headers)

  expect_true("accept" %in% names(result$request))
  expect_true("host" %in% names(result$request))
  expect_true("location" %in% names(result$response))
  expect_true("content_type" %in% names(result$entity))
  expect_true("x_custom" %in% names(result$entity))
})

test_that("cat_headers formats and prints headers correctly", {
  headers <- list(
    content_type = "application/json",
    user_agent = "Test Agent",
    x_custom_header = "Custom Value"
  )

  # Capture the output of cat_headers
  output <- capture.output(cat_headers(headers))

  expect_match(output[1], "^Content-Type: application/json$", all = FALSE)
  expect_match(output[2], "^User-Agent: Test Agent$", all = FALSE)
  expect_match(output[3], "^X-Custom-Header: Custom Value$", all = FALSE)

  # Test empty headers list
  empty_output <- capture.output(cat_headers(list()))
  expect_equal(length(empty_output), 0)
})

test_that("print.session_cookie_settings works correctly", {
  # Create a session cookie settings object
  cookie_settings <- session_cookie(
    name = "test_session",
    path = "/app",
    secure = TRUE
  )

  # Capture the print output
  output <- capture.output(print(cookie_settings), type = "message")

  expect_match(output[1], "Settings for a session cookie named test_session", fixed = TRUE)
  expect_match(output[2], "Attributes:", fixed = TRUE)

  # Test with additional options
  cookie_settings2 <- session_cookie(
    name = "another_session",
    path = "/api",
    secure = TRUE,
    same_site = "Strict"
  )

  output2 <- capture.output(print(cookie_settings2), type = "message")
  expect_match(output2[1], "Settings for a session cookie named another_session", fixed = TRUE)
})
