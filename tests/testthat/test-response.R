headers <- list(
  Content_Type = 'application/json',
  Accept = 'application/json, application/xml; q=0.5, text/*; q=0.3',
  Accept_Encoding = 'gzip, br',
  Cookie = 'id=Thomas; key=123',
  X_Forwarded_For = '500.0.0.0, 400.0.0.0',
  X_Forwarded_Host = 'www.example.com:80',
  X_Forwarded_Proto = 'https'
)
body <- '{"name":["Thomas Lin Pedersen"],"age":[31],"homepage":["www.data-imaginist.com","www.github.com/thomasp85"]}'
rook <- fiery::fake_request(
  url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
  content = body,
  headers = headers,
  REMOTE_ADDR = '230.45.12.45'
)

test_that('response are created correctly', {
  req <- Request$new(rook)
  res <- Response$new(req)
  expect_identical(req$response, res)
  expect_identical(res$request, req)
  expect_snapshot(Response$new(req), error = TRUE)

  expect_equal(res$status, 404L)
  expect_identical(res$body, '')
  expect_identical(res$type, 'text/plain')
})

test_that('headers can be get, set, appended, and removed', {
  req <- Request$new(rook)
  res <- Response$new(req)
  expect_true(res$has_header('Content-Type'))
  expect_false(res$has_header('Date'))
  expect_null(res$get_header('Date'))

  time <- Sys.time()
  res$set_header('Date', to_http_date(time))
  expect_true(res$has_header('Date'))
  expect_equal(res$get_header('Date'), to_http_date(time))
  res$remove_header('Date')
  expect_false(res$has_header('Date'))
  expect_null(res$get_header('Date'))

  res$append_header('Content-Encoding', 'gzip')
  expect_equal(res$get_header('Content-Encoding'), 'gzip')
  res$append_header('Content-Encoding', 'br')

  expect_equal(res$get_header('Content-Encoding'), c('gzip', 'br'))
})

test_that('data can be get, set, and removed', {
  req <- Request$new(rook)
  res <- Response$new(req)

  expect_false(res$has_data('test'))
  expect_null(res$get_data('test'))
  res$set_data('test', letters)
  expect_true(res$has_data('test'))
  expect_equal(res$get_data('test'), letters)
  res$remove_data('test')
  expect_false(res$has_data('test'))
  expect_null(res$get_data('test'))
})

test_that('cookies can be get, set, and removed', {
  req <- Request$new(rook)
  res <- Response$new(req)

  expect_false(res$has_cookie('test'))

  exp <- Sys.Date() + 1000
  res$set_cookie(
    'test',
    'this is a test',
    TRUE,
    expires = exp,
    http_only = TRUE,
    max_age = 1000,
    path = '/test',
    secure = TRUE,
    same_site = 'Lax'
  )
  expect_true(res$has_cookie('test'))
  expect_equal(
    res$as_list()$headers[['set-cookie']],
    paste0(
      'test=this%20is%20a%20test; Expires=',
      to_http_date(exp),
      '; HttpOnly; Max-Age=1000; Path=/test; Secure; SameSite=Lax'
    )
  )
  res$remove_cookie('test')
  expect_false(res$has_cookie('test'))
})

test_that('special header method works', {
  req <- Request$new(rook)
  res <- Response$new(req)

  time <- Sys.time()
  res$timestamp()
  res$has_header('Date')
  expect_equal(res$get_header('Date'), to_http_date(time))

  res$set_links(alternate = '/feed')
  res$has_header('Link')
  expect_equal(res$get_header('Link'), "</feed>; rel=\"alternate\"")
})

test_that('files are added correctly', {
  req <- Request$new(rook)
  res <- Response$new(req)
  file <- system.file('DESCRIPTION', package = 'reqres')

  expect_snapshot(res$file <- "not_a_real_file", error = TRUE)
  res$file <- file
  expect_equal(res$body, c(file = file))
  expect_equal(res$type, 'text/plain')
  expect_equal(res$get_header('Last-Modified'), to_http_date(file.mtime(file)))
  res$attach(file)
  expect_equal(
    res$get_header('Content-Disposition'),
    "attachment; filename=\"DESCRIPTION\""
  )

  expect_equal(res$as_list()$body, c(file = file))
})

test_that('status text are added', {
  req <- Request$new(rook)
  res <- Response$new(req)

  res$status_with_text(416L)
  expect_equal(res$body, 'Range Not Satisfiable')
})

test_that('print functino works', {
  req <- Request$new(rook)
  res <- Response$new(req)

  expect_snapshot(res$print())
})

test_that('body formatting works', {
  req <- Request$new(rook)
  res <- Response$new(req)
  body <- list(lower = letters, upper = LETTERS)
  res$body <- body
  expect_false(res$format('zip' = function(x) x, autofail = FALSE))
  expect_true(res$format(!!!default_formatters, compress = FALSE))
  expect_equal(res$body, jsonlite::toJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'gzip'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(!!!default_formatters))
  expect_equal(res$body, gzip(charToRaw(jsonlite::toJSON(body))))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'br'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(!!!default_formatters))
  expect_equal(
    res$body,
    brotli::brotli_compress(charToRaw(jsonlite::toJSON(body)))
  )

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'deflate'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(!!!default_formatters))
  expect_equal(res$body, memCompress(charToRaw(jsonlite::toJSON(body))))
})

test_that("attach works correctly", {
  # Create a request/response pair
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Use attach with a temporary file
  temp_file <- tempfile(fileext = ".txt")
  writeLines("Test content", temp_file)
  on.exit(unlink(temp_file))

  res$attach(temp_file, filename = "test-file.txt")

  # Check content disposition and type
  expect_equal(
    res$get_header("Content-Disposition"),
    'attachment; filename="test-file.txt"'
  )
  expect_equal(res$type, "text/plain")
  expect_equal(res$file, file_path_as_absolute(temp_file))

  # Check with custom type
  res$attach(
    temp_file,
    filename = "test.dat",
    type = "application/octet-stream"
  )
  expect_equal(res$type, "application/octet-stream")
})

test_that("as_download works correctly", {
  # Create a request/response pair
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Test without filename
  res$as_download()
  expect_equal(res$get_header("Content-Disposition"), "attachment")

  # Test with filename
  res$as_download(filename = "example.txt")
  expect_equal(
    res$get_header("Content-Disposition"),
    'attachment; filename="example.txt"'
  )
})

test_that("status_with_text works correctly", {
  # Create a request/response pair
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Test with common status code
  res$status_with_text(200L)
  expect_equal(res$status, 200L)
  expect_equal(res$body, "OK")
  expect_equal(res$type, "text/plain")

  # Test with clearing headers
  res$set_header("X-Custom", "Value")
  res$status_with_text(404L, clear_headers = TRUE)
  expect_equal(res$status, 404L)
  expect_equal(res$body, "Not Found")
  expect_null(res$get_header("X-Custom"))

  # Test with unknown status code
  res$status_with_text(599L)
  expect_equal(res$status, 599L)
  expect_equal(res$body, "599") # Uses code as text for unknown status
})

test_that("problem creates HTTP problem response", {
  # Create a request/response pair
  rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'application/json')
  )
  req <- Request$new(rook)
  res <- req$respond()

  # Create a problem response
  res$problem(
    code = 400L,
    detail = "Invalid input parameter",
    title = "Bad Request",
    type = "https://example.com/errors/bad-request",
    instance = "error-12345"
  )

  # Check response properties
  expect_equal(res$status, 400L)
  expect_equal(res$type, "application/problem+json")

  # Check body structure
  body <- fromJSON(res$body)
  expect_type(body, "list")
  expect_equal(body$status, 400L)
  expect_equal(body$detail, "Invalid input parameter")
  expect_equal(body$title, "Bad Request")
  expect_equal(body$type, "https://example.com/errors/bad-request")
  expect_equal(body$instance, "error-12345")

  # Test with minimal parameters (using defaults)
  res$reset()
  res$problem(404L, "Resource not found")
  expect_equal(res$status, 404L)
  body <- fromJSON(res$body)
  expect_equal(body$detail, "Resource not found")
  expect_equal(body$title, "Not Found")
  expect_equal(
    body$type,
    "https://datatracker.ietf.org/doc/html/rfc9110#section-15.5.5"
  )
})

test_that("set_cookie and cookie management works correctly", {
  # Create a request/response pair
  rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Cookies = "to_clear=value")
  )
  req <- Request$new(rook)
  res <- req$respond()

  # Set a basic cookie
  res$set_cookie("simple", "value")
  expect_true(res$has_cookie("simple"))

  # Set a cookie with options
  res$set_cookie(
    name = "complex",
    value = "test",
    path = "/app",
    secure = TRUE,
    http_only = TRUE,
    max_age = 3600
  )
  expect_true(res$has_cookie("complex"))

  # Remove a cookie
  res$remove_cookie("simple")
  expect_false(res$has_cookie("simple"))

  # Clear a cookie (asks client to delete it)
  res$clear_cookie("to_clear")
  expect_true(res$has_cookie("to_clear"))

  # Test automatic secure flag for secure-prefixed cookies
  res$set_cookie("__Secure-auto", "value")
  cookies_list <- as.list(res)$headers
  secure_cookie_header <- cookies_list[
    names(cookies_list) == "set-cookie" &
      grepl("__Secure-auto", cookies_list)
  ]
  expect_match(secure_cookie_header[[1]], "; Secure")
})

test_that("set_links works correctly", {
  # Create a request/response pair
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Set links
  res$set_links(
    `next` = "https://example.com/page/2",
    prev = "https://example.com/page/0"
  )

  # Check link header
  link_header <- res$get_header("Link")
  expect_match(link_header, "<https://example.com/page/2>; rel=\"next\"")
  expect_match(link_header, "<https://example.com/page/0>; rel=\"prev\"")
})

test_that("format works correctly with content negotiation", {
  # Create request with specific Accept header
  json_rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'application/json')
  )
  json_req <- Request$new(json_rook)
  json_res <- json_req$respond()

  # Test data
  test_data <- list(name = "John", age = 30)
  json_res$body <- test_data

  # Format using content negotiation
  result <- json_res$format(
    json = format_json(),
    xml = format_xml(),
    html = format_html()
  )

  expect_true(result)
  expect_equal(json_res$type, "application/json")
  expect_equal(unclass(json_res$body), '{"name":["John"],"age":[30]}')

  # Create request preferring XML
  xml_rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'application/xml, application/json;q=0.8')
  )
  xml_req <- Request$new(xml_rook)
  xml_res <- xml_req$respond()

  # Format with same options
  xml_res$body <- test_data
  xml_res$format(
    json = format_json(),
    xml = format_xml(),
    html = format_html()
  )

  expect_equal(xml_res$type, "application/xml")
  expect_match(xml_res$body, "<name>John</name>")

  # Test with default formatter when no match
  no_match_rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'text/plain')
  )
  no_match_req <- Request$new(no_match_rook)
  no_match_res <- no_match_req$respond()

  no_match_res$body <- test_data
  no_match_res$format(
    json = format_json(),
    xml = format_xml(),
    default = "json"
  )

  expect_equal(no_match_res$type, "application/json")
})

test_that("set_formatter works correctly", {
  # Create request with specific Accept header
  rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'application/json')
  )
  req <- Request$new(rook)
  res <- req$respond()

  # Set formatter
  result <- res$set_formatter(
    json = format_json(),
    xml = format_xml()
  )

  expect_true(result)
  expect_equal(res$type, "application/json")
  expect_type(res$formatter, "closure")

  # Test with no matching formatter and no default
  no_match_rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept = 'text/plain')
  )
  no_match_req <- Request$new(no_match_rook)
  no_match_res <- no_match_req$respond()

  expect_snapshot(
    no_match_res$set_formatter(
      json = format_json(),
      xml = format_xml(),
      autofail = TRUE
    ),
    error = TRUE
  )

  # Test with default formatter
  default_result <- no_match_res$set_formatter(
    json = format_json(),
    xml = format_xml(),
    default = "json",
    autofail = TRUE
  )

  expect_true(default_result)
  expect_equal(no_match_res$type, "application/json")
})

test_that("compress works correctly", {
  # Skip if brotli not available
  skip_if_not_installed("brotli")

  # Create request with gzip accept-encoding
  rook <- fiery::fake_request(
    'http://example.com',
    headers = list(Accept_Encoding = 'gzip, deflate')
  )
  req <- Request$new(rook, compression_limit = 10) # Low threshold for testing
  res <- req$respond()

  # Set a compressible type and content
  res$type <- "text/plain"
  res$body <- "This is a test string that should be compressed"

  # Compress
  result <- res$compress()

  expect_true(result)
  expect_equal(res$get_header("Content-Encoding"), "gzip")
  expect_equal(res$get_header("Vary"), "Accept-Encoding")
  expect_true(is.raw(res$body))

  # Test with uncompressible type
  res$type <- "image/jpeg"
  res$body <- "Binary data"

  uncompressible_result <- res$compress()
  expect_false(uncompressible_result)

  # Test with force=TRUE
  force_result <- res$compress(force = TRUE)
  expect_true(force_result)
})

test_that("content_length works correctly", {
  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Test with string body
  test_string <- "Test string"
  res$body <- test_string
  expect_equal(res$content_length(), nchar(test_string, "bytes"))

  # Test with raw body
  raw_data <- charToRaw("Binary data")
  res$body <- raw_data
  expect_equal(res$content_length(), length(raw_data))

  # Test with file body
  temp_file <- tempfile()
  writeLines("File content", temp_file)
  on.exit(unlink(temp_file))

  res$file <- temp_file
  expect_equal(res$content_length(), file.size(temp_file))
})

test_that("as_list prepares response correctly", {
  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Set some properties
  res$status <- 200L
  res$body <- "Test body"
  res$set_header("X-Custom", "Value")

  # Get as list
  list_response <- res$as_list()

  expect_equal(list_response$status, 200L)
  expect_equal(list_response$body, "Test body")
  expect_equal(list_response$headers[["content-type"]], "text/plain")
  expect_equal(list_response$headers[["x-custom"]], "Value")
  expect_true("date" %in% names(list_response$headers))

  # Test with formatter
  res$body <- list(name = "Test")
  res$set_formatter(json = format_json())

  formatted_response <- res$as_list()
  expect_equal(formatted_response$body, '{\"name\":[\"Test\"]}')
})

test_that("as_message prints response correctly", {
  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Set properties
  res$status <- 200L
  res$body <- "Test response body"
  res$set_header("X-Custom", "Test header")

  # Capture output
  output <- capture.output(res$as_message())

  # Check content
  expect_match(output[1], "HTTP/1.1 200 OK", fixed = TRUE)
  expect_true(any(grepl("X-Custom: Test header", output)))
  expect_true(any(grepl("Content-Length: 18", output)))
  expect_true(any(grepl("Test response body", output)))
})

test_that("reset clears response state", {
  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Set some state
  res$status <- 200L
  res$body <- "Test body"
  res$set_header("X-Custom", "Value")
  res$set_data("test_key", "test_value")

  # Reset
  res$reset()

  # Check defaults
  expect_equal(res$status, 404L)
  expect_equal(res$body, "")
  expect_null(res$get_header("X-Custom"))
  expect_null(res$get_data("test_key"))
  expect_equal(res$type, "text/plain")
})

test_that("session management works correctly", {
  skip_if_not_installed("sodium")

  # Generate a key for the session cookie
  key <- sodium::bin2hex(sodium::random(32))

  # Create a session cookie settings object
  session_cookie_settings <- session_cookie(
    name = "test_session",
    path = "/app",
    secure = TRUE
  )

  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook, key = key, session_cookie = session_cookie_settings)
  res <- req$respond()

  # Set session data
  res$session <- list(user_id = 123, logged_in = TRUE)

  # Check that session is accessible from both request and response
  expect_equal(res$session$user_id, 123)
  expect_equal(req$session$user_id, 123)

  # Check that session data is included in response headers
  list_response <- res$as_list()
  session_headers <- list_response$headers[
    names(list_response$headers) == "set-cookie"
  ]
  expect_true(any(grepl("test_session=", session_headers)))
})

test_that("data store functions work correctly", {
  # Create request/response
  rook <- fiery::fake_request('http://example.com')
  req <- Request$new(rook)
  res <- req$respond()

  # Test set_data and get_data
  res$set_data("key1", "value1")
  res$set_data("key2", list(a = 1, b = 2))

  expect_equal(res$get_data("key1"), "value1")
  expect_equal(res$get_data("key2"), list(a = 1, b = 2))
  expect_true(res$has_data("key1"))
  expect_false(res$has_data("non_existent"))

  # Test remove_data
  res$remove_data("key1")
  expect_false(res$has_data("key1"))
  expect_true(res$has_data("key2"))

  # Test data_store accessor
  all_data <- res$data_store
  expect_type(all_data, "list")
  expect_named(all_data, "key2")

  # Test immutability of data store
  expect_snapshot(res$data_store <- list(), error = TRUE)
})
