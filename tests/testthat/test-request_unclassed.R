headers <- list(
  Content_Type = 'application/json',
  Date = 'Wed, 21 Oct 2015 07:28:00 GMT',
  Accept = 'application/json, application/xml; q=0.5, text/*; q=0.3',
  Accept_Encoding = 'gzip, br',
  Cookie = 'id=Thomas; key=123',
  X_Forwarded_For = '500.0.0.0, 400.0.0.0',
  X_Forwarded_Host = 'www.example.com:80',
  X_Forwarded_Proto = 'https',
  X_Custom_Message = '"Testing string literals, with comma", no-literal'
)
body <- '{"name":["Thomas Lin Pedersen"],"age":[31],"homepage":["www.data-imaginist.com","www.github.com/thomasp85"]}'
rook <- fiery::fake_request(
  url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
  content = body,
  headers = headers,
  REMOTE_ADDR = '230.45.12.45'
)

test_that('request gets created correctly', {
  req <- unclass_request(Request$new(rook))
  expect_null(req$body)
  expect_equal(req$host, "127.0.0.1:80")
  expect_false(req$trust)
  expect_equal(req$method, 'get')
  expect_equal(req$cookies, list(id = 'Thomas', key = '123'))
  expect_named(req$headers, tolower(sort(names(headers))))
  expect_length(req$headers$accept, 3)
  expect_equal(req$headers$content_type, 'application/json')
  expect_equal(req$ip, '230.45.12.45')
  expect_equal(req$ips, character(0))
  expect_equal(req$protocol, 'http')
  expect_equal(req$root, '')
  expect_equal(req$path, '/summary')
  expect_equal(
    req$url,
    'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen'
  )
  expect_equal(req$query, list(id = "2347", user = 'Thomas Lin Pedersen'))
  expect_false(req$xhr)
  expect_false(req$secure)
  expect_identical(req$origin, rook)
})

test_that('trust works', {
  req <- unclass_request(Request$new(rook))
  expect_false(req$trust)
  expect_snapshot(req$trust <- 'test', error = TRUE)
  req$trust <- TRUE
  expect_true(req$trust)
  expect_equal(req$host, 'www.example.com:80')
  expect_equal(req$protocol, 'https')
  expect_true(req$secure)
  expect_equal(req$ip, '500.0.0.0')
  expect_equal(req$ips, c('500.0.0.0', '400.0.0.0'))
})

test_that('header parsing works', {
  req <- unclass_request(Request$new(rook))
  expect_equal(req$get_header('Content-Type'), headers$Content_Type)
  expect_equal(req$get_header('Date'), headers$Date)
  expect_equal(
    req$get_header('X-Custom-Message'),
    c("\"Testing string literals, with comma\"", "no-literal")
  )
})

test_that('response can be generated', {
  req <- unclass_request(Request$new(rook))
  res <- req$respond()
  expect_identical(req$response, res)
  req2 <- unclass_request(Request$new(rook))
  res2 <- req2$respond()
  expect_snapshot(req$response <- res2, error = TRUE)
})

test_that('content type can be queried', {
  req <- unclass_request(Request$new(rook))
  expect_true(req$is('json'))
  expect_true(req$is('application/json'))
  expect_true(req$is('application/*'))
  expect_true(req$is('*/json'))
  expect_true(req$is('*'))
  expect_false(req$is('application'))
  expect_false(req$is('text'))
})

test_that('body can be parsed', {
  req <- unclass_request(Request$new(rook))
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))
  req <- unclass_request(Request$new(rook))
  res <- req$respond()
  expect_snapshot(req$parse(xml = parse_xml()), error = TRUE)
  req <- unclass_request(Request$new(rook))
  res <- req$respond()
  expect_false(req$parse(xml = parse_xml(), autofail = FALSE))
  expect_equal(res$status, 404L)
  req <- unclass_request(Request$new(rook))
  res <- req$respond()
  expect_true(req$parse_raw())
  expect_equal(req$body, charToRaw(paste0(body, '\n')))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = brotli::brotli_compress(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'br'))
  )
  req <- unclass_request(Request$new(rook2))
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = memCompress(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'deflate'))
  )
  req <- unclass_request(Request$new(rook2))
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = gzip(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'gzip'))
  )
  req <- unclass_request(Request$new(rook2))
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))
})

test_that('accept negotiation works', {
  req <- unclass_request(Request$new(rook))
  expect_null(req$accepts('application/zip'))
  expect_equal(req$accepts(names(default_formatters)), 'application/json')
  expect_equal(req$accepts_encoding(c('deflate', 'zip')), 'identity')
  expect_equal(req$accepts_encoding(c('gzip', 'br')), 'gzip')
})

test_that("encode and decode works", {
  test_string <- "This is a test string for testing"

  # No key
  req <- unclass_request(Request$new(rook))
  encoded <- req$encode_string(test_string)
  expect_equal(req$decode_string(encoded), test_string)

  # With key
  req <- unclass_request(Request$new(rook, key = random_key()))
  encoded_key <- req$encode_string(test_string)
  expect_equal(req$decode_string(encoded_key), test_string)

  expect_true(encoded != encoded_key)
})

test_that("has_header works correctly", {
  headers <- list(
    Content_Type = 'application/json',
    Accept = 'application/json'
  )
  rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test',
    content = '{"name":"test"}',
    headers = headers
  )

  req <- unclass_request(Request$new(rook))

  expect_true(req$has_header("Content-Type"))
  expect_true(req$has_header("Accept"))
  expect_false(req$has_header("X-Custom-Header"))
})

test_that("accepts_charsets works correctly", {
  rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test',
    headers = list(
      Accept_Charset = 'utf-8, iso-8859-1;q=0.5'
    )
  )

  req <- unclass_request(Request$new(rook))

  expect_equal(req$accepts_charsets(c("utf-8", "iso-8859-1")), "utf-8")
  expect_equal(req$accepts_charsets(c("iso-8859-1", "ascii")), "iso-8859-1")
  expect_null(req$accepts_charsets(character(0)))
})

test_that("accepts_language works correctly", {
  rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test',
    headers = list(
      Accept_Language = 'en-US, fr-FR;q=0.8, de;q=0.5'
    )
  )

  req <- unclass_request(Request$new(rook))

  expect_equal(req$accepts_language(c("en-US", "fr-FR")), "en-US")
  expect_equal(req$accepts_language(c("fr-FR", "de")), "fr-FR")
  expect_equal(req$accepts_language(c("en", "fr")), "en") # Matches main language part
  expect_null(req$accepts_language(c("es", "it"))) # No match
})

test_that("encode_string and decode_string work correctly without key", {
  rook <- fiery::fake_request('http://example.com')
  req <- unclass_request(Request$new(rook))

  test_string <- "This is a test string"
  encoded <- req$encode_string(test_string)
  decoded <- req$decode_string(encoded)

  expect_equal(decoded, test_string)
  expect_true(nchar(encoded) > 0)
  expect_false(encoded == test_string)

  # Test empty string handling
  expect_equal(req$encode_string(""), "")
  expect_equal(req$decode_string(""), "")
})

test_that("encode_string and decode_string work with key", {
  skip_if_not_installed("sodium")

  # Generate a random key for testing
  key <- sodium::bin2hex(sodium::random(32))

  rook <- fiery::fake_request('http://example.com')
  req <- unclass_request(Request$new(rook, key = key))

  test_string <- "This is a test string"
  encoded <- req$encode_string(test_string)
  decoded <- req$decode_string(encoded)

  expect_equal(decoded, test_string)
  expect_true(grepl("_", encoded)) # Should contain the delimiter for nonce
})

test_that("query_delim setter works correctly", {
  rook <- fiery::fake_request(
    url = 'http://example.com/test?items=1,2,3'
  )

  req <- unclass_request(Request$new(rook))
  expect_null(req$query_delim)
  expect_equal(req$query$items, "1,2,3")

  # Set delimiter and verify query is re-parsed
  req$query_delim <- ","
  expect_equal(req$query$items, c("1", "2", "3"))

  # Change delimiter
  req$query_delim <- "|"
  expect_equal(req$query$items, "1,2,3") # Back to original since delimiter doesn't match
})

test_that("clear works correctly", {
  rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test?id=123',
    content = '{"name":"test"}',
    headers = list(Content_Type = "application/json")
  )

  req <- unclass_request(Request$new(rook))

  # Modify some properties
  req$parse(json = parse_json())
  expect_equal(req$body$name, "test")

  # Reset the request
  req$clear()

  # Verify the reset worked
  expect_null(req$body)

  req$initialize(rook)
  # Verify that we can parse again
  req$parse(json = parse_json())
  expect_equal(req$body$name, "test")
})

test_that("as.Request and is.Request work correctly", {
  rook <- fiery::fake_request('http://example.com')

  # Test coercion from rook environment
  req1 <- as.Request(rook)
  expect_true(is.Request(req1))

  # Test that coercion from Request is identity
  req2 <- as.Request(req1)
  expect_identical(req1, req2)

  # Test non-Request object
  expect_false(is.Request(list()))

  # Test error on non-Rook environment
  non_rook_env <- new.env()
  expect_snapshot(as.Request(non_rook_env), error = TRUE)
})

test_that("as_message prints request details correctly", {
  headers <- list(
    Content_Type = 'application/json',
    Accept = 'application/json'
  )
  rook <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test?id=123',
    content = '{"name":"test"}',
    headers = headers
  )

  req <- unclass_request(Request$new(rook))

  # Capture output from as_message
  output <- capture.output(req$as_message())

  # Check for expected elements in the output
  expect_match(output[1], "GET /test?id=123 HTTP/1.1", fixed = TRUE)
  expect_match(
    paste(output, collapse = "\n"),
    "Content-Type: application/json",
    fixed = TRUE
  )
  expect_match(
    paste(output, collapse = "\n"),
    "Accept: application/json",
    fixed = TRUE
  )
})

test_that("parse_raw handles compressed content", {
  json_content <- '{"name":"test"}'

  # Create request with gzip compression
  gzip_content <- gzip(charToRaw(json_content))
  rook_gzip <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test',
    content = gzip_content,
    headers = list(
      Content_Type = 'application/json',
      Content_Encoding = 'gzip'
    )
  )

  req_gzip <- unclass_request(Request$new(rook_gzip))
  expect_true(req_gzip$parse_raw())
  expect_equal(rawToChar(req_gzip$body), json_content)

  # Create request with deflate compression
  deflate_content <- memCompress(charToRaw(json_content))
  rook_deflate <- fiery::fake_request(
    url = 'http://127.0.0.1:80/test',
    content = deflate_content,
    headers = list(
      Content_Type = 'application/json',
      Content_Encoding = 'deflate'
    )
  )

  req_deflate <- unclass_request(Request$new(rook_deflate))
  expect_true(req_deflate$parse_raw())
  expect_equal(rawToChar(req_deflate$body), json_content)
})

test_that("get_charset_spec works correctly", {
  # Create a request object
  rook <- fiery::fake_request('http://example.com')
  req <- unclass_request(Request$new(rook))

  # Access the private function using the ::: operator
  # Note: In a real test, you'd need to expose this function or test it indirectly
  # Here we're showing the test as if you had access to the private function

  # Create a mock accept charsets data frame
  accepts <- data.frame(
    full = c("utf-8;q=1.0", "iso-8859-1;q=0.8", "*;q=0.5"),
    main = c("utf-8", "iso-8859-1", "*"),
    q = c(1.0, 0.8, 0.5),
    stringsAsFactors = FALSE
  )

  # Test exact match
  result <- get_charset_spec(c("utf-8", "iso-8859-1"), accepts)
  expect_equal(result, 1) # utf-8 should be preferred

  # Test wildcard match
  result <- get_charset_spec("utf-16", accepts)
  expect_equal(result, 1) # Should match * with index 1 (first in the input)

  # Test no match
  result <- get_charset_spec(character(0), accepts)
  expect_null(result)
})
