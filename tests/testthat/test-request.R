context("request")

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

test_that('request gets created correctly', {
  req <- Request$new(rook)
  expect_null(req$body)
  expect_equal(req$host, "127.0.0.1:80")
  expect_false(req$trust)
  expect_equal(req$method, 'get')
  expect_equal(req$cookies, list(id = 'Thomas', key = '123'))
  expect_named(req$headers, sort(names(headers)))
  expect_length(req$headers$Accept, 3)
  expect_equal(req$headers$Content_Type, 'application/json')
  expect_equal(req$ip, '230.45.12.45')
  expect_equal(req$ips, character(0))
  expect_equal(req$protocol, 'http')
  expect_null(req$root)
  expect_equal(req$path, '/summary')
  expect_equal(req$url, 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen')
  expect_equal(req$query, list(id = 2347, user = 'Thomas Lin Pedersen'))
  expect_false(req$xhr)
  expect_false(req$secure)
  expect_identical(req$origin, rook)
  expect_null(req$response)
  expect_output(print(req), 'A HTTP request')
})

test_that('trust works', {
  req <- Request$new(rook)
  expect_false(req$trust)
  expect_error(req$trust <- 'test')
  req$trust <- TRUE
  expect_true(req$trust)
  expect_equal(req$host, 'www.example.com:80')
  expect_equal(req$protocol, 'https')
  expect_true(req$secure)
  expect_equal(req$ip, '500.0.0.0')
  expect_equal(req$ips, c('500.0.0.0', '400.0.0.0'))
})

test_that('header parsing works', {
  req <- Request$new(rook)
  expect_equal(req$get_header('Content-Type'), headers$Content_Type)
})

test_that('response can be generated', {
  req <- Request$new(rook)
  res <- req$respond()
  expect_identical(req$response, res)
  req2 <- Request$new(rook)
  res2 <- req2$respond()
  expect_error(req$response <- res2)
})

test_that('content type can be queried', {
  req <- Request$new(rook)
  expect_true(req$is('json'))
  expect_true(req$is('application/json'))
  expect_false(req$is('application'))
  expect_false(req$is('text'))
  expect_false(req$is('*'))
})

test_that('body can be parsed', {
  req <- Request$new(rook)
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))
  req <- Request$new(rook)
  res <- req$respond()
  expect_false(req$parse(xml = parse_xml()))
  expect_equal(res$status, 415L)
  req <- Request$new(rook)
  res <- req$respond()
  expect_false(req$parse(xml = parse_xml(), autofail = FALSE))
  expect_equal(res$status, 404L)
  req <- Request$new(rook)
  res <- req$respond()
  expect_true(req$parse_raw())
  expect_equal(req$body, charToRaw(paste0(body, '\n')))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = brotli::brotli_compress(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'br'))
  )
  req <- Request$new(rook2)
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = memCompress(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'deflate'))
  )
  req <- Request$new(rook2)
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = gzip(charToRaw(body)),
    headers = c(headers, list(Content_Encoding = 'gzip'))
  )
  req <- Request$new(rook2)
  expect_true(req$parse(json = parse_json()))
  expect_equal(req$body, jsonlite::fromJSON(body))
})

test_that('accept negotiation works', {
  req <- Request$new(rook)
  expect_null(req$accepts('application/zip'))
  expect_equal(req$accepts(names(default_formatters)), 'application/json')
  expect_equal(req$accepts_encoding(c('deflate', 'zip')), 'identity')
  expect_equal(req$accepts_encoding(c('gzip', 'br')), 'gzip')
})
