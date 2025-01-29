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
  expect_snapshot(res$remove_header('Date'))

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
  expect_snapshot(res$remove_data('test'))
})

test_that('cookies can be get, set, and removed', {
  req <- Request$new(rook)
  res <- Response$new(req)

  expect_false(res$has_cookie('test'))
  expect_snapshot(res$remove_cookie('test'))

  exp <- Sys.Date() + 1000
  res$set_cookie('test', 'this is a test', TRUE, expires = exp, http_only = TRUE, max_age = 1000, path = '/test', secure = TRUE, same_site = 'Lax')
  expect_true(res$has_cookie('test'))
  expect_equal(res$as_list()$headers[['Set-Cookie']], paste0('test=this%20is%20a%20test; Expires=', to_http_date(exp), '; HttpOnly; Max-Age=1000; Path=/test; Secure; SameSite=Lax'))
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

  res$set_links(list(alternate = '/feed'))
  res$has_header('Link')
  expect_equal(res$get_header('Link'), "</feed>; rel=\"alternate\"")
})

test_that('files are added correctly', {
  req <- Request$new(rook)
  res <- Response$new(req)
  file <- system.file('DESCRIPTION', package = 'reqres')

  expect_snapshot(res$file <- paste0(file, '_test'), error = TRUE)
  res$file <- file
  expect_equal(res$body, c(file = file))
  expect_equal(res$type, 'text/plain')
  expect_equal(res$get_header('Last-Modified'), to_http_date(file.mtime(file)))
  res$attach(file)
  expect_equal(res$get_header('Content-Disposition'), "attachment; filename=DESCRIPTION")

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
  expect_true(res$format(default_formatters, compress = FALSE))
  expect_equal(res$body, jsonlite::toJSON(body))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'gzip'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(default_formatters))
  expect_equal(res$body, gzip(charToRaw(jsonlite::toJSON(body))))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'br'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(default_formatters))
  expect_equal(res$body, brotli::brotli_compress(charToRaw(jsonlite::toJSON(body))))

  rook2 <- fiery::fake_request(
    url = 'http://127.0.0.1:80/summary?id=2347&user=Thomas+Lin+Pedersen',
    content = '',
    headers = modifyList(headers, list(Accept_Encoding = 'deflate'))
  )
  req <- Request$new(rook2)
  res <- Response$new(req)
  res$body <- body
  expect_true(res$format(default_formatters))
  expect_equal(res$body, memCompress(charToRaw(jsonlite::toJSON(body))))
})
