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

key <- random_key()
session_cookie <- session_cookie()

test_that("Requests know about session cookies", {
  # No session cookie settings
  req <- Request$new(rook)
  res <- req$respond()
  expect_null(req$session_cookie_settings)
  expect_null(res$session_cookie_settings)
  expect_false(req$has_key)
  expect_false(res$has_key)
  expect_equal(req$session, list())
  expect_equal(res$session, list())
  expect_false(req$has_session_cookie)

  # Settings but no cookie
  req <- Request$new(rook, key = key, session_cookie = session_cookie)
  res <- req$respond()
  expect_equal(req$session_cookie_settings, session_cookie)
  expect_equal(res$session_cookie_settings, session_cookie)
  expect_true(req$has_key)
  expect_true(res$has_key)
  expect_equal(req$session, list())
  expect_equal(res$session, list())
  expect_false(req$has_session_cookie)

  # Adding session cookie to request
  session_val <- list(test = 4)
  val <- url_encode(req$encode_string(jsonlite::toJSON(session_val)))
  old_cookie <- rook$HTTP_COOKIE
  rook$HTTP_COOKIE <- paste0(old_cookie, "; ", session_cookie$name, "=", val)

  # Cookie but no settings
  req <- Request$new(rook)
  res <- req$respond()
  expect_null(req$session_cookie_settings)
  expect_null(res$session_cookie_settings)
  expect_false(req$has_key)
  expect_false(res$has_key)
  expect_equal(req$session, list())
  expect_equal(res$session, list())
  expect_false(req$has_session_cookie)

  # Cookie and settings
  req <- Request$new(rook, key = key, session_cookie = session_cookie)
  res <- req$respond()
  expect_equal(req$session_cookie_settings, session_cookie)
  expect_equal(res$session_cookie_settings, session_cookie)
  expect_true(req$has_key)
  expect_true(res$has_key)
  expect_equal(req$session, session_val)
  expect_equal(res$session, session_val)
  expect_true(req$has_session_cookie)

  rook$HTTP_COOKIE <- old_cookie
})

test_that("Response knows how to handle session cookies", {
  # No session cookie settings
  req <- Request$new(rook)
  res <- req$respond()

  expect_snapshot(res$session$test <- 4)
  expect_equal(res$session, list())

  # Adding session cookie to request
  req <- Request$new(rook, key = key, session_cookie = session_cookie)
  session_val <- list(test = 4)
  val <- url_encode(req$encode_string(jsonlite::toJSON(session_val)))
  old_cookie <- rook$HTTP_COOKIE
  rook$HTTP_COOKIE <- paste0(old_cookie, "; ", session_cookie$name, "=", val)

  req <- Request$new(rook, key = key, session_cookie = session_cookie)
  res <- req$respond()

  set_cookie <- res$as_list()$headers[["set-cookie"]]

  expect_true(grepl(paste0("^", session_cookie$name, "=.*; HttpOnly$"), set_cookie))
  cookie_val <- sub(paste0("^", session_cookie$name, "=(.*); HttpOnly$"), "\\1", set_cookie)

  expect_equal(jsonlite::fromJSON(res$decode_string(url_decode(cookie_val))), res$session)

  res$session$test2 <- "A"

  set_cookie <- res$as_list()$headers[["set-cookie"]]
  cookie_val <- sub(paste0("^", session_cookie$name, "=(.*); HttpOnly$"), "\\1", set_cookie)

  expect_equal(jsonlite::fromJSON(res$decode_string(url_decode(cookie_val))), res$session)

  res$session <- NULL

  set_cookie <- res$as_list()$headers[["set-cookie"]]
  expect_equal(set_cookie, paste0(session_cookie$name, "=; Expires=Thu, 01 Jan 1970 00:00:00 GMT"))

  rook$HTTP_COOKIE <- old_cookie
})
