test_that("problem_abort creates properly formed error condition", {
  # Create a problem abort function for a specific code
  bad_request_abort <- problem_abort(400L)
  
  # Test that it creates an error with the expected class and attributes
  expect_snapshot({
    bad_request_abort("Invalid request parameters")
  }, error = TRUE)
  
  # Capture the condition to examine its structure
  cnd <- tryCatch(
    bad_request_abort("Test detail"),
    reqres_problem = function(c) c
  )
  
  expect_s3_class(cnd, "reqres_problem")
  expect_equal(cnd$status, 400L)
  expect_equal(cnd$detail, "Test detail")
})

test_that("abort_http_problem creates complete problem condition", {
  # Test basic functionality
  expect_snapshot({
    abort_http_problem(404L, "Resource not found")
  }, error = TRUE)
  
  # Capture the condition to examine its structure
  cnd <- tryCatch(
    abort_http_problem(
      code = 404L,
      detail = "Resource not found",
      title = "Not Found",
      type = "https://example.com/errors/not-found",
      instance = "error-12345"
    ),
    reqres_problem = function(c) c
  )
  
  expect_s3_class(cnd, "reqres_problem")
  expect_equal(cnd$status, 404L)
  expect_equal(cnd$detail, "Resource not found")
  expect_equal(cnd$title, "Not Found")
  expect_equal(cnd$type, "https://example.com/errors/not-found")
  expect_equal(cnd$instance, "error-12345")
})

test_that("abort_status creates simpler status code condition", {
  # Test basic functionality
  expect_snapshot({
    abort_status(403L)
  }, error = TRUE)
  
  # Capture the condition to examine its structure
  cnd <- tryCatch(
    abort_status(403L),
    reqres_problem = function(c) c
  )
  
  expect_s3_class(cnd, "reqres_problem")
  expect_equal(cnd$status, 403L)
  expect_equal(cnd$message, "Forbidden")
  
  # Test with custom message
  cnd_custom <- tryCatch(
    abort_status(403L, "Custom message"),
    reqres_problem = function(c) c
  )
  
  expect_equal(cnd_custom$message, "Custom message")
})

test_that("specific problem abort functions work correctly", {
  # Test a few specific abort functions
  
  # bad request (400)
  cnd_400 <- tryCatch(
    abort_bad_request("Bad input"),
    reqres_problem = function(c) c
  )
  expect_equal(cnd_400$status, 400L)
  expect_equal(cnd_400$detail, "Bad input")
  
  # not found (404)
  cnd_404 <- tryCatch(
    abort_not_found("Resource missing"),
    reqres_problem = function(c) c
  )
  expect_equal(cnd_404$status, 404L)
  expect_equal(cnd_404$detail, "Resource missing")
  
  # internal error (500)
  cnd_500 <- tryCatch(
    abort_internal_error("Server crashed"),
    reqres_problem = function(c) c
  )
  expect_equal(cnd_500$status, 500L)
  expect_equal(cnd_500$detail, "Server crashed")
})

test_that("handle_problem correctly formats response", {
  # Create a mock response object
  mock_response <- list(
    status_with_text = function(status) {
      return(list(type = "status", status = status))
    },
    problem = function(code, detail, title, type, instance) {
      return(list(
        type = "problem",
        status = code,
        detail = detail,
        title = title,
        problem_type = type,
        instance = instance
      ))
    }
  )
  class(mock_response) <- "Response"
  
  # Test handling condition with no detail (simple status)
  simple_cnd <- list(
    status = 404L,
    detail = NULL
  )
  class(simple_cnd) <- c("reqres_problem", "error", "condition")
  
  result_simple <- handle_problem(mock_response, simple_cnd)
  expect_equal(result_simple$type, "status")
  expect_equal(result_simple$status, 404L)
  
  # Test handling detailed problem
  detailed_cnd <- list(
    status = 400L,
    detail = "Invalid parameters",
    title = "Bad Request",
    type = "https://example.com/errors/bad-request",
    instance = "error-5678"
  )
  class(detailed_cnd) <- c("reqres_problem", "error", "condition")
  
  result_detailed <- handle_problem(mock_response, detailed_cnd)
  expect_equal(result_detailed$type, "problem")
  expect_equal(result_detailed$status, 400L)
  expect_equal(result_detailed$detail, "Invalid parameters")
  expect_equal(result_detailed$title, "Bad Request")
  expect_equal(result_detailed$problem_type, "https://example.com/errors/bad-request")
  expect_equal(result_detailed$instance, "error-5678")
})

test_that("is_reqres_problem correctly identifies problem conditions", {
  # Create a problem condition
  problem_cnd <- structure(
    list(message = "Not Found", status = 404L),
    class = c("reqres_problem", "error", "condition")
  )
  
  # Create a regular error condition
  regular_error <- simpleError("Regular error")
  
  # Test identification
  expect_true(is_reqres_problem(problem_cnd))
  expect_false(is_reqres_problem(regular_error))
  expect_false(is_reqres_problem("not a condition"))
})