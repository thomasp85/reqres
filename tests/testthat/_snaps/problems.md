# problem_abort creates properly formed error condition

    Code
      bad_request_abort("Invalid request parameters")
    Condition
      Error:
      ! Invalid request parameters

# abort_http_problem creates complete problem condition

    Code
      abort_http_problem(404L, "Resource not found")
    Condition
      Error:
      ! Resource not found

# abort_status creates simpler status code condition

    Code
      abort_status(403L)
    Condition
      Error:
      ! Forbidden

