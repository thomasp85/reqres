test_that("format_json works correctly", {
  # Create a formatter function
  json_formatter <- format_json(pretty = TRUE)

  # Test with different data types
  # Simple vector
  numbers <- 1:3
  expect_equal(
    json_formatter(numbers),
    jsonlite::toJSON(numbers, pretty = TRUE)
  )

  # Data frame
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  expect_equal(
    json_formatter(df),
    jsonlite::toJSON(df, pretty = TRUE)
  )

  # List
  list_data <- list(name = "John", age = 30, hobbies = c("reading", "hiking"))
  expect_equal(
    json_formatter(list_data),
    jsonlite::toJSON(list_data, pretty = TRUE)
  )

  # Test with auto_unbox = TRUE
  unbox_formatter <- format_json(auto_unbox = TRUE)
  expect_equal(
    unbox_formatter(list(single = "value")),
    jsonlite::toJSON(list(single = "value"), auto_unbox = TRUE)
  )
})

test_that("format_plain works correctly", {
  # Default separator (newline)
  plain_formatter <- format_plain()

  # Test with character vector
  chars <- c("line 1", "line 2", "line 3")
  expect_equal(
    plain_formatter(chars),
    "line 1\nline 2\nline 3"
  )

  # Test with custom separator
  comma_formatter <- format_plain(sep = ", ")
  expect_equal(
    comma_formatter(chars),
    "line 1, line 2, line 3"
  )

  # Test with numeric vector
  nums <- 1:5
  expect_equal(
    plain_formatter(nums),
    "1\n2\n3\n4\n5"
  )

  # Test with mixed list
  mixed <- list("text", 123, TRUE)
  expect_equal(
    plain_formatter(mixed),
    "text\n123\nTRUE"
  )
})

test_that("format_xml works correctly", {
  # Create a formatter with default options
  xml_formatter <- format_xml()

  # Test with simple named list
  simple_list <- list(item = "value")
  xml_output <- xml_formatter(simple_list)
  expect_match(xml_output, "<document>", fixed = TRUE)
  expect_match(xml_output, "<item>value</item>", fixed = TRUE)
  expect_match(xml_output, "</document>", fixed = TRUE)

  # Test with custom root name
  custom_root_formatter <- format_xml(root_name = "root")
  custom_xml_output <- custom_root_formatter(simple_list)
  expect_match(custom_xml_output, "<root>", fixed = TRUE)
  expect_match(custom_xml_output, "<item>value</item>", fixed = TRUE)
  expect_match(custom_xml_output, "</root>", fixed = TRUE)

  # Test with nested structure
  nested_list <- list(
    person = list(
      name = "John",
      age = "30",
      hobbies = c("reading", "hiking")
    )
  )

  nested_output <- xml_formatter(nested_list)
  expect_match(nested_output, "<person>", fixed = TRUE)
  expect_match(nested_output, "<name>John</name>", fixed = TRUE)
  expect_match(
    nested_output,
    "<hobbies><character>reading</character><character>hiking</character></hobbies>",
    fixed = TRUE
  )

  # Test with plain string
  expect_equal(xml_formatter("raw text"), "raw text")
})

test_that("format_html works correctly", {
  # Create a formatter with default options
  html_formatter <- format_html()

  # Test with simple named list
  simple_list <- list(body = list(h1 = "Title", p = "Paragraph"))
  html_output <- html_formatter(simple_list)
  expect_match(html_output, "<html>", fixed = TRUE)
  expect_match(html_output, "<body>", fixed = TRUE)
  expect_match(html_output, "<h1>Title</h1>", fixed = TRUE)
  expect_match(html_output, "<p>Paragraph</p>", fixed = TRUE)

  # Test with list that already has html root
  html_list <- list(html = list(body = list(p = "Content")))
  html_root_output <- html_formatter(html_list)
  expect_match(html_root_output, "<html>", fixed = TRUE)
  expect_match(html_root_output, "<body>", fixed = TRUE)
  expect_match(html_root_output, "<p>Content</p>", fixed = TRUE)

  # Test with plain string
  expect_equal(html_formatter("raw html"), "raw html")

  # Test with shiny.tag object if available
  if (requireNamespace("shiny", quietly = TRUE)) {
    tag <- shiny::tags$div(shiny::tags$p("Hello"))
    expect_equal(html_formatter(tag), as.character(tag))
  }
})

test_that("format_table works correctly", {
  # Create a formatter with default options
  table_formatter <- format_table()

  # Test with data frame
  df <- data.frame(
    name = c("John", "Alice"),
    age = c(30, 25),
    stringsAsFactors = FALSE
  )

  table_output <- table_formatter(df)
  expect_match(table_output, "name", fixed = TRUE)
  expect_match(table_output, "age", fixed = TRUE)
  expect_match(table_output, "John", fixed = TRUE)
  expect_match(table_output, "30", fixed = TRUE)

  # Test with custom options
  csv_formatter <- format_table(sep = ",", row.names = FALSE)
  csv_output <- csv_formatter(df)
  expect_false(grepl('"1"', csv_output)) # No row names
  expect_match(csv_output, '"name","age"', fixed = TRUE)
  expect_match(csv_output, '"John",30', fixed = TRUE)
})

test_that("listify handles different object types correctly", {
  # Test with scalar
  scalar_result <- listify(42)
  expect_equal(scalar_result, list("42"))

  # Test with character vector
  char_vec <- c("a", "b", "c")
  char_result <- listify(char_vec)
  expect_length(char_result, 3)
  expect_equal(
    char_result,
    list(character = list("a"), character = list("b"), character = list("c"))
  )

  # Test with named list
  named_list <- list(name = "John", age = 30)
  list_result <- listify(named_list)
  expect_equal(names(list_result), c("name", "age"))
  expect_equal(list_result$name, list("John"))
  expect_equal(list_result$age, list("30"))

  # Test with complex nested structure
  nested <- list(
    person = list(name = "John", hobbies = c("reading", "hiking"))
  )
  nested_result <- listify(nested)
  expect_equal(names(nested_result), "person")
  expect_equal(names(nested_result$person), c("name", "hobbies"))
  expect_equal(nested_result$person$name, list("John"))
  expect_equal(
    nested_result$person$hobbies,
    list(character = list("reading"), character = list("hiking"))
  )
})
