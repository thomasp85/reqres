test_that("parse_plain works correctly", {
  # Create a parser function with default separator
  plain_parser <- parse_plain()

  # Test with simple text
  text <- "Line 1\nLine 2\nLine 3"
  raw_text <- charToRaw(text)

  result <- plain_parser(raw_text)
  expect_equal(result, c("Line 1", "Line 2", "Line 3"))

  # Test with custom separator
  comma_parser <- parse_plain(sep = ",")
  csv_text <- "value1,value2,value3"
  raw_csv <- charToRaw(csv_text)

  csv_result <- comma_parser(raw_csv)
  expect_equal(csv_result, c("value1", "value2", "value3"))

  # Test with empty input
  empty_result <- plain_parser(charToRaw(""))
  expect_equal(empty_result, character(0))
})

test_that("parse_xml works correctly", {
  # Create a parser function
  xml_parser <- parse_xml()

  # Test with simple XML
  xml_text <- "<root><item>value</item></root>"
  xml_raw <- charToRaw(xml_text)

  result <- xml_parser(xml_raw)
  expect_equal(result$root$item[[1]], "value")

  # Test with nested XML
  nested_xml <- "<root><person><name>John</name><age>30</age></person></root>"
  nested_raw <- charToRaw(nested_xml)

  nested_result <- xml_parser(nested_raw)
  expect_equal(nested_result$root$person$name[[1]], "John")
  expect_equal(nested_result$root$person$age[[1]], "30")
})

test_that("parse_html works correctly", {
  # Create a parser function
  html_parser <- parse_html()

  # Test with simple HTML
  html_text <- "<html><body><h1>Title</h1><p>Paragraph</p></body></html>"
  html_raw <- charToRaw(html_text)

  result <- html_parser(html_raw)
  expect_equal(result$html$body$h1[[1]], "Title")
  expect_equal(result$html$body$p[[1]], "Paragraph")

  # Test with HTML that needs recovery (malformed HTML)
  malformed_html <- "<html><body><p>Text<span>Span</p></body></html>"
  malformed_raw <- charToRaw(malformed_html)

  # This should not error due to RECOVER and NOERROR options
  expect_no_error(html_parser(malformed_raw))
})

test_that("parse_multiform works correctly", {
  # Create a parser function
  multiform_parser <- parse_multiform()

  # Simple multipart form data with boundary
  boundary <- "boundary123"
  form_data <- paste0(
    "--", boundary, "\r\n",
    "Content-Disposition: form-data; name=\"field1\"\r\n\r\n",
    "value1\r\n",
    "--", boundary, "\r\n",
    "Content-Disposition: form-data; name=\"field2\"\r\n\r\n",
    "value2\r\n",
    "--", boundary, "--\r\n"
  )

  raw_form <- charToRaw(form_data)
  directives <- list(boundary = boundary)

  # Skip the test if webutils package is not available
  skip_if_not_installed("webutils")

  # Test parsing multipart form data
  result <- multiform_parser(raw_form, directives)
  expect_equal(rawToChar(result$field1$value), "value1")
  expect_equal(rawToChar(result$field2$value), "value2")
})

test_that("parse_queryform works correctly", {
  # Create a parser function with default options
  queryform_parser <- parse_queryform()

  # Test with simple query string
  query <- "name=John&age=30"
  raw_query <- charToRaw(query)

  result <- queryform_parser(raw_query, list())
  expect_equal(result$name, "John")
  expect_equal(result$age, "30")

  # Test with URL encoded values
  encoded_query <- "name=John%20Doe&location=New%20York"
  raw_encoded <- charToRaw(encoded_query)

  encoded_result <- queryform_parser(raw_encoded, list())
  expect_equal(encoded_result$name, "John Doe")
  expect_equal(encoded_result$location, "New York")

  # Test with array values
  array_query <- "tags=red&tags=green&tags=blue"
  raw_array <- charToRaw(array_query)

  array_result <- queryform_parser(raw_array, list())
  expect_equal(array_result$tags, c("red", "green", "blue"))

  # Test with delimiter
  delim_parser <- parse_queryform(delim = "|")
  delim_query <- "colors=red|green|blue"
  raw_delim <- charToRaw(delim_query)

  delim_result <- delim_parser(raw_delim, list())
  expect_equal(delim_result$colors, c("red", "green", "blue"))
})

test_that("parse_table works correctly", {
  # Create a parser function with default options
  table_parser <- parse_table(header = TRUE, sep = ",")

  # Test with CSV data
  csv_data <- "name,age\nJohn,30\nAlice,25"
  raw_csv <- charToRaw(csv_data)

  result <- table_parser(raw_csv, list())
  expect_equal(result$name, c("John", "Alice"))
  expect_equal(result$age, c(30, 25))

  # Test with custom options
  tsv_parser <- parse_table(header = TRUE, sep = "\t")
  tsv_data <- "name\tage\nJohn\t30\nAlice\t25"
  raw_tsv <- charToRaw(tsv_data)

  tsv_result <- tsv_parser(raw_tsv, list())
  expect_equal(tsv_result$name, c("John", "Alice"))
  expect_equal(tsv_result$age, c(30, 25))
})
