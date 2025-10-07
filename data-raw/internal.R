## code to prepare `mimes` dataset goes here

db <- jsonlite::read_json(
  "https://cdn.jsdelivr.net/gh/jshttp/mime-db@master/db.json"
)
mimes <- data.frame(
  name = names(db),
  extensions = I(unname(lapply(db, function(x) x$extensions))),
  charset = unname(vapply(
    db,
    function(x) x$charset %||% NA_character_,
    character(1)
  )),
  compressible = unname(vapply(
    db,
    function(x) x$compressible %||% NA,
    logical(1)
  )),
  source = unname(vapply(
    db,
    function(x) x$source %||% NA_character_,
    character(1)
  ))
)

# Additional processing
geojson <- which(mimes$name == "application/vnd.geo+json")
mimes$extensions[[geojson]] <- unique(append(
  mimes$extensions[[geojson]],
  "geojson"
))

gpkg <- which(mimes$name == "application/geopackage+sqlite3")
mimes$extensions[[gpkg]] <- unique(append(mimes$extensions[[gpkg]], "gpkg"))

jsonp <- which(mimes$name == "application/javascript")
mimes$extensions[[jsonp]] <- unique(append(mimes$extensions[[jsonp]], "jsonp"))

plain <- which(mimes$name == "text/plain")
mimes$extensions[[plain]] <- unique(append(
  mimes$extensions[[plain]],
  c("r", "rd")
))

md <- which(mimes$name == "text/x-markdown")
mimes$extensions[[md]] <- unique(append(
  mimes$extensions[[md]],
  c("md", "rmd", "qmd")
))

md <- which(mimes$name == "text/markdown")
mimes$extensions[[md]] <- unique(append(
  mimes$extensions[[md]],
  c("md", "rmd", "qmd")
))

mimes <- rbind(
  mimes,
  data.frame(
    name = "text/x-sweave",
    extensions = I(list("rnw")),
    charset = NA_character_,
    compressible = TRUE,
    source = "reqres"
  )
)

scss <- which(mimes$name == "text/css")
mimes$extensions[[scss]] <- unique(append(mimes$extensions[[scss]], "scss"))

mimes <- rbind(
  mimes,
  data.frame(
    name = "application/rds",
    extensions = I(list("rds")),
    charset = NA_character_,
    compressible = FALSE,
    source = "reqres"
  )
)

feather <- which(mimes$name == "application/vnd.apache.arrow.file")
mimes$extensions[[feather]] <- unique(append(
  mimes$extensions[[feather]],
  "feather"
))

parquet <- which(mimes$name == "application/vnd.apache.parquet")
mimes$extensions[[parquet]] <- unique(append(
  mimes$extensions[[parquet]],
  "parquet"
))

# Fast Extension lookup
mimes_ext <- data.frame(
  ext = unlist(mimes$extensions),
  index = rep(seq_along(mimes$extensions), lengths(mimes$extensions))
)

# Status code names

## Main RFC
doc <- "https://datatracker.ietf.org/doc/html/rfc9110"
codes <- rvest::read_html(doc) |>
  rvest::html_element("#status\\.codes") |>
  rvest::html_elements('div[id^="status"]>section>h4')

links <- codes |>
  rvest::html_element('.section-number') |>
  rvest::html_attr("href")
links <- paste0(doc, links)
codes <- codes |>
  rvest::html_element('.section-name') |>
  rvest::html_text()

status <- data.frame(
  code = as.integer(substr(codes, 1, 3)),
  message = sub("^\\d\\d\\d ", "", codes),
  link = links
)

## A Few additionals
doc <- "https://datatracker.ietf.org/doc/html/rfc6585"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h2')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## Legal reasons
doc <- "https://datatracker.ietf.org/doc/html/rfc7725"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h2')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## Early hints
doc <- "https://datatracker.ietf.org/doc/html/rfc8297"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h2')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d:", "", codes),
    link = links
  )
)

## WebDAV
doc <- "https://datatracker.ietf.org/doc/html/rfc4918"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h3')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## WebDAV++
doc <- "https://datatracker.ietf.org/doc/html/rfc5842"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h3')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## IM Used
doc <- "https://datatracker.ietf.org/doc/html/rfc3229"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h4')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## Too Early
doc <- "https://datatracker.ietf.org/doc/html/rfc8470"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h3')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\((.+)\\).*$", "\\1", codes),
    link = links
  )
)

## Variant also negotiates
doc <- "https://datatracker.ietf.org/doc/html/rfc2295"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h3')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

## Not Extended
doc <- "https://datatracker.ietf.org/doc/html/rfc2774"
codes <- rvest::read_html(doc) |>
  rvest::html_elements('span.h2')

links <- codes |>
  rvest::html_element('.selflink') |>
  rvest::html_attr("href")
links <- paste0(doc, links)

codes <- codes |>
  rvest::html_text()

is_code <- grepl("\\d\\d\\d", codes)
links <- links[is_code]
codes <- codes[is_code]

status <- rbind(
  status,
  data.frame(
    code = as.integer(sub("^.*(\\d\\d\\d).*$", "\\1", codes)),
    message = sub("^.*\\d\\d\\d\\s+", "", codes),
    link = links
  )
)

status <- status[!duplicated(status$code), ]
status <- status[order(status$code), ]
attr(status, "row.names") <- .set_row_names(nrow(status))

usethis::use_data(mimes, mimes_ext, status, overwrite = TRUE, internal = TRUE)
