## code to prepare `mimes` dataset goes here

db <- jsonlite::read_json("https://cdn.jsdelivr.net/gh/jshttp/mime-db@master/db.json")
mimes <- data.frame(
  name = names(db),
  extensions = I(unname(lapply(db, function(x) x$extensions))),
  charset = unname(vapply(db, function(x) x$charset %||% NA_character_, character(1))),
  compressible = unname(vapply(db, function(x) x$compressible %||% NA, logical(1))),
  source = unname(vapply(db, function(x) x$source %||% NA_character_, character(1)))
)

# Additional processing
geojson <- which(mimes$name == "application/vnd.geo+json")
mimes$extensions[[geojson]] <- unique(append(mimes$extensions[[geojson]], "geojson"))

gpkg <- which(mimes$name == "application/geopackage+sqlite3")
mimes$extensions[[gpkg]] <- unique(append(mimes$extensions[[gpkg]], "gpkg"))

jsonp <- which(mimes$name == "application/javascript")
mimes$extensions[[jsonp]] <- unique(append(mimes$extensions[[jsonp]], "jsonp"))

plain <- which(mimes$name == "text/plain")
mimes$extensions[[plain]] <- unique(append(mimes$extensions[[plain]], c("r", "rd")))

md <- which(mimes$name == "text/x-markdown")
mimes$extensions[[md]] <- unique(append(mimes$extensions[[md]], c("md", "rmd", "qmd")))

md <- which(mimes$name == "text/markdown")
mimes$extensions[[md]] <- unique(append(mimes$extensions[[md]], c("md", "rmd", "qmd")))

mimes <- rbind(mimes,
  data.frame(name = "text/x-sweave", extensions = I(list("rnw")), charset = NA_character_, compressible = TRUE, source = "reqres")
)

scss <- which(mimes$name == "text/css")
mimes$extensions[[scss]] <- unique(append(mimes$extensions[[scss]], "scss"))

mimes <- rbind(mimes,
  data.frame(name = "application/rds", extensions = I(list("rds")), charset = NA_character_, compressible = FALSE, source = "reqres")
)

feather <- which(mimes$name == "application/vnd.apache.arrow.file")
mimes$extensions[[feather]] <- unique(append(mimes$extensions[[feather]], "feather"))

parquet <- which(mimes$name == "application/vnd.apache.parquet")
mimes$extensions[[parquet]] <- unique(append(mimes$extensions[[parquet]], "parquet"))

# Fast Extension lookup
mimes_ext <- data.frame(
  ext = unlist(mimes$extensions),
  index = rep(seq_along(mimes$extensions), lengths(mimes$extensions))
)

usethis::use_data(mimes, mimes_ext, overwrite = TRUE, internal = TRUE)
