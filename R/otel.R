otel_tracer_name <- "r.package.reqres"

get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})

testthat__is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


request_ospan <- function(request, start_time, tracer) {
  # OpenTelemetry
  # TODO: Allow server introspection of actual server host and port (network.local.address and network.local.port)
  # http.response.status_code and http.response.header.<key> can only be set later
  span <- otel::start_span(
    tolower(request$method),
    options = list(
      start_system_time = start_time,
      kind = "server",
      parent = otel::extract_http_context(request$headers)
    ),
    attributes = list2(
      http.request.method = toupper(request$method),
      url.path = request$path,
      url.scheme = request$protocol,
      network.protocol.name = "http",
      server.port = as.integer(sub("^.*:(.*)$", "\\1", request$host)),
      url.query = request$querystring,
      client.address = request$ip,
      network.protocol.version = "1.1",
      server.address = sub("^(.*):.*$", "\\1", request$host),
      user_agent.original = request$headers[["user_agent"]],
      !!!set_names(
        request$headers,
        paste0(
          "http.request.header.",
          gsub("_", "-", names(request$headers))
        )
      )
    ),
    tracer = tracer
  )
}
