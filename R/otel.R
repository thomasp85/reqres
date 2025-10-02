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

get_meter <- local({
  meter <- NULL
  function() {
    if (!is.null(meter)) {
      return(meter)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_meter())
    }
    meter <<- otel::get_meter()
    meter
  }
})
metric_attributes <- function(request, response) {
  list2(
    http.request.method = toupper(request$method),
    url.scheme = request$protocol,
    !!!if(response$status >= 500) list(error.type = as.character(response$status)),
    http.response.status_code = response$status,
    network.protocol.name = "http",
    network.protocol.version = "1.1",
    server.address = sub("^(.*):.*$", "\\1", request$host),
    server.port = as.integer(sub("^.*:(.*)$", "\\1", request$host))
  )
}
record_duration <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.request.duration",
      request$duration,
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}
push_active_request <- function(request) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::up_down_counter_add(
      "http.server.active_requests",
      value = 1L,
      attributes = list(
        http.request.method = toupper(request$method),
        url.scheme = request$protocol,
        server.address = sub("^(.*):.*$", "\\1", request$host),
        server.port = as.integer(sub("^.*:(.*)$", "\\1", request$host))
      ),
      context = request$otel,
      meter = meter
    )
  }
}
pop_active_request <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::up_down_counter_add(
      "http.server.active_requests",
      value = -1L,
      attributes = attributes[
        c("http.request.method", "url.scheme", "server.address", "server.port")
      ],
      context = request$otel,
      meter = meter
    )
  }
}
record_request_body <- function(request, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.request.body.size",
      value = request$headers$content_length,
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}
record_response_body <- function(request, response, attributes) {
  meter <- get_meter()
  if (meter$is_enabled()) {
    otel::histogram_record(
      "http.server.response.body.size",
      value = ,
      attributes = attributes,
      context = request$otel,
      meter = meter
    )
  }
}



request_ospan <- function(request, start_time, tracer) {
  # OpenTelemetry
  # TODO: Allow server introspection of actual server host and port (network.local.address and network.local.port)
  # http.response.status_code and http.response.header.<key> can only be set later
  span <- otel::start_span(
    tolower(paste0(request$method, "_", request$path)),
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
