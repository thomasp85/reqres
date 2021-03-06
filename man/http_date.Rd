% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\name{to_http_date}
\alias{to_http_date}
\alias{from_http_date}
\title{Format timestamps to match the HTTP specs}
\usage{
to_http_date(time, format = NULL)

from_http_date(time)
}
\arguments{
\item{time}{A string or an object coercible to POSIXct}

\item{format}{In case \code{time} is not a POSIXct object a specification how the
string should be interpreted.}
}
\value{
\code{to_http_date()} returns a properly formatted string, while
\code{from_http_date()} returns a POSIXct object
}
\description{
Dates/times in HTTP headers needs a specific format to be valid, and is
furthermore always given in GMT time. These two functions aids in converting
back and forth between the required format.
}
\examples{
time <- to_http_date(Sys.time())
time
from_http_date(time)
}
