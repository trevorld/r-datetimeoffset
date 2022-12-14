% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invalid.r
\name{datetimeoffset-invalid}
\alias{datetimeoffset-invalid}
\alias{invalid_detect.datetimeoffset}
\alias{invalid_resolve.datetimeoffset}
\alias{invalid_any.datetimeoffset}
\alias{invalid_count.datetimeoffset}
\alias{invalid_remove.datetimeoffset}
\title{Invalid datetimeoffset datetimes}
\usage{
\method{invalid_detect}{datetimeoffset}(x)

\method{invalid_resolve}{datetimeoffset}(x, ..., invalid = "NA", nonexistent = "NA")

\method{invalid_any}{datetimeoffset}(x)

\method{invalid_count}{datetimeoffset}(x)

\method{invalid_remove}{datetimeoffset}(x)
}
\arguments{
\item{x}{A \code{\link[=datetimeoffset]{datetimeoffset()}} object.}

\item{...}{Ignored.}

\item{invalid}{Invalid date resolution strategy.  See \code{\link[clock:clock-invalid]{clock::invalid_resolve()}}.}

\item{nonexistent}{Nonexistent (because of DST spring forward) time resolution strategy.
See \code{\link[clock:as-zoned-time-naive-time]{clock::as_zoned_time.clock_naive_time()}}.}
}
\value{
\code{invalid_detect()}, \code{invalid_remove()}, and \code{invalid_resolve()} return \code{\link[=datetimeoffset]{datetimeoffset()}} vectors.
\code{invalid_count()} returns an integer and \code{invalid_any()} returns a logical value.
}
\description{
\code{invalid_detect()} detects invalid datetimes.
\code{invalid_any()} returns TRUE if any datetimes are invalid.
\code{invalid_count()} returns number of invalid datetimes.
\code{invalid_remove()} removes invalid datetimes.
\code{invalid_resolve()} resolves invalid datetimes.
}
\details{
\code{datetimeoffset()} datetimes can be considered invalid for three main reasons:
\enumerate{
\item An invalid "calendar date" such as \code{"2020-02-30"} (there are less than 30 days in February).
\item A "nonexistent" datetime due to a Daylight Savings Time "spring forward" such as \code{"2020-03-08T02:59:59[America/Los_Angeles]"}
\item Incorrect UTC offsets such as \code{"2020-03-08T01:59:59-08[America/New_York]"} (that particular Eastern time has a UTC offset of \code{-05})
}
}
\examples{
# invalid date because April only has 30 days
dts <- c("2019-04-30T03:30:00", "2019-04-31T02:30:00")
dts <- as_datetimeoffset(dts)
clock::invalid_detect(dts)
clock::invalid_any(dts)
clock::invalid_count(dts)
clock::invalid_remove(dts)
clock::invalid_resolve(dts)
clock::invalid_resolve(dts, invalid = "previous")
clock::invalid_resolve(dts, invalid = "previous-day")

# non-existent time because of DST "spring forward"
if ("America/Los_Angeles" \%in\% OlsonNames()) {
  dt <- as_datetimeoffset("2020-03-08T02:59:59[America/Los_Angeles]")
  print(clock::invalid_detect(dt))
  clock::invalid_resolve(dt, nonexistent = "roll-forward")
}

# incorrect UTC offsets
if ("America/New_York" \%in\% OlsonNames()) {
  dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/New_York]")
  print(clock::invalid_detect(dt))
  clock::invalid_resolve(dt)
}
}
