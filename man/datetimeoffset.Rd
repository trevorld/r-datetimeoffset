% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetimeoffset.r
\name{datetimeoffset}
\alias{datetimeoffset}
\title{Datetime object with optional UTC offsets and/or timezones}
\usage{
datetimeoffset(
  year = NA_integer_,
  month = NA_integer_,
  day = NA_integer_,
  hour = NA_integer_,
  minute = NA_integer_,
  second = NA_integer_,
  nanosecond = NA_integer_,
  subsecond_digits = NA_integer_,
  hour_offset = NA_integer_,
  minute_offset = NA_integer_,
  tz = NA_character_
)
}
\arguments{
\item{year}{Year (integer, optional)}

\item{month}{Month (integer, optional)}

\item{day}{Day (integer, optional)}

\item{hour}{Hour (integer, optional)}

\item{minute}{Minute (integer, optional)}

\item{second}{Second (integer, optional)}

\item{nanosecond}{Nanosecond (integer, optional)}

\item{subsecond_digits}{Number of digits used by fractional seconds (integer, optional)}

\item{hour_offset}{UTC offset in hours (integer, optional)}

\item{minute_offset}{UTC offset in minutes (integer, optional).
Will be coerced to a non-negative value.}

\item{tz}{Time zone (character, optional)}
}
\value{
A \code{vctrs} record with class \code{datetimeoffset}.
}
\description{
\code{datetimeoffset()} creates a datetime with possible UTC offset object.
It can be used to represent datetimes with possible UTC offsets
(without necessarily any knowledge of the time zone).
}
\examples{
  datetimeoffset(2020)
  datetimeoffset(2020, 5)
  datetimeoffset(2020, 5, 15)
  datetimeoffset(2020, 5, 15, 8)
  datetimeoffset(2020, 5, 15, 8, 23)
  datetimeoffset(2020, 5, 15, 8, 23, 16) # local time with unknown timezone
  if ("US/Pacific" \%in\% OlsonNames())
    datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "US/Pacific")
  datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "GMT")
  datetimeoffset(2020, 5, 15, 8, 23, 16, hour_offset = -7)
  datetimeoffset(2020, 5, 15, 8, 23, 16, hour_offset = -7, minute_offset = 30)
}
