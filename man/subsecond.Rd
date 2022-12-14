% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsecond.r
\name{subsecond}
\alias{subsecond}
\alias{get_millisecond.datetimeoffset}
\alias{set_millisecond.datetimeoffset}
\alias{get_microsecond.datetimeoffset}
\alias{set_microsecond.datetimeoffset}
\alias{get_subsecond}
\alias{get_subsecond.datetimeoffset}
\alias{set_subsecond}
\alias{set_subsecond.datetimeoffset}
\title{Subsecond helper getter/setter}
\usage{
\method{get_millisecond}{datetimeoffset}(x)

\method{set_millisecond}{datetimeoffset}(x, value, ..., na_set = FALSE, digits = 3L)

\method{get_microsecond}{datetimeoffset}(x)

\method{set_microsecond}{datetimeoffset}(x, value, ..., na_set = FALSE, digits = 6L)

get_subsecond(x, ...)

\method{get_subsecond}{datetimeoffset}(x, digits = get_subsecond_digits(x), ...)

set_subsecond(x, value, digits = 1L, ...)

\method{set_subsecond}{datetimeoffset}(x, value, digits = 1L, ..., na_set = FALSE)
}
\arguments{
\item{x}{A datetime object.}

\item{value}{The replacement value.  For \code{set_day()} this can also be "last".}

\item{...}{Currently ignored.}

\item{na_set}{If \code{TRUE} set component for \code{NA} datetimes (making them no longer \code{NA})}

\item{digits}{If \code{NULL} do not update the \code{subsecond_digits} field.
Otherwise an integer vector (\code{1L} through \code{9L} or \code{NA_integer_})
to update the \code{subsecond_digits} field with.}
}
\value{
\code{get_millisecond()}, \code{get_microsecond()}, and \code{get_subsecond()} returns an integer vector.
\code{set_millisecond()}, \code{set_microsecond()}, and \code{set_subsecond()} returns a datetime vector.
}
\description{
Helper getter/setter methods for the subseconds (aka fractional seconds) of \code{\link[=datetimeoffset]{datetimeoffset()}} objects.
}
\details{
Internally \code{\link[=datetimeoffset]{datetimeoffset()}} objects represent subseconds with two fields:
\enumerate{
\item Nanoseconds (as an integer)
\item Number of subsecond digits (as an integer)
}

One can explicitly get/set these fields with
\itemize{
\item \code{get_nanosecond()} / \code{set_nanosecond()}
\item \code{get_subsecond_digits()} / \code{set_subsecond_digits()}
}

We implement \code{\link[=datetimeoffset]{datetimeoffset()}} support for the following S3 methods from \code{clock}:
\itemize{
\item \code{get_millisecond()}
\item \code{get_microsecond()}
\item \code{set_millisecond()} (note sets any non-zero microsecond/nanosecond elements to zero)
\item \code{set_microsecond()} (note sets any non-zero nanosecond elements to zero)
}

We implement the following new S3 methods:
\itemize{
\item \code{get_subsecond()}
\item \code{set_subsecond()}
}
}
\examples{
library("clock")
dt <- as_datetimeoffset("2020-01-01T10:10:10.123456789")
format(dt)
get_millisecond(dt)
get_microsecond(dt)
get_subsecond(dt, 1L)
get_subsecond(dt, 7L)

set_microsecond(dt, 123456L)
set_millisecond(dt, 123L)
set_subsecond(dt, 12L, digits = 2L)
set_subsecond(dt, 12L, digits = 3L)

}
