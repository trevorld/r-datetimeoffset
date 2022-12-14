% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractors.r
\name{weekdays.datetimeoffset}
\alias{weekdays.datetimeoffset}
\alias{months.datetimeoffset}
\alias{quarters.datetimeoffset}
\alias{julian.datetimeoffset}
\title{Additional datetime extractors}
\usage{
\method{weekdays}{datetimeoffset}(x, abbreviate = FALSE)

\method{months}{datetimeoffset}(x, abbreviate = FALSE)

\method{quarters}{datetimeoffset}(x, ...)

\method{julian}{datetimeoffset}(x, origin = as.Date("1970-01-01"), ...)
}
\arguments{
\item{x}{A \code{\link[=datetimeoffset]{datetimeoffset()}} datetime}

\item{abbreviate}{Logical vector for whether the names should be abbreviated}

\item{...}{Ignored}

\item{origin}{Length one datetime of origin}
}
\value{
\code{weekdays()}, \code{months()}, \code{quarters()}, \code{julian()} return character vectors.
See \code{\link[base:weekday.POSIXt]{base::weekdays()}} for more information.
}
\description{
Additional datetime extractors for \code{\link[=datetimeoffset]{datetimeoffset()}} objects.
}
\details{
We implement \code{\link[=datetimeoffset]{datetimeoffset()}} support for the following S3 methods from \code{base}:
\itemize{
\item \code{weekdays()}
\item \code{months()}
\item \code{quarters()}
\item \code{julian()}
}

There is also \code{\link[=datetimeoffset]{datetimeoffset()}} support for the following methods from \code{lubridate}:
\itemize{
\item \code{isoyear()} and \code{epiyear()}
\item \code{quarter()} and \code{semester()}
\item \code{week()}, \code{isoweek()}, and \code{epiweek()}
\item \code{wday()} and \verb{wday<-()}
\item \code{qday()} and \verb{qday<-()}
\item \code{yday()} and \verb{yday<-()}
\item \code{am()} and \code{pm()}
\item \code{days_in_month()}
\item \code{dst()}
\item \code{leap_year()}
}
}
\examples{
dto <- datetimeoffset_now()
print(dto)
weekdays(dto)
months(dto)
quarters(dto)
julian(dto)

if (require("lubridate")) {
  cat("`isoyear(dto)`: ", isoyear(dto), "\n")
  cat("`epiyear(dto)`: ", epiyear(dto), "\n")
  cat("`semester(dto)`: ", semester(dto), "\n")
  cat("`quarter(dto)`: ", quarter(dto), "\n")
  cat("`week(dto)`: ", week(dto), "\n")
  cat("`isoweek(dto)`: ", isoweek(dto), "\n")
  cat("`epiweek(dto)`: ", epiweek(dto), "\n")
  cat("`wday(dto)`: ", wday(dto), "\n")
  cat("`qday(dto)`: ", qday(dto), "\n")
  cat("`yday(dto)`: ", yday(dto), "\n")
  cat("`am(dto)`: ", am(dto), "\n")
  cat("`pm(dto)`: ", pm(dto), "\n")
  cat("`days_in_month(dto)`: ", days_in_month(dto), "\n")
  cat("`dst(dto)`: ", dst(dto), "\n")
  cat("`leap_year(dto)`: ", leap_year(dto), "\n")
}

}
