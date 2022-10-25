#' Convert to other datetime objects
#'
#' We provide methods to convert [datetimeoffset()] objects to
#' other R datetime objects:
#'
#' We provide the following methods:
#'
#' * `as.Date()` and `as_date()` returns the "local" date as a [base::Date()] object
#' * `as.POSIXct()` and `as_date_time()` returns the "local" datetime as a [base::POSIXct()] object
#' * `as.POSIXlt()` returns the "local" datetime as a [base::POSIXlt()] object
#' * `as.nanotime()` returns the "global" datetime as a [nanotime::nanotime()] object
#'
#' @param x A [datetimeoffset()] object
#' @param year If missing what year to assume
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param hour   If missing what hour to assume
#' @param minute   If missing what minute to assume
#' @param second   If missing what second to assume
#' @param nanosecond   If missing what nanosecond to assume
#' @param tz,zone   If missing and hour offset also missing what time zone to assume
#' @param ... Ignored
#' @name from_datetimeoffset
#' @examples
#'   # {base}
#'   as.Date(as_datetimeoffset("2020-03-05"))
#'   as.Date(as_datetimeoffset("2020"))
#'   as.Date(as_datetimeoffset("2020"), month = 6, day = 15)
#'   as.POSIXct(as_datetimeoffset(Sys.time()))
#'   as.POSIXlt(as_datetimeoffset(Sys.time()))
#'
#'   # {clock}
#'   clock::as_date(as_datetimeoffset(Sys.Date()))
#'   clock::as_date_time(as_datetimeoffset(Sys.time()))
#'
#'   if (require("nanotime")) {
#'     nanotime::as.nanotime(as_datetimeoffset(Sys.time()))
#'   }
NULL

#' @rdname from_datetimeoffset
#' @export
as.Date.datetimeoffset <- function(x, ..., year = 0L, month = 1L, day = 1L) {
    x <- calendar_widen(x, "day", year = year, month = month, day = day)

    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    s <- paste0(year_str, month_str, day_str)
    as.Date(s)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_date
#' @export
as_date.datetimeoffset <- function(x) {
    as.Date.datetimeoffset(x)
}


#' @rdname from_datetimeoffset
#' @export
as.POSIXct.datetimeoffset <- function(x, tz = mode_tz(x), ...,
                                       year = 0L, month = 1L, day = 1L,
                                       hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    assert_suggested("nanotime")
    as.POSIXct(nanotime::as.nanotime(x, ...,
                           year = year, month = month, day = day,
                           hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                           tz = tz),
               tz = tz)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_date_time
#' @export
as_date_time.datetimeoffset <- function(x, zone = mode_tz(x), ...,
                                       year = 0L, month = 1L, day = 1L,
                                       hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    as.POSIXct.datetimeoffset(x, tz = zone, ...,
                              year = year, month = month, day = day,
                              hour = hour, minute = minute, second = second, nanosecond = nanosecond)
}

#' @rdname from_datetimeoffset
#' @export
as.POSIXlt.datetimeoffset <- function(x, tz = mode_tz(x), ...,
                                       year = 0L, month = 1L, day = 1L,
                                       hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    assert_suggested("nanotime")
    as.POSIXlt(nanotime::as.nanotime(x, ...,
                           year = year, month = month, day = day,
                           hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                           tz = tz),
               tz = tz)
}
