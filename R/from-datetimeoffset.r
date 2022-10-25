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
#' * `as_year_month_day()` returns a [clock::year_month_day()] calendar
#' * `as_year_month_weekday()` returns a [clock::year_month_weekday()] calendar
#' * `as_iso_year_week_day()` returns a [clock::iso_year_week_day()] calendar
#' * `as_year_quarter_day()` returns a [clock::year_quarter_day()] calendar
#' * `as_year_day()` returns a "clock" [clock::year_day()] calendar
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
#'   today <- as_datetimeoffset(Sys.Date())
#'   now <- as_datetimeoffset(Sys.time())
#'
#'   as.Date(as_datetimeoffset("2020-03-05"))
#'   as.Date(as_datetimeoffset("2020"))
#'   as.Date(as_datetimeoffset("2020"), month = 6, day = 15)
#'   as.POSIXct(now)
#'   as.POSIXlt(now)
#'
#'   # {clock}
#'   clock::as_date(today)
#'   clock::as_date_time(now)
#'   clock::as_year_month_day(now)
#'   clock::as_year_month_weekday(now)
#'   clock::as_iso_year_week_day(now)
#'   clock::as_year_quarter_day(now)
#'   clock::as_year_day(now)
#'
#'   if (require("nanotime")) {
#'     nanotime::as.nanotime(now)
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
    #### #22
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
    #### #22
    assert_suggested("nanotime")
    as.POSIXlt(nanotime::as.nanotime(x, ...,
                           year = year, month = month, day = day,
                           hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                           tz = tz),
               tz = tz)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_day
#' @export
as_year_month_day.datetimeoffset <- function(x) {
    # coerce to at least "year" then use "minimum" precision
    x <- calendar_widen(x, "year")
    precision <- calendar_precision(x, range = TRUE)[1]
    precision <- factor(precision, c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
    precision <- as.integer(precision)
    year <- field(x, "year")
    month <- NULL
    day <- NULL
    hour <- NULL
    minute <- NULL
    second <- NULL
    subsecond <- NULL
    subsecond_precision <- NULL
    if (precision >= 2L) month <- field(x, "month")
    if (precision >= 3L) day <- field(x, "day")
    if (precision >= 4L) hour <- field(x, "hour")
    if (precision >= 5L) minute <- field(x, "minute")
    if (precision >= 6L) second <- field(x, "second")
    if (precision >= 7L) {
        subsecond <- field(x, "nanosecond")
        subsecond_precision <- "nanosecond"
    }
    clock::year_month_day(year, month, day, hour, minute, second, subsecond,
                          subsecond_precision = subsecond_precision)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_weekday
#' @export
as_year_month_weekday.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_month_weekday(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_iso_year_week_day
#' @export
as_iso_year_week_day.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_iso_year_week_day(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_quarter_day
#' @export
as_year_quarter_day.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_quarter_day(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_day
#' @export
as_year_day.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_day(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_sys_time
#' @export
as_sys_time.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_sys_time(ymd)
}
