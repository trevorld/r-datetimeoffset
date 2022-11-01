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
#' * `as_year_day()` returns a [clock::year_day()] calendar
#' * `as_naive_time()` returns a "clock" naive-time
#' * `as_sys_time()` returns a "clock" sys-time
#' * `as_zoned_time()` returns a "clock" zoned-time
#'
#' @param x A [datetimeoffset()] object
#' @param tz,zone   What time zone to assume
#' @param ... Ignored
#' @name from_datetimeoffset
#' @examples
#'   # {base}
#'   today <- as_datetimeoffset(Sys.Date())
#'   now <- as_datetimeoffset(Sys.time())
#'
#'   as.Date(today)
#'   as.Date(now)
#'   as.POSIXct(now)
#'   as.POSIXlt(now)
#'
#'   # {clock}
#'   clock::as_date(today)
#'   clock::as_date_time(now)
#'
#'   clock::as_year_month_day(now)
#'   clock::as_year_month_weekday(now)
#'   clock::as_iso_year_week_day(now)
#'   clock::as_year_quarter_day(now)
#'   clock::as_year_day(now)
#'
#'   clock::as_naive_time(now)
#'   clock::as_sys_time(now)
#'   clock::as_zoned_time(now)
#'
#'   if (require("nanotime")) {
#'     nanotime::as.nanotime(now)
#'   }
NULL

#' @rdname from_datetimeoffset
#' @export
as.Date.datetimeoffset <- function(x, ...) {
    x <- datetime_widen(x, "day")
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
as.POSIXct.datetimeoffset <- function(x, tz = mode_tz(x), ...) {
    as.POSIXct(as_zoned_time.datetimeoffset(x, tz))
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_date_time
#' @export
as_date_time.datetimeoffset <- function(x, zone = mode_tz(x), ...) {
    as.POSIXct.datetimeoffset(x, tz = zone, ...)
}

#' @rdname from_datetimeoffset
#' @export
as.POSIXlt.datetimeoffset <- function(x, tz = mode_tz(x), ...) {
    as.POSIXlt(as_zoned_time.datetimeoffset(x, tz))
}

as.nanotime.datetimeoffset <- function(from, tz = "") {
                x <- datetime_widen(from, "nanosecond")
                x <- update_missing_zone(x, tz = tz)
                nanotime::as.nanotime(format_iso8601(x))
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_day
#' @export
as_year_month_day.datetimeoffset <- function(x) {
    # coerce to at least "year" then use "minimum" precision
    x <- datetime_widen(x, "year")
    precision <- datetime_precision(x, range = TRUE)[1]
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
#' @importFrom clock as_naive_time
#' @export
as_naive_time.datetimeoffset <- function(x) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_naive_time(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_sys_time
#' @export
as_sys_time.datetimeoffset <- function(x) {
    precision <- datetime_precision(x, range = TRUE)[1]
    x <- datetime_narrow(x, precision)
    purrr::map_vec(x, as_sys_time_helper)
}

as_sys_time_helper <- function(x) {
    if (!is.na(get_hour_offset(x))) {
        ft <- datetime_widen(x, "nanosecond")
        if (is.na(get_minute_offset(x)))
            ft <- set_minute_offset(ft, 0L)
        st <- clock::sys_time_parse(format(ft),
                                    format = "%Y-%m-%dT%H:%M:%S%Ez",
                                    precision = "nanosecond")
        clock::time_point_floor(st, datetime_precision(x))
    } else if (!is.na(get_tz(x))) {
        nt <- as_naive_time.datetimeoffset(x)
        zt <- clock::as_zoned_time(nt, get_tz(x),
                                   ambiguous = "error", nonexistent = "error")
        st <- clock::as_sys_time(zt)
        clock::time_point_floor(st, datetime_precision(x))
    } else {
        ymd <- as_year_month_day.datetimeoffset(x)
        clock::as_sys_time(ymd)
    }
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_zoned_time
#' @export
as_zoned_time.datetimeoffset <- function(x, zone = mode_tz(x)) {
    precision <- datetime_precision(x, range = TRUE)[1]
    x <- datetime_narrow(x, precision)
    x <- update_missing_zone(x, tz = zone)
    st <- as_sys_time.datetimeoffset(x)
    clock::as_zoned_time(st, zone)
}
