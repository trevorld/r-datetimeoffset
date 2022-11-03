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
    precision <- precision_to_int(datetime_precision.datetimeoffset(x))
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    s <- paste0(year_str, month_str, day_str)
    format <- rep("%Y-%m-%d", length(x))
    format <- ifelse(precision < PRECISION_DAY, "%Y-%m", format)
    format <- ifelse(precision < PRECISION_MONTH, "%Y", format)
    as.Date(s, format = format)
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
    n <- max(length(from), length(tz))
    if (length(from) < n)
        from <- rep(from, length.out = n)
    if (length(tz) < n)
        tz <- rep(tz, length.out = n)
    x <- datetime_widen(from, "nanosecond")
    x <- update_missing_zone(x, tz = tz)
    s <- ifelse(is.na(from), NA_character_, format_iso8601(x))
    nanotime::as.nanotime(s)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_day
#' @export
as_year_month_day.datetimeoffset <- function(x) {
    precision <- precision_to_int(datetime_precision(x, range = TRUE)[1])
    year <- ifelse(is.na(x), NA_integer_, field(x, "year"))
    month <- if (precision >= PRECISION_MONTH) field(x, "month") else NULL
    day <- if (precision >= PRECISION_DAY) field(x, "day") else NULL
    hour <- if (precision >= PRECISION_HOUR) field(x, "hour") else NULL
    minute <- if (precision >= PRECISION_MINUTE) field(x, "minute") else NULL
    second <- if (precision >= PRECISION_SECOND) field(x, "second") else NULL
    if (precision >= PRECISION_NANOSECOND) {
        subsecond <- field(x, "nanosecond")
        subsecond_precision <- "nanosecond"
    } else {
        subsecond <- NULL
        subsecond_precision <- NULL
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
    # {clock} won't convert to time point if less precise than day so make missing
    precisions <- precision_to_int(datetime_precision(x))
    is.na(x) <- ifelse(precisions < precision_to_int("day"), TRUE, FALSE)
    # {clock} doesn't allow mixed precision so standardize to narrowest precision
    precision <- datetime_precision(na_omit(x), range = TRUE)[1]
    if (!is.na(precision))
        x <- datetime_narrow(x, precision)
    purrr::map_vec(x, as_sys_time_helper)
}

as_sys_time_helper <- function(x) {
    if (is.na(x))
        return(clock::sys_time_parse(NA_character_))
    if (!is.na(get_hour_offset(x))) {
        ft <- datetime_widen(x, "nanosecond")
        if (is.na(get_minute_offset(x)))
            ft <- set_minute_offset(ft, 0L)
        st <- clock::sys_time_parse(format(ft),
                                    format = "%Y-%m-%dT%H:%M:%S%Ez",
                                    precision = "nanosecond")
    } else if (!is.na(get_tz(x))) {
        nt <- as_naive_time.datetimeoffset(x)
        zt <- clock::as_zoned_time(nt, get_tz(x),
                                   ambiguous = "error", nonexistent = "error")
        st <- clock::as_sys_time(zt)
    } else {
        ymd <- as_year_month_day.datetimeoffset(x)
        st <- clock::as_sys_time(ymd)
    }
    clock::time_point_floor(st, datetime_precision(x))
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_zoned_time
#' @export
as_zoned_time.datetimeoffset <- function(x, zone = mode_tz(x)) {
    x <- update_missing_zone(x, tz = zone)
    # {clock} won't convert to time point if less precise than day so make missing
    precisions <- precision_to_int(datetime_precision(x))
    is.na(x) <- ifelse(precisions < precision_to_int("day"), TRUE, FALSE)
    st <- as_sys_time.datetimeoffset(x)
    clock::as_zoned_time(st, zone)
}
