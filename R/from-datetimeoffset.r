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
#' * `as_weekday()` returns a [clock::weekday()] object
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
#'   clock::as_weekday(now)
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
as.POSIXct.datetimeoffset <- function(x, tz = mode_tz(x), ..., fill = "") {
    x <- datetime_widen.datetimeoffset(x, "nanosecond")
    x <- fill_tz(x, fill)
    zt <- as_zoned_time.datetimeoffset(x)
    as.POSIXct(format(zt, format = "%FT%H:%M:%S%z"),
               tz = tz, format = "%FT%H:%M:%OS%z")
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_date_time
#' @export
as_date_time.datetimeoffset <- function(x, zone = mode_tz(x), ..., fill = NA_character_) {
    as.POSIXct.datetimeoffset(x, tz = zone, ..., fill = fill)
}

#' @rdname from_datetimeoffset
#' @export
as.POSIXlt.datetimeoffset <- function(x, tz = mode_tz(x), ..., fill = "") {
    x <- datetime_widen.datetimeoffset(x, "nanosecond")
    x <- fill_tz(x, fill)
    zt <- as_zoned_time.datetimeoffset(x)
    as.POSIXlt(format(zt, format = "%FT%H:%M:%S%z"),
               tz = tz, format = "%FT%H:%M:%OS%z")
}

as.nanotime.datetimeoffset <- function(from, fill = NA_character_) {
    x <- datetime_widen(from, "nanosecond")
    x <- fill_tz(x, fill)
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
#' @param start The month to start the fiscal year in.
#'              See [clock::as_year_quarter_day()].
#' @importFrom clock as_year_quarter_day
#' @export
as_year_quarter_day.datetimeoffset <- function(x, ..., start = NULL) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_quarter_day(ymd, start = start)
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


as_sys_time_dto <- function(x, ...,
                            ambiguous = "error",
                            nonexistent = "error",
                            fill = NA_character_) {
    # {clock} won't convert to time point if less precise than day so make missing
    precisions <- precision_to_int(datetime_precision(x))
    is.na(x) <- ifelse(precisions < precision_to_int("day"), TRUE, FALSE)
    x <- fill_tz(x, fill)
    # {clock} doesn't allow mixed precision so standardize to widest precision
    precision <- datetime_precision(na_omit(x), range = TRUE)[2]
    if (!is.na(precision))
        x <- datetime_widen(x, precision)
    purrr::map_vec(x, as_sys_time_helper,
                   ambiguous = ambiguous, nonexistent = nonexistent)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_sys_time
#' @export
as_sys_time.datetimeoffset <- function(x) {
    # `clock::as_sys_time()` currently doesn't support `...`
    as_sys_time_dto(x)
}

as_sys_time_helper <- function(x, ambiguous = "error", nonexistent = "error") {
    if (!is.na(get_hour_offset(x))) {
        ft <- datetime_widen(x, "nanosecond")
        ft <- set_tz(ft, NA_character_)
        if (is.na(get_minute_offset(x)))
            ft <- set_minute_offset(ft, 0L)
        st <- clock::sys_time_parse(format(ft),
                                    format = "%Y-%m-%dT%H:%M:%S%Ez",
                                    precision = "nanosecond")
    } else if (!is.na(get_tz(x))) {
        nt <- as_naive_time.datetimeoffset(x)
        zt <- clock::as_zoned_time(nt, get_tz(x),
                                   ambiguous = ambiguous, nonexistent = nonexistent)
        st <- clock::as_sys_time(zt)
    } else {
        return(clock::sys_time_parse(NA_character_))
    }
    clock::time_point_floor(st, datetime_precision(x))
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_zoned_time
#' @param nonexistent What to do when the "clock time" in the new time zone doesn't exist.
#'                    See [clock::as_zoned_time.clock_naive_time()].
#' @param ambiguous What to do when the "clock time" in the new time zone is ambiguous.
#'                  See [clock::as_zoned_time.clock_naive_time()].
#' @param fill If timezone and UTC offset info is missing what
#'             timezone to assume.  See [fill_tz()].
#' @export
as_zoned_time.datetimeoffset <- function(x, zone = mode_tz(x), ...,
                                         ambiguous = "error", nonexistent = "error",
                                         fill = NA_character_) {
    # {clock} won't convert to time point if less precise than day so make missing
    precisions <- precision_to_int(datetime_precision(x))
    is.na(x) <- ifelse(precisions < precision_to_int("day"), TRUE, FALSE)
    x <- fill_tz(x, fill)
    st <- as_sys_time_dto(x, ambiguous = ambiguous, nonexistent = nonexistent)
    clock::as_zoned_time(st, zone)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_weekday
#' @export
as_weekday.datetimeoffset <- function(x) {
    clock::as_weekday(as_naive_time.datetimeoffset(x))
}
