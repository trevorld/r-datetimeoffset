#' Convert to other datetime objects
#'
#' We register S3 methods to convert [datetimeoffset()] objects to
#' other R datetime objects:
#'
#' We register S3 methods for the following:
#'
#' * [as.Date()] and [clock::as_date()] returns the "local" date as a [base::Date()] object
#' * [as.POSIXct()] and [clock::as_date_time()] returns the "local" datetime as a [base::POSIXct()] object
#' * [as.POSIXlt()] returns the "local" datetime as a [base::POSIXlt()] object
#' * [nanotime::as.nanotime()] returns the "global" datetime as a [nanotime::nanotime()] object
#' * [parttime::as.parttime()] returns the "local" datetime as a [parttime::parttime()] object
#' * [clock::as_year_month_day()] returns a [clock::year_month_day()] calendar
#' * [clock::as_year_month_weekday()] returns a [clock::year_month_weekday()] calendar
#' * [clock::as_iso_year_week_day()] returns a [clock::iso_year_week_day()] calendar
#' * [clock::as_year_quarter_day()] returns a [clock::year_quarter_day()] calendar
#' * [clock::as_year_day()] returns a [clock::year_day()] calendar
#' * [clock::as_naive_time()] returns a "clock" naive-time
#' * [clock::as_sys_time()] returns a "clock" sys-time
#' * [clock::as_zoned_time()] returns a "clock" zoned-time
#' * [clock::as_weekday()] returns a [clock::weekday()] object
#'
#' @param x A [datetimeoffset()] object
#' @param tz,zone   What time zone to assume
#' @param ... Ignored
#' @return A datetime object vector
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
#'   if (requireNamespace("nanotime")) {
#'     nanotime::as.nanotime(now)
#'   }
#'
#'   if (requireNamespace("parttime")) {
#'     parttime::as.parttime(now)
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
as_date.datetimeoffset <- function(x, ...) {
    as.Date.datetimeoffset(x)
}

#' @rdname from_datetimeoffset
#' @export
as.POSIXct.datetimeoffset <- function(x, tz = mode_tz(x), ..., fill = "") {
    x <- datetime_widen.datetimeoffset(x, "microsecond")
    x <- fill_tz(x, fill)
    purrr::map_vec(x, as_posixt_helper, tz = tz, f = as.POSIXct)
}

as_posixt_helper <- function(x, tz, f) {
    if (is.na(x)) return (f(NA_character_))
    years <- get_year.datetimeoffset(x)
    secs <- get_second.datetimeoffset(x)
    if (years < 0L || years > 9999L) { # Can't use `as.POSIXt.character()` for small/large years
        zt <- as_zoned_time.datetimeoffset(x)
        lt <- as.POSIXlt(zt, tz = tz)
        lt[, "sec"] <- lt[, "sec"] + get_microsecond.datetimeoffset(x) / 1e6
        f(lt)
    } else if (secs < 60L) { # {clock} doesn't handle leap seconds
        zt <- as_zoned_time.datetimeoffset(x)
        f(format(zt, format = "%FT%H:%M:%S%z"),
          tz = tz, format = "%FT%H:%M:%OS%z")
    } else {
        x_tz <- get_tz(x)
        if (!is.na(x_tz)) {
            s <- format_iso8601(x, offsets = FALSE)
            dt <- f(s, tz = x_tz, format = "%FT%H:%M:%OS")
            if (tz != x_tz) {
                if (!inherits(dt, "POSIXct"))
                    dt <- as.POSIXct(dt)
                attr(dt, "tzone") <- tz
                f(dt)
            } else {
                dt
            }
        } else {
            utc_offsets <- get_utc_offsets(x, sep = "")
            s <- paste0(format_iso8601(x, offsets = FALSE), utc_offsets)
            f(s, tz = tz, format = "%FT%H:%M:%OS%z")
        }
    }
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
    x <- datetime_widen.datetimeoffset(x, "microsecond")
    x <- fill_tz(x, fill)
    purrr::map_vec(x, as_posixt_helper, tz = tz, f = as.POSIXlt)
}

as.nanotime.datetimeoffset <- function(from, fill = NA_character_) {
    x <- datetime_widen(from, "nanosecond")
    x <- fill_tz(x, fill)
    s <- ifelse(is.na(from), NA_character_, format_iso8601(x))
    nanotime::as.nanotime(s)
}

# Supports `parttime::as.parttime()`
vec_cast.partial_time.datetimeoffset <- function(x, to, ...) {
    x <- fill_utc_offsets(x)
    year <- as.numeric(field(x, "year"))
    month <- as.numeric(field(x, "month"))
    day <- as.numeric(field(x, "day"))
    hour <- as.numeric(field(x, "hour"))
    min <- as.numeric(field(x, "minute"))
    ns <- 1e-9 * ifelse(is.na(field(x, "nanosecond")), 0, field(x, "nanosecond"))
    sec <- as.numeric(field(x, "second")) + ns
    mo <- sign(field(x, "hour_offset")) * field(x, "minute_offset") / 60.0
    tz <- as.numeric(field(x, "hour_offset")) + mo
    parttime::parttime(year, month, day, hour, min, sec, tz)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_day
#' @export
as_year_month_day.datetimeoffset <- function(x, ...) {
    precision <- precision_to_int(datetime_precision(x, range = TRUE)[1])
    year <- ifelse(is.na(x), NA_integer_, field(x, "year"))
    month <- if (precision >= PRECISION_MONTH) field(x, "month") else NULL
    day <- if (precision >= PRECISION_DAY) field(x, "day") else NULL
    hour <- if (precision >= PRECISION_HOUR) field(x, "hour") else NULL
    minute <- if (precision >= PRECISION_MINUTE) field(x, "minute") else NULL
    second <- if (precision >= PRECISION_SECOND) field(x, "second") else NULL
    if (precision > PRECISION_MICROSECOND) {
        subsecond <- field(x, "nanosecond")
        subsecond_precision <- "nanosecond"
    } else if (precision > PRECISION_MILLISECOND) {
        subsecond <- get_microsecond(x)
        subsecond_precision <- "microsecond"
    } else if (precision > PRECISION_SECOND) {
        subsecond <- get_millisecond(x)
        subsecond_precision <- "millisecond"
    } else {
        subsecond <- NULL
        subsecond_precision <- NULL
    }
    # {clock} doesn't handle leap seconds, use next second like `POSIXct` or `nanotime`
    idx <- which(second == 60L)
    if (length(idx) > 0L)
        second[idx] <- 59L
    ymd <- clock::year_month_day(year, month, day, hour, minute, second, subsecond,
                                 subsecond_precision = subsecond_precision)
    if (length(idx) > 0L) {
        ymd[idx] <- clock::as_year_month_day(clock::add_seconds(clock::as_naive_time(ymd[idx]), 1L))
    }
    ymd
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_year_month_weekday
#' @export
as_year_month_weekday.datetimeoffset <- function(x, ...) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_month_weekday(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_iso_year_week_day
#' @export
as_iso_year_week_day.datetimeoffset <- function(x, ...) {
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
as_year_day.datetimeoffset <- function(x, ...) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_year_day(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_naive_time
#' @export
as_naive_time.datetimeoffset <- function(x, ...) {
    ymd <- as_year_month_day.datetimeoffset(x)
    clock::as_naive_time(ymd)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_sys_time
#' @export
as_sys_time.datetimeoffset <- function(x, ...,
                            ambiguous = "error",
                            nonexistent = "error",
                            fill = NA_character_) {
    # {clock} won't convert to time point if less precise than day so make missing
    precisions <- precision_to_int(datetime_precision(x))
    is.na(x) <- ifelse(precisions < precision_to_int("day"), TRUE, FALSE)
    x <- fill_tz(x, fill)
    # {clock} doesn't allow mixed precision so standardize to widest precision
    precision <- datetime_precision(na_omit(x), range = TRUE)[2]
    precision_int <- precision_to_int(precision)
    if (!is.na(precision)) {
        # {clock} only supports "millisecond", "microsecond", "nanosecond" subsecond precisions
        if (precision_int > PRECISION_MICROSECOND) {
            precision <- "nanosecond"
        } else if (precision_int > PRECISION_MILLISECOND) {
            precision <- "microsecond"
        } else if (precision_int > PRECISION_SECOND) {
            precision <- "millisecond"
        }
        x <- datetime_widen(x, precision)
    }
    purrr::map_vec(x, as_sys_time_helper,
                   ambiguous = ambiguous, nonexistent = nonexistent)
}

as_sys_time_helper <- function(x, ambiguous = "error", nonexistent = "error") {
    if (!is.na(get_hour_offset(x))) {
        ft <- datetime_widen(x, "nanosecond")
        ft <- set_tz(ft, NA_character_)
        if (is.na(get_minute_offset(x)))
            ft <- set_minute_offset(ft, 0L)
        # {clock} doesn't handle leap seconds, use next second like `POSIXct` or `nanotime`
        idx <- which(get_second(ft) == 60L)
        if (length(idx) > 0L)
            ft[idx] <- set_second(ft[idx], 59L)
        st <- clock::sys_time_parse(format(ft),
                                    format = "%Y-%m-%dT%H:%M:%S%Ez",
                                    precision = "nanosecond")
        if (length(idx) > 0L) {
            st[idx] <- clock::add_seconds(st[idx], 1L)
        }
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
    st <- as_sys_time.datetimeoffset(x, ambiguous = ambiguous, nonexistent = nonexistent)
    clock::as_zoned_time(st, zone)
}

#' @rdname from_datetimeoffset
#' @importFrom clock as_weekday
#' @export
as_weekday.datetimeoffset <- function(x, ...) {
    clock::as_weekday(as_naive_time.datetimeoffset(x))
}
