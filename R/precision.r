#' Datetime precision
#'
#' `datetime_precision()` returns the "precision" of a datetime vector's datetimes.
#'
#' @param x A datetime vector.  Either [datetimeoffset()], a "clock" "calendar", or a "clock" "time".
#' @param range If `TRUE` return just the minimum and maximum "precision".
#' @param unspecified If `TRUE` use the smallest non-missing component's as the precision even
#'                    if there is a missing value for a larger component.
#' @param ... Reserved for other methods.
#' @return A character vector of precisions ("year", "month", "day", "hour", "minute", "second", or "nanosecond").
#' @examples
#'   dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
#'   datetime_precision(dts)
#'   datetime_precision(dts, range = TRUE)
#'
#'   dt <- datetimeoffset(2020, NA_integer_, 10)
#'   datetime_precision(dt)
#'   datetime_precision(dt, unspecified = TRUE)
#'
#'   library("clock")
#'   datetime_precision(year_month_day(1918, 11, 11))
#'   datetime_precision(sys_time_now())
#'   datetime_precision(zoned_time_now(Sys.timezone()))
#' @name datetime_precision
NULL

#' @rdname datetime_precision
#' @export
datetime_precision <- function(x, ...) {
    UseMethod("datetime_precision")
}

#' @rdname datetime_precision
#' @export
datetime_precision.datetimeoffset <- function(x, range = FALSE, unspecified = FALSE,...) {
    if (length(x) == 0L) {
        if (range)
            return(c(NA_character_, NA_character_))
        else
            return(character())
    }
    if (unspecified) {
        precision <- rep_len("missing", length(x))
        for (component in c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
            precision <- ifelse(!is.na(field(x, component)), component, precision)
    } else {
        precision <- rep_len("nanosecond", length(x))
        precision <- ifelse(is.na(field(x, "nanosecond")), "second", precision)
        precision <- ifelse(is.na(field(x, "second")), "minute", precision)
        precision <- ifelse(is.na(field(x, "minute")), "hour", precision)
        precision <- ifelse(is.na(field(x, "hour")), "day", precision)
        precision <- ifelse(is.na(field(x, "day")), "month", precision)
        precision <- ifelse(is.na(field(x, "month")), "year", precision)
        precision <- ifelse(is.na(field(x, "year")), "missing", precision)
    }
    if (range) {
        precision <- dto_precision_integer(precision)
        c("missing", "year", "month", "day", "hour", "minute", "second", "nanosecond")[range(precision)]
    } else {
        precision
    }
}

#' @rdname datetime_precision
#' @export
datetime_precision.clock_calendar <- function(x, ...) {
    clock::calendar_precision(x)
}

#' @rdname datetime_precision
#' @export
datetime_precision.clock_time_point <- function(x, ...) {
    clock::time_point_precision(x)
}

#' @rdname datetime_precision
#' @export
datetime_precision.clock_zoned_time <- function(x, ...) {
    clock::zoned_time_precision(x)
}

#' Widen/narrow datetime precision
#'
#' `datetime_widen()` sets a floor on the minimum "precision" in the datetime vector
#'  by setting any missing elements to their minimum possible value.
#' `datetime_narrow()` sets a cap on the maximum "precision" by setting
#' any more precise elements missing.
#'
#' @param x A datetime vector.  Either [datetimeoffset()], a "clock" "calendar", or a "clock" "time point".
#' @param precision Precision to narrow/widen to.  Either "missing", "year", "month", "day", "hour", "minute", "second", or "nanosecond".
#' @param ... Reserved for other methods.
#' @return A datetime vector.
#' @examples
#'   dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
#'   datetime_precision(dts)
#'   datetime_narrow(dts, "day")
#'   datetime_widen(dts, "day")
#'   datetime_widen(dts, "day", month = 6, day = 15)
#'
#'   # vectorized "precision" is allowed
#'   datetime_narrow(as_datetimeoffset(Sys.time()),
#'                   c("year", "month", "day"))
#'   datetime_widen(NA_datetimeoffset_, c("year", "month", "day"))
#'
#'   library("clock")
#'   ymd <- year_month_day(1918, 11, 11, 11)
#'   datetime_narrow(ymd, "day")
#'   datetime_narrow(ymd, "second") # already narrower than "second"
#'   datetime_widen(ymd, "second")
#'   datetime_widen(ymd, "day") # already wider than "day"
#'
#'   \dontrun{
#'     # these equivalent {clock} calendar methods throw an error
#'     clock::calendar_narrow(ymd, "second") # already narrower than "second"
#'     clock::calendar_widen(ymd, "day") # already wider than "day"
#'   }
#'
#'   nt <- as_naive_time(ymd)
#'   datetime_narrow(nt, "day")
#'   datetime_narrow(ymd, "second")
#'   datetime_widen(nt, "second")
#'   datetime_widen(ymd, "day")
#' @name datetime_cast
NULL

#' @rdname datetime_cast
#' @export
datetime_narrow <- function(x, precision, ...) {
    UseMethod("datetime_narrow")
}

#' @rdname datetime_cast
#' @export
datetime_narrow.datetimeoffset <- function(x, precision, ...) {
    precision <- dto_precision_integer(precision)
    n <- max(length(x), length(precision))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(precision) < n)
        precision <- rep(precision, length.out = n)
    nas <- rep_len(NA_integer_, n)
    for (component in c("nanosecond", "second", "minute", "hour", "day", "month", "year"))
        field(x, component) <- ifelse(precision < dto_precision_integer(component),
                                      nas,
                                      field(x, component))
    x
}

#' @rdname datetime_cast
#' @export
datetime_narrow.clock_calendar <- function(x, precision, ...) {
    old_precision <- clock_precision_integer(clock::calendar_precision(x))
    new_precision <- clock_precision_integer(precision)
    if (old_precision <= new_precision)
        x
    else
        clock::calendar_narrow(x, precision)
}

#' @rdname datetime_cast
#' @export
datetime_narrow.clock_time_point <- function(x, precision, ...) {
    old_precision <- clock_precision_integer(clock::time_point_precision(x))
    new_precision <- clock_precision_integer(precision)
    if (old_precision <= new_precision)
        x
    else
        clock::time_point_floor(x, precision)
}

#' @rdname datetime_cast
#' @param ... Used by certain methods
#' @export
datetime_widen <- function(x, precision, ...) {
    UseMethod("datetime_widen")
}

#' @rdname datetime_cast
#' @param year If missing what year to assume
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param hour   If missing what hour to assume
#' @param minute   If missing what minute to assume
#' @param second   If missing what second to assume
#' @param nanosecond   If missing what nanosecond to assume
#' @export
datetime_widen.datetimeoffset <- function(x, precision, ...,
                                          year = 0L, month = 1L, day = 1L,
                                          hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    precision <- dto_precision_integer(precision)
    n <- max(length(x), length(precision))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(precision) < n)
        precision <- rep(precision, length.out = n)
    for (component in c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
        field(x, component) <- ifelse(precision >= dto_precision_integer(component),
                                      update_missing(field(x, component), get(component)),
                                      field(x, component))
    x
}

dto_precision_integer <- function(precision) {
    f <- factor(precision, c("missing", "year", "month", "day", "hour", "minute", "second", "nanosecond"))
    as.integer(f)
}
PRECISION_MISSING <- dto_precision_integer("missing")
PRECISION_YEAR <- dto_precision_integer("year")
PRECISION_MONTH <- dto_precision_integer("month")
PRECISION_DAY <- dto_precision_integer("day")
PRECISION_HOUR <- dto_precision_integer("hour")
PRECISION_MINUTE <- dto_precision_integer("minute")
PRECISION_SECOND <- dto_precision_integer("second")
PRECISION_NANOSECOND <- dto_precision_integer("nanosecond")

clock_precision_integer <- function(precision) {
    f <- factor(precision,
                c("year", "quarter", "month", "week", "day", "hour", "minute", "second", "millisecond", "microsecond", "nanosecond"))
    as.integer(f)
}

update_missing <- function(original, replacement) ifelse(is.na(original), replacement, original)
update_missing_zone <- function(x, tz = "") {
    set_tz(x, ifelse(is.na(get_tz(x)) & is.na(get_hour_offset(x)), clean_tz(tz), get_tz(x)))
}

#' @rdname datetime_cast
#' @export
datetime_widen.clock_calendar <- function(x, precision, ...) {
    old_precision <- clock_precision_integer(clock::calendar_precision(x))
    new_precision <- clock_precision_integer(precision)
    if (old_precision >= new_precision)
        x
    else
        clock::calendar_widen(x, precision, ...)
}

#' @rdname datetime_cast
#' @export
datetime_widen.clock_time_point <- function(x, precision, ...) {
    old_precision <- clock_precision_integer(clock::time_point_precision(x))
    new_precision <- clock_precision_integer(precision)
    if (old_precision >= new_precision)
        x
    else
        clock::time_point_cast(x, precision)
}
