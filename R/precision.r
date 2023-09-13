#' Datetime precision
#'
#' `datetime_precision()` returns the "precision" of a datetime vector's datetimes.
#' `precision_to_int()` converts the precision to an integer.
#'
#' @param x A datetime vector.  Either [datetimeoffset()], a "clock" "calendar", or a "clock" "time".
#' @param range If `TRUE` return just the minimum and maximum "precision".
#' @param unspecified If `TRUE` use the smallest non-missing component's as the precision even
#'                    if there is a missing value for a larger component.
#' @param ... Used by some S3 methods.
#' @return `datetime_precision()` returns a character vector of precisions.
#'         Depending on the object either "missing", "year", "quarter", "month", "week",
#'         "day", "hour", "minute", "second", "millisecond", "microsecond", or "nanosecond".
#'         `precision_to_int()` returns an integer vector.
#' @examples
#'   dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
#'   datetime_precision(dts)
#'   datetime_precision(dts, range = TRUE)
#'
#'   dt <- datetimeoffset(2020, NA_integer_, 10)
#'   datetime_precision(dt)
#'   datetime_precision(dt, unspecified = TRUE)
#'
#'   precision_to_int("year") < precision_to_int("day")
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
datetime_precision.datetimeoffset <- function(x, range = FALSE, unspecified = FALSE, ...) {
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
        precision <- ifelse(precision == "nanosecond" & !is.na(field(x, "subsecond_digits")),
                            subsecond_digit_to_precision(field(x, "subsecond_digits")),
                            precision)
    } else {
        precision <- subsecond_digit_to_precision(field(x, "subsecond_digits"))
        precision <- ifelse(is.na(field(x, "nanosecond")), "second", precision)
        precision <- ifelse(is.na(field(x, "second")), "minute", precision)
        precision <- ifelse(is.na(field(x, "minute")), "hour", precision)
        precision <- ifelse(is.na(field(x, "hour")), "day", precision)
        precision <- ifelse(is.na(field(x, "day")), "month", precision)
        precision <- ifelse(is.na(field(x, "month")), "year", precision)
        precision <- ifelse(is.na(field(x, "year")), "missing", precision)
    }
    if (range) {
        precision <- precision_to_int(precision)
        datetime_precisions[range(precision)]
    } else {
        precision
    }
}

subsecond_digit_to_precision <- function(x) {
    purrr::map_chr(x, function(x) {
                       precision <- switch(x,
                                           "decisecond",
                                           "centisecond",
                                           "millisecond",
                                           "hundred microseconds",
                                           "ten microseconds",
                                           "microsecond",
                                           "hundred nanoseconds",
                                           "ten nanoseconds",
                                           "nanosecond")
                       precision %||% "nanosecond"
                    })
}

precision_to_subsecond_digit <- function(x) {
    purrr::map_int(x, function(x) {
                       switch(x,
                              nanosecond = 9L,
                              `ten nanoseconds` = 8L,
                              `hundred nanoseconds` = 7L,
                              microsecond = 6L,
                              `ten microseconds` = 5L,
                              `hundred microseconds` = 4L,
                              millisecond = 3L,
                              centisecond = 2L,
                              decisecond = 1L,
                              NA_integer_)
                    })
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

#' @rdname datetime_precision
#' @export
datetime_precision.nanotime <- function(x, ...) {
    "nanosecond"
}

#' Widen/narrow datetime precision
#'
#' `datetime_widen()` sets a floor on the minimum "precision" in the datetime vector
#'  by setting any missing elements to their minimum possible value.
#' `datetime_narrow()` sets a cap on the maximum "precision" by setting
#' any more precise elements missing.
#' `datetime_cast()` sets the precision exactly by calling both
#' `datetime_narrow()` and `datetime_widen()`.
#'
#' @param x A datetime vector.  Either [datetimeoffset()], a "clock" "calendar", or a "clock" "time point".
#' @param precision Precision to narrow/widen to.  Either "missing", "year", "month", "day", "hour", "minute", "second", or "nanosecond".
#' @param ... Used by some methods.
#'            The default method for `datetime_cast()` will pass this to both `datetime_narrow()` and `datetime_widen()`.
#' @return A datetime vector.
#' @examples
#'   dts <- as_datetimeoffset(c(NA_character_, "2020", "2020-04-10", "2020-04-10T10:10"))
#'   datetime_precision(dts)
#'   datetime_narrow(dts, "day")
#'   datetime_widen(dts, "day")
#'   datetime_cast(dts, "day")
#'
#'   datetime_widen(datetimeoffset(2020L), "day", month = 6, day = 15)
#'
#'   # vectorized "precision" is allowed
#'   datetime_narrow(as_datetimeoffset(Sys.time()),
#'                   c("year", "day", "second"))
#'   datetime_widen(NA_datetimeoffset_, c("year", "day", "second"), na_set = TRUE)
#'
#'   library("clock")
#'   ymd <- year_month_day(1918, 11, 11, 11)
#'   datetime_narrow(ymd, "day")
#'   datetime_narrow(ymd, "second") # already narrower than "second"
#'   datetime_widen(ymd, "second")
#'   datetime_widen(ymd, "day") # already wider than "day"
#'
#'   \dontrun{
#'     # comparable {clock} calendar methods throw an error in certain cases
#'     clock::calendar_narrow(ymd, "second") # already narrower than "second"
#'     clock::calendar_widen(ymd, "day") # already wider than "day"
#'   }
#'
#'   nt <- as_naive_time(ymd)
#'   datetime_narrow(nt, "day")
#'   datetime_narrow(nt, "second")
#'   datetime_widen(nt, "second")
#'   datetime_widen(nt, "day")
#'   datetime_cast(nt, "day") # same as clock::time_point_floor(nt, "day")
#'   datetime_cast(nt, "day", method = "cast") # same as clock::time_point_cast(nt, "day")
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
    precision_int <- precision_to_int(precision)
    n <- max(length(x), length(precision))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(precision_int) < n) {
        precision <- rep(precision, length.out = n)
        precision_int <- rep(precision_int, length.out = n)
    }
    nas <- rep_len(NA_integer_, n)
    field(x, "nanosecond") <- ifelse(precision_int <= precision_to_int("second"),
                                     nas,
                                     field(x, "nanosecond"))
    field(x, "subsecond_digits") <- ifelse(precision_int <= precision_to_int("second"),
                                           nas,
                                           precision_to_subsecond_digit(precision))
    for (component in c("second", "minute", "hour", "day", "month", "year"))
        field(x, component) <- ifelse(precision_int < precision_to_int(component),
                                      nas,
                                      field(x, component))
    x
}

#' @rdname datetime_cast
#' @export
datetime_narrow.clock_calendar <- function(x, precision, ...) {
    old_precision <- precision_to_int(clock::calendar_precision(x))
    new_precision <- precision_to_int(precision)
    if (old_precision <= new_precision)
        x
    else
        clock::calendar_narrow(x, precision)
}

#' @rdname datetime_cast
#' @param method Depending on the class either "floor", "ceiling", "round", and/or "cast".
#' @export
datetime_narrow.clock_time_point <- function(x, precision, ...,
                                             method = c("floor", "round", "ceiling", "cast")) {
    old_precision <- precision_to_int(clock::time_point_precision(x))
    new_precision <- precision_to_int(precision)
    if (old_precision <= new_precision) {
        x
    } else {
        method <- match.arg(method, c("floor", "round", "ceiling", "cast"))
        switch(method,
               floor = clock::time_point_floor(x, precision),
               round = clock::time_point_round(x, precision),
               ceiling = clock::time_point_ceiling(x, precision),
               cast = clock::time_point_cast(x, precision))
    }
}

#' @rdname datetime_cast
#' @param nonexistent What to do when the "clock time" in the new time zone doesn't exist.
#'                    See [clock::as_zoned_time.clock_naive_time()].
#' @param ambiguous What to do when the "clock time" in the new time zone is ambiguous.
#'                  See [clock::as_zoned_time.clock_naive_time()].
#' @export
datetime_narrow.POSIXt <- function(x, precision, ...,
                                   method = c("floor", "round", "ceiling"),
                                   nonexistent = "error", ambiguous = x) {
        method <- match.arg(method, c("floor", "round", "ceiling"))
        switch(method,
               floor = clock::date_floor(x, precision, nonexistent = nonexistent, ambiguous = ambiguous),
               round = clock::date_round(x, precision, nonexistent = nonexistent, ambiguous = ambiguous),
               ceiling = clock::date_ceiling(x, precision, nonexistent = nonexistent, ambiguous = ambiguous))
}

#' @rdname datetime_cast
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
#' @param na_set If `TRUE` widen the "missing" datetimes as well.
#' @export
datetime_widen.datetimeoffset <- function(x, precision, ...,
                                          year = 0L, month = 1L, day = 1L,
                                          hour = 0L, minute = 0L, second = 0L, nanosecond = 0L,
                                          na_set = FALSE) {
    precision_int <- precision_to_int(precision)
    n <- max(length(x), length(precision_int))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(precision_int) < n) {
        precision <- rep(precision, length.out = n)
        precision_int <- rep(precision_int, length.out = n)
    }
    for (component in c("year", "month", "day", "hour", "minute", "second"))
        field(x, component) <- ifelse((na_set | !is.na(x)) & precision_int >= precision_to_int(component),
                                      update_missing(field(x, component), get(component)),
                                      field(x, component))
    field(x, "nanosecond") <- ifelse((na_set | !is.na(x)) & precision_int > precision_to_int("second"),
                                     update_missing(field(x, "nanosecond"), nanosecond),
                                     field(x, "nanosecond"))
    field(x, "subsecond_digits") <- ifelse((na_set | !is.na(x)) & precision_int > precision_to_int("second"),
                                           precision_to_subsecond_digit(precision),
                                           field(x, "subsecond_digits"))
    x
}

#' @rdname datetime_cast
#' @export
datetime_widen.clock_calendar <- function(x, precision, ...) {
    old_precision <- precision_to_int(clock::calendar_precision(x))
    new_precision <- precision_to_int(precision)
    if (old_precision >= new_precision)
        x
    else
        clock::calendar_widen(x, precision, ...)
}

#' @rdname datetime_cast
#' @export
datetime_widen.clock_time_point <- function(x, precision, ...) {
    old_precision <- precision_to_int(clock::time_point_precision(x))
    new_precision <- precision_to_int(precision)
    if (old_precision >= new_precision)
        x
    else
        clock::time_point_cast(x, precision)
}

#' @rdname datetime_cast
#' @export
datetime_widen.POSIXt <- function(x, precision, ...) {
    x
}

#' @rdname datetime_cast
#' @export
datetime_cast <- function(x, precision, ...) {
    UseMethod("datetime_cast")
}

#' @rdname datetime_cast
#' @export
datetime_cast.default <- function(x, precision, ...) {
    datetime_widen(datetime_narrow(x, precision, ...), precision, ...)
}

# precisions used by {datetimeoffset} and/or {clock}
datetime_precisions <- c("missing",
                         "year", "quarter", "month", "week", "day",
                         "hour", "minute", "second",
                         "decisecond", "centisecond", "millisecond",
                         "hundred microseconds", "ten microseconds", "microsecond",
                         "hundred nanoseconds", "ten nanoseconds", "nanosecond")

#' @param precision A datetime precision (as returned by `datetime_precision()`).
#' @rdname datetime_precision
#' @export
precision_to_int <- function(precision) {
    f <- factor(precision, datetime_precisions)
    as.integer(f)
}
PRECISION_MISSING <- precision_to_int("missing")
PRECISION_YEAR <- precision_to_int("year")
PRECISION_MONTH <- precision_to_int("month")
PRECISION_DAY <- precision_to_int("day")
PRECISION_HOUR <- precision_to_int("hour")
PRECISION_MINUTE <- precision_to_int("minute")
PRECISION_SECOND <- precision_to_int("second")
PRECISION_MILLISECOND <- precision_to_int("millisecond")
PRECISION_MICROSECOND <- precision_to_int("microsecond")
PRECISION_NANOSECOND <- precision_to_int("nanosecond")

update_missing <- function(original, replacement) ifelse(is.na(original), replacement, original)
