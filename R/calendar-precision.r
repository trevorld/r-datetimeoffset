#' Calendar precision
#'
#' `calendar_precision()` returns the "precision" of a [datetimeoffset()] vector's datetimes.
#'
#' @param x A [datetimeoffset()] vector
#' @param range If `TRUE` return just the minimum and maximum "precision".
#' @param ... Reserved for other methods.
#' @return `calendar_precision()` returns a character vector of precisions ("year", "month", "day", "hour", "minute", "second", or "nanosecond").
#' @seealso [clock::calendar_precision()]
#' @examples
#'   dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
#'   library("clock", exclude = c("calendar_narrow", "calendar_precision", "calendar_widen"))
#'   calendar_precision(dts)
#'   calendar_precision(dts, range = TRUE)
#' @name calendar_precision
NULL

#' @rdname calendar_precision
#' @export
calendar_precision <- function(x, ...) {
    UseMethod("calendar_precision")
}

#' @importFrom clock calendar_precision
#' @rdname calendar_precision
#' @export
calendar_precision.datetimeoffset <- function(x, range = FALSE, ...) {
    precision <- "nanosecond"
    precision <- ifelse(is.na(field(x, "nanosecond")), "second", precision)
    precision <- ifelse(is.na(field(x, "second")), "minute", precision)
    precision <- ifelse(is.na(field(x, "minute")), "hour", precision)
    precision <- ifelse(is.na(field(x, "hour")), "day", precision)
    precision <- ifelse(is.na(field(x, "day")), "month", precision)
    precision <- ifelse(is.na(field(x, "month")), "year", precision)
    precision <- ifelse(is.na(field(x, "year")), NA_character_, precision)
    if (range) {
        precision <- factor(precision, c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
        precision <- as.integer(precision)
        c("year", "month", "day", "hour", "minute", "second", "nanosecond")[range(precision)]
    } else {
        precision
    }
}

#' Widen/narrow calendar precision
#'
#' `calendar_widen()` sets a floor on the minimum "precision" in the vector by setting any missing
#' elements to their minimum possible value.
#' `calendar_narrow()` sets a cap on the maximum "precision" by setting
#' any more precise elements missing.
#'
#' @param x A [datetimeoffset()] vector
#' @param precision Precision to narrow/widen to.  Either "year", "month", "day", "hour", "minute", "second", or "nanosecond".
#' @param ... Reserved for other methods.
#' @return A [datetimeoffset()] vector.
#' @seealso [clock::calendar_narrow()] and [clock::calendar_widen()]
#' @examples
#'   dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
#'   library("clock", exclude = c("calendar_narrow", "calendar_precision", "calendar_widen"))
#'   calendar_precision(dts)
#'   calendar_narrow(dts, "day")
#'   calendar_widen(dts, "day")
#' @name calendar_narrow
NULL

#' @rdname calendar_narrow
#' @export
calendar_narrow <- function(x, precision, ...) {
    UseMethod("calendar_narrow")
}

#' @rdname calendar_narrow
#' @export
calendar_narrow.datetimeoffset <- function(x, precision, ...) {
    precision <- factor(precision, c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
    precision <- as.integer(precision)
    nas <- rep_len(NA_integer_, length(x))
    if (precision < 7L)
        field(x, "nanosecond") <- nas
    if (precision < 6L)
        field(x, "second") <- nas
    if (precision < 5L)
        field(x, "minute") <- nas
    if (precision < 4L)
        field(x, "hour") <- nas
    if (precision < 3L)
        field(x, "day") <- nas
    if (precision < 2L)
        field(x, "month") <- nas
    x
}

#' @rdname calendar_narrow
#' @export
calendar_narrow.default <- function(x, precision, ...) {
    clock::calendar_narrow(x, precision)
}

#' @rdname calendar_narrow
#' @param ... Used by certain methods
#' @export
calendar_widen <- function(x, precision, ...) {
    UseMethod("calendar_widen")
}

#' @rdname calendar_narrow
#' @param year If missing what year to assume
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param hour   If missing what hour to assume
#' @param minute   If missing what minute to assume
#' @param second   If missing what second to assume
#' @param nanosecond   If missing what nanosecond to assume
#' @export
calendar_widen.datetimeoffset <- function(x, precision, ...,
                                          year = 0L, month = 1L, day = 1L,
                                          hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    precision <- factor(precision, c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
    precision <- as.integer(precision)
    field(x, "year") <- update_missing(field(x, "year"), year)
    if (precision >= 2L)
        field(x, "month") <- update_missing(field(x, "month"), month)
    if (precision >= 3L)
        field(x, "day") <- update_missing(field(x, "day"), day)
    if (precision >= 4L)
        field(x, "hour") <- update_missing(field(x, "hour"), hour)
    if (precision >= 5L)
        field(x, "minute") <- update_missing(field(x, "minute"), minute)
    if (precision >= 6L)
        field(x, "second") <- update_missing(field(x, "second"), second)
    if (precision >= 7L)
        field(x, "nanosecond") <- update_missing(field(x, "nanosecond"), nanosecond)
    x
}

update_missing <- function(original, replacement) ifelse(is.na(original), replacement, original)
update_missing_zone <- function(x, tz = "") {
    set_zone(x, ifelse(is.na(get_zone(x)) & is.na(get_hour_offset(x)), clean_tz(tz), get_zone(x)))
}

#' @rdname calendar_narrow
#' @export
calendar_widen.default <- function(x, precision, ...) {
    clock::calendar_widen(x, precision, ...)
}
