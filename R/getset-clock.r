#' Get datetime components with "clock" style getters
#'
#' `clock` style getter methods for [datetimeoffset()] objects
#'
#' We implement the following datetime component getter functions from `clock`:
#'
#' * [clock::get_year()]
#' * [clock::get_month()]
#' * [clock::get_day()]
#' * [clock::get_hour()]
#' * [clock::get_minute()]
#' * [clock::get_second()]
#' * [clock::get_nanosecond()]
#'
#' We also implement new `clock` "style" getter functions (not in `clock`):
#'
#' * `get_hour_offset()`
#' * `get_minute_offset()`
#' * `get_zone()`
#'
#' @param x A [datetimeoffset()] object.
#' @return The component
#' @name clock-getters
#' @seealso \link[clock]{clock-getters}
#' @examples
#' library("clock", quietly = TRUE)
#' if ("Europe/Paris" %in% OlsonNames()) {
#'   dt <- as_datetimeoffset("1918-11-11T11:11:11+00:00[Europe/Paris]")
#' } else {
#'   dt <- as_datetimeoffset("1918-11-11T11:11:11")
#' }
#' get_year(dt)
#' get_month(dt)
#' get_day(dt)
#' get_hour(dt)
#' get_minute(dt)
#' get_second(dt)
#' get_nanosecond(dt)
#' get_zone(dt)
#' get_hour_offset(dt)
#' get_minute_offset(dt)
NULL

#' Set datetime components with "clock" style setters
#'
#' `clock` style setter methods for [datetimeoffset()] objects
#'
#' We implement the following datetime component setter functions from `clock`:
#'
#' * [clock::set_year()]
#' * [clock::set_month()]
#' * [clock::set_day()]
#' * [clock::set_hour()]
#' * [clock::set_minute()]
#' * [clock::set_second()]
#' * [clock::set_nanosecond()]
#'
#' We also implement new `clock` "style" getter functions (not in `clock`):
#'
#' * `set_hour_offset()`
#' * `set_minute_offset()`
#' * `set_zone()` (changes system time but not clock time)
#'
#' @param x A [datetimeoffset()] object.
#' @param value The replacement value.
#' @param ... Currently ignored.
#' @return A [datetimeoffset()] object.
#' @name clock-setters
#' @seealso \link[clock]{clock-setters}
#' @examples
#' library("clock", quietly = TRUE)
#' dt <- NA_datetimeoffset_
#' dt <- set_year(dt, 1918L)
#' dt <- set_month(dt, 11L)
#' dt <- set_day(dt, 11L)
#' dt <- set_hour(dt, 11L)
#' dt <- set_minute(dt, 11L)
#' dt <- set_second(dt, 11L)
#' dt <- set_nanosecond(dt, NA_integer_)
#' dt <- set_zone(dt, "Europe/Paris")
#' dt <- set_hour_offset(dt, 0L)
#' dt <- set_minute_offset(dt, 0L)
#' format(dt)
NULL

#' @importFrom clock get_year
#' @rdname clock-getters
#' @export
get_year.datetimeoffset <- function(x) {
    field(x, "year")
}

#' @importFrom clock set_year
#' @rdname clock-setters
#' @export
set_year.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "year") <- value
    x
}

#' @importFrom clock get_month
#' @rdname clock-getters
#' @export
get_month.datetimeoffset <- function(x) {
    field(x, "month")
}

#' @importFrom clock set_month
#' @rdname clock-setters
#' @export
set_month.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "month") <- value
    x
}

#' @importFrom clock get_day
#' @rdname clock-getters
#' @export
get_day.datetimeoffset <- function(x) {
    field(x, "day")
}

#' @importFrom clock set_day
#' @rdname clock-setters
#' @export
set_day.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "day") <- value
    x
}

#' @importFrom clock get_hour
#' @rdname clock-getters
#' @export
get_hour.datetimeoffset <- function(x) {
    field(x, "hour")
}

#' @importFrom clock set_hour
#' @rdname clock-setters
#' @export
set_hour.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "hour") <- value
    x
}

#' @importFrom clock get_minute
#' @rdname clock-getters
#' @export
get_minute.datetimeoffset <- function(x) {
    field(x, "minute")
}

#' @importFrom clock set_minute
#' @rdname clock-setters
#' @export
set_minute.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "minute") <- value
    x
}

#' @importFrom clock get_second
#' @rdname clock-getters
#' @export
get_second.datetimeoffset <- function(x) {
    field(x, "second")
}

#' @importFrom clock set_second
#' @rdname clock-setters
#' @export
set_second.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "second") <- value
    x
}

#' @importFrom clock get_nanosecond
#' @rdname clock-getters
#' @export
get_nanosecond.datetimeoffset <- function(x) {
    field(x, "nanosecond")
}

#' @importFrom clock set_nanosecond
#' @rdname clock-setters
#' @export
set_nanosecond.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    s <- formatC(value, format = "d", flag = "0", width = 9L)
    stopifnot(isFALSE(any(nchar(s) > 9L)))
    field(x, "nanosecond") <- value
    x
}

#' @rdname clock-getters
#' @export
get_zone <- function(x) {
    UseMethod("get_zone")
}

#' @rdname clock-getters
#' @export
get_zone.datetimeoffset <- function(x) {
    field(x, "tz")
}

#' @rdname clock-getters
#' @export
get_zone.POSIXt <- function(x) {
    tzone <- attr(x, "tzone")
    if (is.null(tzone)) Sys.timezone() else tzone[[1]]
}

#' @rdname clock-getters
#' @export
get_zone.default <- function(x) {
    stop(paste("Method not defined for an object of class", class(x)))
}

#' @rdname clock-setters
#' @export
set_zone <- function(x, value, ...) {
    UseMethod("set_zone")
}

#' @rdname clock-setters
#' @export
set_zone.datetimeoffset <- function(x, value, ...) {
    tzone <- clean_tz(value)
    field(x, "tz") <- tzone
    x
}

#' @rdname clock-setters
#' @export
set_zone.default <- function(x, value, ...) {
    lubridate::force_tz(x, value)
}

#' @rdname clock-getters
#' @export
get_hour_offset <- function(x) {
    UseMethod("get_hour_offset")
}

#' @rdname clock-getters
#' @export
get_hour_offset.datetimeoffset <- function(x) {
    field(x, "hour_offset")
}

#' @rdname clock-getters
#' @export
get_hour_offset.default <- function(x) {
    get_hour_offset(as_datetimeoffset(x))
}

#' @rdname clock-getters
#' @export
get_hour_offset.POSIXt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname clock-setters
#' @export
set_hour_offset <- function(x, value, ...) {
    UseMethod("set_hour_offset")
}

#' @rdname clock-setters
#' @export
set_hour_offset.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "hour_offset") <- value
    x
}

#' @rdname clock-setters
#' @export
set_hour_offset.default <- function(x, value, ...) {
    stop(paste("Method not defined for an object of class", class(x)))
}

#' @rdname clock-getters
#' @export
get_minute_offset <- function(x) {
    UseMethod("get_minute_offset")
}

#' @rdname clock-getters
#' @export
get_minute_offset.datetimeoffset <- function(x) {
    field(x, "minute_offset")
}

#' @rdname clock-getters
#' @export
get_minute_offset.default <- function(x) {
    get_minute_offset(as_datetimeoffset(x))
}

#' @rdname clock-getters
#' @export
get_minute_offset.POSIXt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname clock-setters
#' @export
set_minute_offset <- function(x, value, ...) {
    UseMethod("set_minute_offset")
}

#' @rdname clock-setters
#' @export
set_minute_offset.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "minute_offset") <- value
    x
}

#' @rdname clock-setters
#' @export
set_minute_offset.default <- function(x, value, ...) {
    stop(paste("Method not defined for an object of class", class(x)))
}
