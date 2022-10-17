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
#' * [clock::zoned_time_zone()]
#'
#' We also implement new `clock` "style" getter functions (not in `clock`):
#'
#' * `get_hour_offset()`
#' * `get_minute_offset()`
#' * `get_time_zone()`
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
#' zoned_time_zone(dt)
#' get_time_zone(dt)
#' get_hour_offset(dt)
#' get_minute_offset(dt)
NULL

#' @importFrom clock get_year
#' @rdname clock-getters
#' @export
get_year.datetimeoffset <- function(x) {
    field(x, "year")
}

#' @importFrom clock get_month
#' @rdname clock-getters
#' @export
get_month.datetimeoffset <- function(x) {
    field(x, "month")
}

#' @importFrom clock get_day
#' @rdname clock-getters
#' @export
get_day.datetimeoffset <- function(x) {
    field(x, "day")
}

#' @importFrom clock get_hour
#' @rdname clock-getters
#' @export
get_hour.datetimeoffset <- function(x) {
    field(x, "hour")
}

#' @importFrom clock get_minute
#' @rdname clock-getters
#' @export
get_minute.datetimeoffset <- function(x) {
    field(x, "minute")
}

#' @importFrom clock get_second
#' @rdname clock-getters
#' @export
get_second.datetimeoffset <- function(x) {
    field(x, "second")
}

#' @importFrom clock get_nanosecond
#' @rdname clock-getters
#' @export
get_nanosecond.datetimeoffset <- function(x) {
    field(x, "nanosecond")
}

#' @importFrom clock zoned_time_zone
#' @rdname clock-getters
#' @export
zoned_time_zone.datetimeoffset <- function(x) {
    field(x, "tz")
}

#' @rdname clock-getters
#' @export
get_time_zone <- function(x) {
    UseMethod("get_time_zone")
}

#' @rdname clock-getters
#' @export
get_time_zone.datetimeoffset <- function(x) {
    field(x, "tz")
}

#' @rdname clock-getters
#' @export
get_time_zone.default <- function(x) {
    get_time_zone(as_datetimeoffset(x))
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
