#' Get datetime components
#'
#' Getter methods for [datetimeoffset()] objects
#'
#' We implement [datetimeoffset()] support for the following S3 methods from `clock`:
#'
#' * `get_year()`
#' * `get_month()`
#' * `get_day()`
#' * `get_hour()`
#' * `get_minute()`
#' * `get_second()`
#' * `get_nanosecond()`
#'
#' We also implemented new S3 getter methods:
#'
#' * `get_hour_offset()`
#' * `get_minute_offset()`
#' * `get_tz()`
#'
#' We also implement [datetimeoffset()] support for the following S3 methods from `lubridate`:
#'
#' * `year()`
#' * `month()`
#' * `mday()`
#' * `hour()`
#' * `minute()`
#' * `second()`
#' * `tz()`
#' * `date()`
#'
#' @param x A datetime object.
#' @return The component
#' @name getters
#' @examples
#' library("clock")
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
#' get_hour_offset(dt)
#' get_minute_offset(dt)
#' get_tz(dt)
#' if (require("lubridate")) {
#'   paste0(year(dt), "-", month(dt), "-", day(dt),
#'          "T", hour(dt), ":", minute(dt), ":", second(dt),
#'          "[", tz(dt), "]")
#' }
NULL

#' Set datetime components
#'
#' Setter methods for [datetimeoffset()] objects
#'
#' We implement [datetimeoffset()] support for the following S3 methods from `clock`:
#'
#' * `set_year()`
#' * `set_month()`
#' * `set_day()`
#' * `set_hour()`
#' * `set_minute()`
#' * `set_second()`
#' * `set_nanosecond()`
#'
#' We also implemented new S3 setter methods:
#'
#' * `set_hour_offset()`
#' * `set_minute_offset()`
#' * `set_tz()` (changes system time but not clock time)
#'
#' We also implement [datetimeoffset()] support for the following S4 methods from `lubridate`:
#'
#' * `year<-()`
#' * `month<-()`
#' * `day<-()`
#' * `hour<-()`
#' * `minute<-()`
#' * `second<-()`
#' * `date<-()`
#'
#' @param x A datetime object.
#' @param value The replacement value.  For `set_day()` this can also be "last".
#' @param ... Currently ignored.
#' @return A datetime object.
#' @name setters
#' @examples
#' library("clock")
#' dt <- NA_datetimeoffset_
#' dt <- set_year(dt, 1918L)
#' dt <- set_month(dt, 11L)
#' dt <- set_day(dt, 11L)
#' dt <- set_hour(dt, 11L)
#' dt <- set_minute(dt, 11L)
#' dt <- set_second(dt, 11L)
#' dt <- set_nanosecond(dt, NA_integer_)
#' dt <- set_hour_offset(dt, 0L)
#' dt <- set_minute_offset(dt, 0L)
#' dt <- set_tz(dt, "Europe/Paris")
#' format(dt)
#'
#' if (require("lubridate")) {
#'   dt <- NA_datetimeoffset_
#'   year(dt) <- 1918
#'   month(dt) <- 11
#'   day(dt) <- 11
#'   hour(dt) <- 11
#'   minute(dt) <- 11
#'   second(dt) <- 11
#'   if (packageVersion("lubridate") > '1.8.0' &&
#'       "Europe/Paris" %in% OlsonNames()) {
#'     tz(dt) <- "Europe/Paris"
#'   }
#'   format(dt)
#' }
NULL

#' @importFrom clock get_year
#' @rdname getters
#' @export
get_year.datetimeoffset <- function(x) {
    field(x, "year")
}

#' @importFrom clock set_year
#' @rdname setters
#' @export
set_year.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "year") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_month
#' @rdname getters
#' @export
get_month.datetimeoffset <- function(x) {
    field(x, "month")
}

#' @importFrom clock set_month
#' @rdname setters
#' @export
set_month.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "month") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_day
#' @rdname getters
#' @export
get_day.datetimeoffset <- function(x) {
    field(x, "day")
}

#' @importFrom clock set_day
#' @rdname setters
#' @export
set_day.datetimeoffset <- function(x, value, ...) {
    if (identical(value, "last")) {
        precision <- precision_to_int(datetime_precision(x, range = TRUE)[1])
        stopifnot(precision >= PRECISION_MONTH)
        ym <- clock::year_month_day(field(x, "year"), field(x, "month"))
        value <- get_day(set_day(ym, "last"))
    }
    value <- as.integer(value)
    field(x, "day") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_hour
#' @rdname getters
#' @export
get_hour.datetimeoffset <- function(x) {
    field(x, "hour")
}

#' @importFrom clock set_hour
#' @rdname setters
#' @export
set_hour.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "hour") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_minute
#' @rdname getters
#' @export
get_minute.datetimeoffset <- function(x) {
    field(x, "minute")
}

#' @importFrom clock set_minute
#' @rdname setters
#' @export
set_minute.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "minute") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_second
#' @rdname getters
#' @export
get_second.datetimeoffset <- function(x) {
    field(x, "second")
}

#' @importFrom clock set_second
#' @rdname setters
#' @export
set_second.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "second") <- rep_len(value, length(x))
    x
}

#' @importFrom clock get_nanosecond
#' @rdname getters
#' @export
get_nanosecond.datetimeoffset <- function(x) {
    field(x, "nanosecond")
}

#' @importFrom clock set_nanosecond
#' @rdname setters
#' @export
set_nanosecond.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    s <- formatC(value, format = "d", flag = "0", width = 9L)
    stopifnot(isFALSE(any(nchar(s) > 9L)))
    field(x, "nanosecond") <- rep_len(value, length(x))
    x
}

#' @rdname getters
#' @export
get_hour_offset <- function(x) {
    UseMethod("get_hour_offset")
}

#' @rdname getters
#' @export
get_hour_offset.datetimeoffset <- function(x) {
    field(x, "hour_offset")
}

#' @rdname getters
#' @export
get_hour_offset.default <- function(x) {
    get_hour_offset(as_datetimeoffset(x))
}

#' @rdname getters
#' @export
get_hour_offset.POSIXt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname setters
#' @export
set_hour_offset <- function(x, value, ...) {
    UseMethod("set_hour_offset")
}

#' @rdname setters
#' @export
set_hour_offset.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "hour_offset") <- rep_len(value, length(x))
    x
}

#' @rdname getters
#' @export
get_minute_offset <- function(x) {
    UseMethod("get_minute_offset")
}

#' @rdname getters
#' @export
get_minute_offset.datetimeoffset <- function(x) {
    field(x, "minute_offset")
}

#' @rdname getters
#' @export
get_minute_offset.default <- function(x) {
    get_minute_offset(as_datetimeoffset(x))
}

#' @rdname getters
#' @export
get_minute_offset.POSIXt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname setters
#' @export
set_minute_offset <- function(x, value, ...) {
    UseMethod("set_minute_offset")
}

#' @rdname setters
#' @export
set_minute_offset.datetimeoffset <- function(x, value, ...) {
    value <- as.integer(value)
    field(x, "minute_offset") <- rep_len(value, length(x))
    x
}

#' @rdname getters
#' @export
get_tz <- function(x) {
    UseMethod("get_tz")
}

#' @rdname getters
#' @export
get_tz.datetimeoffset <- function(x) {
    field(x, "tz")
}

#' @rdname getters
#' @export
get_tz.POSIXt <- function(x) {
    clock::date_zone(x)
}

#' @rdname getters
#' @export
get_tz.clock_zoned_time <- function(x) {
    clock::zoned_time_zone(x)
}

#' @rdname getters
#' @export
get_tz.default <- function(x) {
    assert_suggested("lubridate")
    lubridate::tz(x)
}

#' @rdname setters
#' @export
set_tz <- function(x, value, ...) {
    UseMethod("set_tz")
}

#' @rdname setters
#' @export
set_tz.datetimeoffset <- function(x, value, ...) {
    tzone <- clean_tz(value)
    field(x, "tz") <- rep_len(tzone, length(x))
    x
}

#' @rdname setters
#' @export
set_tz.default <- function(x, value, ...) {
    assert_suggested("lubridate")
    lubridate::force_tz(x, value)
}
