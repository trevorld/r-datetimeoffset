#' Subsecond helper getter/setter
#'
#' Helper getter/setter methods for the subseconds (aka fractional seconds) of [datetimeoffset()] objects.
#'
#' Internally [datetimeoffset()] objects represent subseconds with two fields:
#'
#' 1. Nanoseconds (as an integer)
#' 2. Number of subsecond digits (as an integer)
#'
#' One can explicitly get/set these fields with
#'
#' * `get_nanosecond()` / `set_nanosecond()`
#' * `get_subsecond_digits()` / `set_subsecond_digits()`
#'
#' We implement [datetimeoffset()] support for the following S3 methods from `clock`:
#'
#' * `get_millisecond()`
#' * `get_microsecond()`
#' * `set_millisecond()` (note sets any non-zero microsecond/nanosecond elements to zero)
#' * `set_microsecond()` (note sets any non-zero nanosecond elements to zero)
#'
#' We implement the following new S3 methods:
#'
#' * `get_subsecond()`
#' * `set_subsecond()`
#'
#' @return `get_millisecond()`, `get_microsecond()`, and `get_subsecond()` returns an integer vector.
#'         `set_millisecond()`, `set_microsecond()`, and `set_subsecond()` returns a datetime vector.
#' @examples
#' library("clock")
#' dt <- as_datetimeoffset("2020-01-01T10:10:10.123456789")
#' format(dt)
#' get_millisecond(dt)
#' get_microsecond(dt)
#' get_subsecond(dt, 1L)
#' get_subsecond(dt, 7L)
#'
#' set_microsecond(dt, 123456L)
#' set_millisecond(dt, 123L)
#' set_subsecond(dt, 12L, digits = 2L)
#' set_subsecond(dt, 12L, digits = 3L)
#'
#' @name subsecond
NULL

#' @importFrom clock get_millisecond
#' @inheritParams set_nanosecond.datetimeoffset
#' @rdname subsecond
#' @export
get_millisecond.datetimeoffset <- function(x) {
    trunc(field(x, "nanosecond") / 1e6)
}

#' @importFrom clock set_millisecond
#' @rdname subsecond
#' @export
set_millisecond.datetimeoffset <- function(x, value, ..., na_set = FALSE, digits = 3L) {
    value <- as.integer(1000000 * value)
    set_nanosecond.datetimeoffset(x, value, na_set = na_set, digits = digits)
}

#' @importFrom clock get_microsecond
#' @rdname subsecond
#' @export
get_microsecond.datetimeoffset <- function(x) {
    trunc(field(x, "nanosecond") / 1e3)
}

#' @importFrom clock set_microsecond
#' @rdname subsecond
#' @export
set_microsecond.datetimeoffset <- function(x, value, ..., na_set = FALSE, digits = 6L) {
    value <- as.integer(1000 * value)
    set_nanosecond.datetimeoffset(x, value, na_set = na_set, digits = digits)
}

#' @rdname subsecond
#' @export
get_subsecond <- function(x, ...) {
    UseMethod("get_subsecond")
}

#' @rdname subsecond
#' @export
get_subsecond.datetimeoffset <- function(x, digits = get_subsecond_digits(x), ...) {
    digits <- as.integer(digits)
    denom <- eval(str2expression(sprintf("1e%d", 9L - digits)))
    trunc(field(x, "nanosecond") / denom)
}

#' @rdname subsecond
#' @export
set_subsecond <- function(x, value, digits = 1L, ...) {
    UseMethod("set_subsecond")
}

#' @rdname subsecond
#' @export
set_subsecond.datetimeoffset <- function(x, value, digits = 1L, ..., na_set = FALSE) {
    digits <- as.integer(digits)
    mult <- eval(str2expression(sprintf("1e%d", 9L - digits)))
    value <- as.integer(mult * value)
    set_nanosecond.datetimeoffset(x, value, na_set = na_set, digits = digits)
}
