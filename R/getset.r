#' Get datetime components
#'
#' Getter methods for [datetimeoffset()] objects.
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
#' * `get_subsecond_digits()`
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
#'   dt <- as_datetimeoffset("1918-11-11T11:11:11.1234+00:00[Europe/Paris]")
#' } else {
#'   dt <- as_datetimeoffset("1918-11-11T11:11:11.1234")
#' }
#' get_year(dt)
#' get_month(dt)
#' get_day(dt)
#' get_hour(dt)
#' get_minute(dt)
#' get_second(dt)
#' get_nanosecond(dt)
#' get_subsecond_digits(dt)
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
#' Setter methods for [datetimeoffset()] objects.
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
#' dt <- set_year(dt, 1918L, na_set = TRUE)
#' dt <- set_month(dt, 11L)
#' dt <- set_day(dt, 11L)
#' dt <- set_hour(dt, 11L)
#' dt <- set_minute(dt, 11L)
#' dt <- set_second(dt, 11L)
#' dt <- set_nanosecond(dt, 123456789L)
#' dt <- set_subsecond_digits(dt, 4L)
#' dt <- set_hour_offset(dt, 0L)
#' dt <- set_minute_offset(dt, 0L)
#' dt <- set_tz(dt, "Europe/Paris")
#' format(dt)
#'
#' if (require("lubridate")) {
#'   dt <- datetimeoffset(0L)
#'   year(dt) <- 1918L
#'   month(dt) <- 11L
#'   day(dt) <- 11L
#'   hour(dt) <- 11L
#'   minute(dt) <- 11L
#'   second(dt) <- 11L
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
#' @param na_set If `TRUE` set component for `NA` datetimes (making them no longer `NA`)
#' @rdname setters
#' @export
set_year.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    field(x, "year") <- set_helper(x, value, na_set)
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
set_month.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, 1L, 12L)
    field(x, "month") <- set_helper(x, value, na_set)
    x
}

assert_bounds <- function(value, min, max) {
    stopifnot(all(is.na(value) | (max >= value) & (value >= min)))
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
set_day.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    if (identical(value, "last")) {
        precision <- precision_to_int(datetime_precision(x, range = TRUE)[1])
        ym <- clock::year_month_day(field(x, "year"), field(x, "month"))
        value <- get_day(set_day(ym, "last"))
    }
    value <- as.integer(value)
    assert_bounds(value, 1L, 31L)
    field(x, "day") <- set_helper(x, value, na_set)
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
set_hour.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, 0L, 24L)
    field(x, "hour") <- set_helper(x, value, na_set)
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
set_minute.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, 0L, 60L)
    field(x, "minute") <- set_helper(x, value, na_set)
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
set_second.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, 0L, 61L) # leap seconds
    field(x, "second") <- set_helper(x, value, na_set)
    x
}

#' @importFrom clock get_nanosecond
#' @rdname getters
#' @export
get_nanosecond.datetimeoffset <- function(x) {
    field(x, "nanosecond")
}

#### Flag to not adjust `subsecond_digits` field?

#' @importFrom clock set_nanosecond
#' @rdname setters
#' @param digits If `NULL` do not update the `subsecond_digits` field.
#'               Otherwise an integer vector (`1L` through `9L` or `NA_integer_`)
#'               to update the `subsecond_digits` field with.
#' @export
set_nanosecond.datetimeoffset <- function(x, value, ...,
                                          na_set = FALSE, digits = NULL) {
    value <- as.integer(value)
    assert_bounds(value, 0L, .Machine$integer.max)
    field(x, "nanosecond") <- set_helper(x, value, na_set)
    if (!is.null(digits))
        x <- set_subsecond_digits(x, digits, na_set = na_set)
    x
}

#' @rdname getters
#' @export
get_subsecond_digits <- function(x) {
    UseMethod("get_subsecond_digits")
}

#' @rdname getters
#' @export
get_subsecond_digits.datetimeoffset <- function(x) {
    field(x, "subsecond_digits")
}

#' @rdname getters
#' @export
get_subsecond_digits.default <- function(x) {
    get_subsecond_digits.datetimeoffset(as_datetimeoffset(x))
}

#' @rdname setters
#' @export
set_subsecond_digits <- function(x, value, ...) {
    UseMethod("set_subsecond_digits")
}

#' @rdname setters
#' @export
set_subsecond_digits.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, 0L, 9L)
    field(x, "subsecond_digits") <- set_helper(x, value, na_set)
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
    as.integer(substr(clock::date_format(x, format = "%z"), 1, 3))
}

#' @rdname setters
#' @export
set_hour_offset <- function(x, value, ...) {
    UseMethod("set_hour_offset")
}

#' @rdname setters
#' @export
set_hour_offset.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(value)
    assert_bounds(value, -12L, 14L)
    field(x, "hour_offset") <- set_helper(x, value, na_set)
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
    as.integer(substr(clock::date_format(x, format = "%z"), 4, 5))
}

#' @rdname setters
#' @export
set_minute_offset <- function(x, value, ...) {
    UseMethod("set_minute_offset")
}

#' @rdname setters
#' @export
set_minute_offset.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    value <- as.integer(abs(value))
    assert_bounds(value, 0L, 60L)
    field(x, "minute_offset") <- set_helper(x, value, na_set)
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
    clock::date_time_zone(x)
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
set_tz.datetimeoffset <- function(x, value, ..., na_set = FALSE) {
    tzone <- clean_tz(value)
    field(x, "tz") <- set_helper(x, tzone, na_set)
    x
}

#' @rdname setters
#' @param nonexistent What to do when the "clock time" in the new time zone doesn't exist.
#'                    See [clock::as_zoned_time.clock_naive_time()].
#' @param ambiguous What to do when the "clock time" in the new time zone is ambiguous.
#'                  See [clock::as_zoned_time.clock_naive_time()].
#' @export
set_tz.clock_zoned_time <- function(x, value, ...,
                                    nonexistent = "error", ambiguous = "error") {
    nt <- clock::as_naive_time(x)
    clock::as_zoned_time(nt, value, nonexistent = nonexistent, ambiguous = ambiguous)
}

#' @rdname setters
#' @export
set_tz.default <- function(x, value, ...) {
    assert_suggested("lubridate")
    lubridate::force_tz(x, value)
}

force_tz.datetimeoffset <- function(time, tzone = "", ...) {
    set_tz.datetimeoffset(time, tzone)
}

`date<-.datetimeoffset` <- function(x, value) {
    x <- set_year(x, get_year(value))
    x <- set_month(x, get_month(value))
    x <- set_day(x, get_day(value))
    x
}

month.datetimeoffset <- function(x, label = FALSE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")) {
    assert_suggested("lubridate")
    lubridate::month(get_month(x), label = label, abbr = abbr, locale = locale)
}

set_helper <- function(x, value, na_set) {
    na <- NA
    storage.mode(na) <- storage.mode(value)
    ifelse(na_set | !is.na(x),
           rep_len(value, length(x)),
           rep_len(na, length(x)))
}
