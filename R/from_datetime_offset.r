#' Convert to other datetime objects
#'
#' We provide methods to convert [datetime_offset()] objects to the following
#' other R datetime objects:
#'
#' * `as.Date()` returns the "local" date as a [base::Date()] object
#'
#' @param x A [datetime_offset()] object
#' @param from A [datetime_offset()] object
#' @param year If missing what year to assume
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param hour   If missing what hour to assume
#' @param minute   If missing what minute to assume
#' @param second   If missing what second to assume
#' @param nanosecond   If missing what nanosecond to assume
#' @param tz   If missing and hour offset also missing what time zone to assume
#' @param ... Ignored
#' @name from_datetime_offset
#' @examples
#'   as.Date(as_datetime_offset("2020-03-05"))
#'   as.Date(as_datetime_offset("2020"))
#'   as.Date(as_datetime_offset("2020"), month = 6, day = 15)
#'
#'   as.nanotime(as_datetime_offset(Sys.time()))
#'   as.POSIXct(as_datetime_offset(Sys.time()), tz = "GMT")
NULL

#' @rdname from_datetime_offset
#' @export
as.Date.datetime_offset <- function(x, year = 1970L, month = 1L, day = 1L, ...) {
    year(x) <- update_missing(year(x), year)
    month(x) <- update_missing(month(x), month)
    day(x) <- update_missing(day(x), day)

    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    s <- paste0(year_str, month_str, day_str)
    as.Date(s)
}

setOldClass("datetime_offset")

#' @rdname from_datetime_offset
#' @export
methods::setMethod("as.nanotime", methods::signature(from="datetime_offset"),
    function(from, ..., year = 1970L, month = 1L, day = 1L,
             hour = 0L, minute = 0L, second = 0L, nanosecond = 0L, tz = "") {
        x <- from
        year(x) <- update_missing(year(x), year)
        month(x) <- update_missing(month(x), month)
        day(x) <- update_missing(day(x), day)
        hour(x) <- update_missing(hour(x), hour)
        minute(x) <- update_missing(minute(x), minute)
        second(x) <- update_missing(second(x), second)
        nanosecond(x) <- update_missing(nanosecond(x), nanosecond)

        tz(x) <- ifelse(is.na(tz(x)) & is.na(hour_offset(x)), clean_tz(tz), tz(x))
        as.nanotime(format_ISO8601(x))
})

update_missing <- function(original, replacement) ifelse(is.na(original), replacement, original)
update_missing_tz <- function(x, tzone) {


}

#' @rdname from_datetime_offset
#' @export
as.POSIXct.datetime_offset <- function(x, tz = mode_tz(x), ...,
                                       year = 1970L, month = 1L, day = 1L,
                                       hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    as.POSIXct(as.nanotime(x, ...,
                           year = year, month = month, day = day,
                           hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                           tz = tz),
               tz = tz)
}

#' @rdname from_datetime_offset
#' @export
as.POSIXlt.datetime_offset <- function(x, tz = mode_tz(x), ...,
                                       year = 1970L, month = 1L, day = 1L,
                                       hour = 0L, minute = 0L, second = 0L, nanosecond = 0L) {
    as.POSIXct(as.nanotime(x, ...,
                           year = year, month = month, day = day,
                           hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                           tz = tz),
               tz = tz)
}
