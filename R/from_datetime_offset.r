#' Convert to other datetime objects
#'
#' We provide methods to convert [datetime_offset()] objects to the following
#' other R datetime objects:
#'
#' * `as.Date()` returns the "local" date as a [base::Date()] object
#'
#' @param x A [datetime_offset()] object
#' @param from A [datetime_offset()] object
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param hour   If missing what hour to assume
#' @param minute   If missing what minute to assume
#' @param second   If missing what second to assume
#' @param tz   If missing and hour offset also missing what time zone to assume
#' @param ... Ignored
#' @name from_datetime_offset
#' @examples
#'   as.Date(as_datetime_offset("2020-03-05"))
#'   as.Date(as_datetime_offset("2020"))
#'   as.Date(as_datetime_offset("2020"), 6, 15)
#'
#'   as.nanotime
NULL

#' @rdname from_datetime_offset
#' @export
as.Date.datetime_offset <- function(x, month = 1, day = 1, ...) {
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
    function(from, ..., month = 1, day = 1,
             hour = 0, minute = 0, second = 0, tz = "GMT") {
        x <- from
        month(x) <- update_missing(month(x), month)
        day(x) <- update_missing(day(x), day)
        hour(x) <- update_missing(hour(x), hour)
        minute(x) <- update_missing(minute(x), minute)
        second(x) <- update_missing(second(x), second)
        tz(x) <- ifelse(is.na(tz(x)) & is.na(hour_offset(x)), tz, tz(x))
        as.nanotime(format(x))
})

update_missing <- function(original, replacement) ifelse(is.na(original), replacement, original)

#' @rdname from_datetime_offset
#' @export
as.POSIXct.datetime_offset <- function(x, tz = mode_tz(x), ...) {
    as.POSIXct(as.nanotime(x), tz = tz)
}

#' @rdname from_datetime_offset
#' @export
as.POSIXlt.datetime_offset <- function(x, tz = mode_tz(x), ...) {
    as.POSIXlt(as.nanotime(x), tz = tz)
}
