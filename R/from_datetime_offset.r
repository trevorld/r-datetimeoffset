#' Convert to other datetime objects
#'
#' We provide methods to convert [datetime_offset()] objects to the following
#' other R datetime objects:
#'
#' * `as.Date()` returns the "local" date as a [base::Date()] object
#'
#' @param x A [datetime_offset()] object
#' @param month If missing what month to assume
#' @param day   If missing what day to assume
#' @param ... Ignored
#' @name from_datetime_offset
#' @examples
#'   as.Date(as_datetime_offset("2020-03-05"))
#'   as.Date(as_datetime_offset("2020"))
#'   as.Date(as_datetime_offset("2020"), 6, 15)
NULL

#' @rdname from_datetime_offset
#' @export
as.Date.datetime_offset <- function(x, month = 1, day = 1, ...) {
    month(x) <- ifelse(is.na(month(x)), month, month(x))
    day(x) <- ifelse(is.na(day(x)), day, day(x))

    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    s <- paste0(year_str, month_str, day_str)
    as.Date(s)
}
