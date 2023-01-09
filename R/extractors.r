#' Additional datetime extractors
#'
#' Additional datetime extractors for [datetimeoffset()] objects.
#'
#' We implement [datetimeoffset()] support for the following S3 methods from `base`:
#'
#' * `weekdays()`
#' * `months()`
#' * `quarters()`
#' * `julian()`
#'
#' There is also [datetimeoffset()] support for the following methods from `lubridate`:
#'
#' * `isoyear()` and `epiyear()`
#' * `quarter()` and `semester()`
#' * `week()`, `isoweek()`, and `epiweek()`
#' * `wday()` and `wday<-()`
#' * `qday()` and `qday<-()`
#' * `yday()` and `yday<-()`
#' * `am()` and `pm()`
#' * `days_in_month()`
#' * `dst()`
#' * `leap_year()`
#'
#' @return `weekdays()`, `months()`, `quarters()`, `julian()` return character vectors.
#'         See [base::weekdays()] for more information.
#' @examples
#' dto <- datetimeoffset_now()
#' print(dto)
#' weekdays(dto)
#' months(dto)
#' quarters(dto)
#' julian(dto)
#'
#' if (require("lubridate")) {
#'   cat("`isoyear(dto)`: ", isoyear(dto), "\n")
#'   cat("`epiyear(dto)`: ", epiyear(dto), "\n")
#'   cat("`semester(dto)`: ", semester(dto), "\n")
#'   cat("`quarter(dto)`: ", quarter(dto), "\n")
#'   cat("`week(dto)`: ", week(dto), "\n")
#'   cat("`isoweek(dto)`: ", isoweek(dto), "\n")
#'   cat("`epiweek(dto)`: ", epiweek(dto), "\n")
#'   cat("`wday(dto)`: ", wday(dto), "\n")
#'   cat("`qday(dto)`: ", qday(dto), "\n")
#'   cat("`yday(dto)`: ", yday(dto), "\n")
#'   cat("`am(dto)`: ", am(dto), "\n")
#'   cat("`pm(dto)`: ", pm(dto), "\n")
#'   cat("`days_in_month(dto)`: ", days_in_month(dto), "\n")
#'   cat("`dst(dto)`: ", dst(dto), "\n")
#'   cat("`leap_year(dto)`: ", leap_year(dto), "\n")
#' }
#'
#' @name weekdays.datetimeoffset
NULL

#' @param x A [datetimeoffset()] datetime
#' @param abbreviate Logical vector for whether the names should be abbreviated
#' @param ... Ignored
#' @rdname weekdays.datetimeoffset
#' @export
weekdays.datetimeoffset <- function(x, abbreviate = FALSE) {
    weekdays(as.Date.datetimeoffset(x), abbreviate = abbreviate)
}

#' @rdname weekdays.datetimeoffset
#' @export
months.datetimeoffset <- function(x, abbreviate = FALSE) {
    months(as.Date.datetimeoffset(x), abbreviate = abbreviate)
}

#' @rdname weekdays.datetimeoffset
#' @export
quarters.datetimeoffset <- function(x, ...) {
    quarters(as.Date.datetimeoffset(x))
}

#' @rdname weekdays.datetimeoffset
#' @param origin Length one datetime of origin
#' @export
julian.datetimeoffset <- function(x, origin = as.Date("1970-01-01"), ...) {
    julian(as.Date.datetimeoffset(x), origin = as.Date(origin))
}

# S3 methods registered during .onLoad()
wday.datetimeoffset <- function(x, label = FALSE, abbr = TRUE,
                                week_start = getOption("lubridate.week.start", 7),
                                locale = Sys.getlocale("LC_TIME")) {
    assert_suggested("lubridate")
    lubridate::wday(as.Date.datetimeoffset(x),
                    label = label, abbr = abbr,
                    week_start = week_start, local = locale)
}
qday.datetimeoffset <- function(x) {
    assert_suggested("lubridate")
    lubridate::qday(as.Date.datetimeoffset(x))
}
yday.datetimeoffset <- function(x) {
    assert_suggested("lubridate")
    lubridate::yday(as.Date.datetimeoffset(x))
}

# needed for `lubridate::wday<-()` and `lubridate::yday<-()`
update.datetimeoffset <- function(x, ...,
                                  ydays = NULL, wdays = NULL, week_start = 1) {
    assert_suggested("lubridate")
    if (!is.null(wdays)) {
        d <- as.Date.datetimeoffset(x)
        lubridate::wday(d, week_start = week_start) <- wdays
        lubridate::date(x) <- d
    }
    if (!is.null(ydays)) {
        d <- as.Date.datetimeoffset(x)
        lubridate::yday(d) <- ydays
        lubridate::date(x) <- d
    }
    x
}

`qday<-.datetimeoffset` <- function(x, value) {
    assert_suggested("lubridate")
    d <- as.Date.datetimeoffset(x)
    lubridate::qday(d) <- value
    lubridate::date(x) <- d
    x
}
