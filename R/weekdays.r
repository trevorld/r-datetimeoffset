#' Base Date extractors
#'
#' [base::weekdays()], [base::months()], [base::quarters()], and [base::julian()]
#' are base generic functions to extract the weekday, month, quarter, or Julian time.
#'
#' @examples
#'   dto <- datetimeoffset_now()
#'   print(dto)
#'   weekdays(dto)
#'   months(dto)
#'   quarters(dto)
#'   julian(dto)
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
