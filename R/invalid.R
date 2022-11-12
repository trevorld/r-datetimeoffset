#' Invalid datetimeoffset datetimes
#'
#' `invalid_detect()` detects invalid datetimes.
#' `invalid_any()` returns TRUE if any datetimes are invalid.
#' `invalid_count()` returns number of invalid datetimes.
#' `invalid_remove()` removes invalid datetimes.
#' @return `invalid_detect()` and `invalid_remove()` return [datetimeoffset()] vectors.
#'         `invalid_count()` returns an integer and `invalid_any()` returns a logical value.
#' @name datetimeoffset-invalid
#' @examples
#' # invalid date because April only has 30 days
#' dts <- c("2019-04-30T03:30:00", "2019-04-31T02:30:00")
#' dts <- as_datetimeoffset(dts)
#' clock::invalid_detect(dts)
#' clock::invalid_any(dts)
#' clock::invalid_count(dts)
#' clock::invalid_remove(dts)
#'
#' if ("America/Los_Angeles" %in% OlsonNames()) {
#'   # non-existent time because of DST "spring forward"
#'   dt <- as_datetimeoffset("2020-03-08 02:59:59[America/Los_Angeles]")
#'   clock::invalid_detect(dt)
#' }
#'
#' if ("America/New_York" %in% OlsonNames()) {
#'   # incorrect UTC offsets
#'   dt <- as_datetimeoffset(c("2020-03-08T01:59:59-08[America/New_York]",
#'                             "2020-03-08T01:59:59-05:30[America/New_York]"))
#'   clock::invalid_detect(dt)
#' }
#' @param x A [datetimeoffset()] object.
#'
NULL

#' @importFrom clock invalid_detect
#' @rdname datetimeoffset-invalid
#' @export
invalid_detect.datetimeoffset <- function(x) {
    purrr::map_lgl(x, invalid_detect_helper)
}

invalid_detect_helper <- function(x) {
    clock::invalid_detect(as_year_month_day.datetimeoffset(x)) ||
        invalid_nonexistent(x)
}

invalid_nonexistent <- function(x) {
    if (is.na(get_tz(x))) {
        FALSE
    } else if (!is.na(get_hour_offset(x))) {
        x <- fill_utc_offsets(x)
        x <- datetime_cast(x, "second")
        isTRUE(tryCatch(clock::zoned_time_parse_complete(format(x)),
                        warning = function(w) TRUE))
    } else {
        nt <- as_naive_time.datetimeoffset(x)
        isTRUE(tryCatch(clock::as_zoned_time(nt, get_tz(x),
                                             ambiguous = "NA", nonexistent = "error"),
                        error = function(e) {
                            if (inherits(e, "clock_error_nonexistent_time")) {
                                TRUE
                            } else {
                                e
                            }
                        }))
    }
}

#' @importFrom clock invalid_any
#' @rdname datetimeoffset-invalid
#' @export
invalid_any.datetimeoffset <- function(x) {
    any(invalid_detect.datetimeoffset(x))
}

#' @importFrom clock invalid_count
#' @rdname datetimeoffset-invalid
#' @export
invalid_count.datetimeoffset <- function(x) {
    sum(invalid_detect.datetimeoffset(x))
}

#' @importFrom clock invalid_remove
#' @rdname datetimeoffset-invalid
#' @export
invalid_remove.datetimeoffset <- function(x) {
    Filter(Negate(invalid_detect.datetimeoffset), x)
}
