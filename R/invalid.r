#' Invalid datetimeoffset datetimes
#'
#' `invalid_detect()` detects invalid datetimes.
#' `invalid_any()` returns TRUE if any datetimes are invalid.
#' `invalid_count()` returns number of invalid datetimes.
#' `invalid_remove()` removes invalid datetimes.
#' `invalid_resolve()` resolves invalid datetimes.
#'
#' `datetimeoffset()` datetimes can be considered invalid for three main reasons:
#'
#'  1. An invalid "calendar date" such as `"2020-02-30"` (there are less than 30 days in February).
#'  2. A "nonexistent" datetime due to a Daylight Savings Time "spring forward" such as `"2020-03-08T02:59:59[America/Los_Angeles]"`
#'  3. Incorrect UTC offsets such as `"2020-03-08T01:59:59-08[America/New_York]"` (that particular Eastern time has a UTC offset of `-05`)
#'
#' @return `invalid_detect()`, `invalid_remove()`, and `invalid_resolve()` return [datetimeoffset()] vectors.
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
#' clock::invalid_resolve(dts)
#' clock::invalid_resolve(dts, invalid = "previous")
#' clock::invalid_resolve(dts, invalid = "previous-day")
#'
#' # non-existent time because of DST "spring forward"
#' if ("America/Los_Angeles" %in% OlsonNames()) {
#'   dt <- as_datetimeoffset("2020-03-08T02:59:59[America/Los_Angeles]")
#'   print(clock::invalid_detect(dt))
#'   clock::invalid_resolve(dt, nonexistent = "roll-forward")
#' }
#'
#' # incorrect UTC offsets
#' if ("America/New_York" %in% OlsonNames()) {
#'   dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/New_York]")
#'   print(clock::invalid_detect(dt))
#'   clock::invalid_resolve(dt)
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
    if (get_second(x) == 60L) # {clock} doesn't handle leap seconds
        x <- set_second(x, 59L)
    invalid_detect_calendar(x) ||
        invalid_detect_utc_offsets(x) ||
        invalid_detect_nonexistent(x)
}

invalid_detect_calendar <- function(x) {
    clock::invalid_detect(as_year_month_day.datetimeoffset(x))
}

invalid_detect_utc_offsets <- function(x) {
    tz <- get_tz(x)
    if (!is.na(get_hour_offset(x)) && !is.na(tz)) {
        x <- fill_utc_offsets(x)
        x <- datetime_cast(x, "second")
        if (is_utc(tz)) {
            get_hour_offset(x) != 0L || isFALSE(get_minute_offset(x) == 0L)
        } else {
            isTRUE(tryCatch(clock::zoned_time_parse_complete(format(x)),
                            warning = function(w) TRUE))
        }
    } else {
        FALSE
    }
}

invalid_detect_nonexistent <- function(x) {
    if (!is.na(get_tz(x)) && is.na(get_hour_offset(x))) {
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
    } else {
        FALSE
    }
}

#' @importFrom clock invalid_resolve
#' @param invalid Invalid date resolution strategy.  See [clock::invalid_resolve()].
#' @param nonexistent Nonexistent (because of DST spring forward) time resolution strategy.
#'                See [clock::as_zoned_time.clock_naive_time()].
#' @param ... Ignored.
#' @rdname datetimeoffset-invalid
#' @export
invalid_resolve.datetimeoffset <- function(x, ..., invalid = "NA", nonexistent = "NA") {
    idx <- which(invalid_detect.datetimeoffset(x))
    if (length(idx) > 0L) {
        x[idx] <- purrr::map_vec(x[idx], invalid_resolve_helper,
                                 invalid = invalid, nonexistent = nonexistent,
                                 .ptype = datetimeoffset())
    }
    x
}

invalid_resolve_helper <- function(x, ...,
                                   invalid = "NA", nonexistent = "NA") {
    precision <- datetime_precision.datetimeoffset(x)
    if (invalid_detect_calendar(x)) {
        ymd <- as_year_month_day.datetimeoffset(x)
        ymd <- clock::invalid_resolve(ymd, invalid = invalid)
        x <- as_datetimeoffset.clock_year_month_day(ymd)
    }
    if (invalid_detect_utc_offsets(x)) {
        x <- set_hour_offset(x, NA_integer_)
        x <- set_minute_offset(x, NA_integer_)
        x <- fill_utc_offsets(x)
    }
    if (invalid_detect_nonexistent(x)) {
        zt <- as_zoned_time.datetimeoffset(x, get_tz(x), nonexistent = nonexistent)
        x <- as_datetimeoffset.clock_zoned_time(zt)
    }
    datetime_cast(x, precision)
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
