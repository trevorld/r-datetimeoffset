new_datetime_offset <- function(year = integer(), month = integer(), day = integer(),
                                hour = integer(), minute = integer(), second = integer(),
                                nanosecond = integer(),
                                hour_offset = integer(), minute_offset = integer(), tz = character()) {
    vec_assert(year, ptype = integer())
    vec_assert(month, ptype = integer())
    vec_assert(day, ptype = integer())
    vec_assert(hour, ptype = integer())
    vec_assert(minute, ptype = integer())
    vec_assert(second, ptype = integer())
    vec_assert(nanosecond, ptype = integer())
    vec_assert(hour_offset, ptype = integer())
    vec_assert(minute_offset, ptype = integer())
    vec_assert(tz, ptype = character())

    new_rcrd(list(year = year, month = month, day = day,
                  hour = hour, minute = minute, second = second, nanosecond = nanosecond,
                  hour_offset = hour_offset, minute_offset = minute_offset, tz = tz),
             class = "datetime_offset")
}

#' Datetime with possible UTC offset object
#'
#' `datetime_offset()` creates a datetime with possible UTC offset object.
#' It can be used to represent datetimes with possible UTC offsets
#' (without necessarily any knowledge of the time zone).
#' @param year Year (integer, mandatory)
#' @param month Month (integer, optional)
#' @param day Day (integer, optional)
#' @param hour Hour (integer, optional)
#' @param minute Minute (integer, optional)
#' @param second Second (integer, optional)
#' @param nanosecond Nanosecond (integer, optional)
#' @param hour_offset UTC offset in hours (integer, optional)
#' @param minute_offset UTC offset in minutes (integer, optional).
#'                      Will be coerced to a non-negative value.
#' @param tz Time zone (character, optional)
#' @examples
#'   datetime_offset(2020)
#'   datetime_offset(2020, 5)
#'   datetime_offset(2020, 5, 15)
#'   datetime_offset(2020, 5, 15, 8)
#'   datetime_offset(2020, 5, 15, 8, 23)
#'   datetime_offset(2020, 5, 15, 8, 23, 16) # local time with unknown timezone
#'   if ("US/Pacific" %in% OlsonNames())
#'     datetime_offset(2020, 5, 15, 8, 23, 16, tz = "US/Pacific")
#'   datetime_offset(2020, 5, 15, 8, 23, 16, tz = "GMT")
#'   datetime_offset(2020, 5, 15, 8, 23, 16, hour_offset = -7)
#'   datetime_offset(2020, 5, 15, 8, 23, 16, hour_offset = -7, minute_offset = 30)
#' @return A `vctrs` record with class `datetime_offset`.
#' @export
datetime_offset <- function(year, month = NA_integer_, day = NA_integer_,
                            hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
                            nanosecond = NA_integer_,
                            hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_) {
    # cast
    year <- vec_cast(year, integer())
    month <- vec_cast(month, integer())
    day <- vec_cast(day, integer())
    hour <- vec_cast(hour, integer())
    minute <- vec_cast(minute, integer())
    second <- vec_cast(second, integer())
    nanosecond <- vec_cast(nanosecond, integer())
    hour_offset <- vec_cast(hour_offset, integer())
    minute_offset <- vec_cast(minute_offset, integer())
    tz <- vec_cast(tz, character())

    # misc.
    if (isTRUE(any(tz == "")))
        tz[which(tz == "")] <- Sys.timezone()
    stopifnot(all(na_omit(tz) %in% OlsonNames()))
    minute_offset <- abs(minute_offset)

    # recycle
    rc <- vec_recycle_common(year, month, day,
                             hour, minute, second, nanosecond,
                             hour_offset, minute_offset, tz)
    year <- rc[[1]]
    month <- rc[[2]]
    day <- rc[[3]]
    hour <- rc[[4]]
    minute <- rc[[5]]
    second <- rc[[6]]
    nanosecond <- rc[[7]]
    hour_offset <- rc[[8]]
    minute_offset <- rc[[9]]
    tz <- rc[[10]]

    new_datetime_offset(year, month, day,
                        hour, minute, second, nanosecond,
                        hour_offset, minute_offset, tz)
}

#' @export
vec_ptype_abbr.datetime_offset <- function(x, ...) "dt_os"

#' @export
vec_ptype_full.datetime_offset <- function(x, ...) "datetime_offset"
