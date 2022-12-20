new_datetimeoffset <- function(year = integer(), month = integer(), day = integer(),
                                hour = integer(), minute = integer(), second = integer(),
                                nanosecond = integer(), subsecond_digits = integer(),
                                hour_offset = integer(), minute_offset = integer(),
                                tz = character()) {
    vec_assert(year, ptype = integer())
    vec_assert(month, ptype = integer())
    vec_assert(day, ptype = integer())
    vec_assert(hour, ptype = integer())
    vec_assert(minute, ptype = integer())
    vec_assert(second, ptype = integer())
    vec_assert(nanosecond, ptype = integer())
    vec_assert(subsecond_digits, ptype = integer())
    vec_assert(hour_offset, ptype = integer())
    vec_assert(minute_offset, ptype = integer())
    vec_assert(tz, ptype = character())

    new_rcrd(list(year = year, month = month, day = day,
                  hour = hour, minute = minute, second = second,
                  nanosecond = nanosecond, subsecond_digits = subsecond_digits,
                  hour_offset = hour_offset, minute_offset = minute_offset, tz = tz),
             class = "datetimeoffset")
}

#' Datetime object with optional UTC offsets and/or timezones
#'
#' `datetimeoffset()` creates a datetime with possible UTC offset object.
#' It can be used to represent datetimes with possible UTC offsets
#' (without necessarily any knowledge of the time zone).
#' @param year Year (integer, optional)
#' @param month Month (integer, optional)
#' @param day Day (integer, optional)
#' @param hour Hour (integer, optional)
#' @param minute Minute (integer, optional)
#' @param second Second (integer, optional)
#' @param nanosecond Nanosecond (integer, optional)
#' @param subsecond_digits Number of digits used by fractional seconds (integer, optional)
#' @param hour_offset UTC offset in hours (integer, optional)
#' @param minute_offset UTC offset in minutes (integer, optional).
#'                      Will be coerced to a non-negative value.
#' @param tz Time zone (character, optional)
#' @examples
#'   datetimeoffset(2020)
#'   datetimeoffset(2020, 5)
#'   datetimeoffset(2020, 5, 15)
#'   datetimeoffset(2020, 5, 15, 8)
#'   datetimeoffset(2020, 5, 15, 8, 23)
#'   datetimeoffset(2020, 5, 15, 8, 23, 16) # local time with unknown timezone
#'   if ("US/Pacific" %in% OlsonNames())
#'     datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "US/Pacific")
#'   datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "GMT")
#'   datetimeoffset(2020, 5, 15, 8, 23, 16, hour_offset = -7)
#'   datetimeoffset(2020, 5, 15, 8, 23, 16, hour_offset = -7, minute_offset = 30)
#' @return A `vctrs` record with class `datetimeoffset`.
#' @export
datetimeoffset <- function(year = NA_integer_, month = NA_integer_, day = NA_integer_,
                            hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
                            nanosecond = NA_integer_, subsecond_digits = NA_integer_,
                            hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_) {
    if (nargs() == 0L) {
        return(new_datetimeoffset(integer(0L), integer(0L), integer(0L),
                                  integer(0L), integer(0L), integer(0L),
                                  integer(0L), integer(0L),
                                  integer(0L), integer(0L), character(0L)))
    }
    # cast
    year <- vec_cast(year, integer())
    month <- vec_cast(month, integer())
    day <- vec_cast(day, integer())
    hour <- vec_cast(hour, integer())
    minute <- vec_cast(minute, integer())
    second <- vec_cast(second, integer())
    nanosecond <- vec_cast(nanosecond, integer())
    subsecond_digits <- vec_cast(subsecond_digits, integer())
    hour_offset <- vec_cast(hour_offset, integer())
    minute_offset <- vec_cast(minute_offset, integer())
    tz <- vec_cast(tz, character())

    # misc.
    if (isTRUE(any(tz == "")))
        tz[which(tz == "")] <- Sys.timezone()
    stopifnot(all(na_omit(tz) %in% OlsonNames()))
    minute_offset <- abs(minute_offset)

    # assert bounds
    assert_bounds(month, 1L, 12L)
    assert_bounds(day, 1L, 31L)
    assert_bounds(hour, 0L, 24L)
    assert_bounds(minute, 0L, 60L)
    assert_bounds(second, 0L, 61L) # leap seconds
    assert_bounds(nanosecond, 0L, .Machine$integer.max)
    assert_bounds(subsecond_digits, 0L, 9L)
    assert_bounds(hour_offset, -12L, 14L)
    assert_bounds(minute_offset, 0L, 60L)

    # recycle
    rc <- vec_recycle_common(year, month, day,
                             hour, minute, second,
                             nanosecond, subsecond_digits,
                             hour_offset, minute_offset, tz)
    year <- rc[[1]]
    month <- rc[[2]]
    day <- rc[[3]]
    hour <- rc[[4]]
    minute <- rc[[5]]
    second <- rc[[6]]
    nanosecond <- rc[[7]]
    subsecond_digits <- rc[[8]]
    hour_offset <- rc[[9]]
    minute_offset <- rc[[10]]
    tz <- rc[[11]]

    new_datetimeoffset(year, month, day,
                        hour, minute, second,
                        nanosecond, subsecond_digits,
                        hour_offset, minute_offset, tz)
}

na_omit <- function(x) Filter(Negate(is.na), x)

#' Various "datetimeoffset" object utilities
#'
#' `is_datetimeoffset()` tests whether a datetime object is of the "datetimeoffset" class.
#' `NA_datetimeoffset_` provides a "missing" "datetimeoffset" object.
#' `datetimeoffset_now()` returns the current time in the corresponding time zone(s).
#' @param x An object to be tested
#' @param tz Time zone(s)
#' @return `is_datetimeoffset()` returns a logical vector.
#'         `datetimeoffset_now()` returns a [datetimeoffset()] vector.
#'
#' @examples
#'   is_datetimeoffset(as_datetimeoffset(Sys.time()))
#'   is_datetimeoffset(Sys.time())
#'
#'   is.na(NA_datetimeoffset_)
#'   is.na(as_datetimeoffset(""))
#'
#'   if (all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
#'     datetimeoffset_now(c("America/Los_Angeles", "America/New_York"))
#'
#' @name datetimeoffset_utilities
NULL

#' @rdname datetimeoffset_utilities
#' @export
is_datetimeoffset <- function(x) inherits(x, "datetimeoffset")

#' @rdname datetimeoffset_utilities
#' @export
NA_datetimeoffset_ <- new_datetimeoffset(NA_integer_, NA_integer_, NA_integer_,
                                         NA_integer_, NA_integer_, NA_integer_,
                                         NA_integer_, NA_integer_,
                                         NA_integer_, NA_integer_, NA_character_)

#' @rdname datetimeoffset_utilities
#' @export
datetimeoffset_now <- function(tz = Sys.timezone()) {
    st <- clock::sys_time_now()
    tz <- clean_tz(tz, Sys.timezone())
    purrr::map_vec(tz,
                   function(x) as_datetimeoffset(clock::as_zoned_time(st, x)),
                   .ptype = datetimeoffset())
}

#' @export
vec_ptype_abbr.datetimeoffset <- function(x, ...) "dto"

#' @export
vec_ptype_full.datetimeoffset <- function(x, ...) "datetimeoffset"

#' @export
vec_proxy_equal.datetimeoffset <- function(x, ...) format_edtf(x, precision = "nanosecond", usetz = TRUE)

#' @export
is.na.datetimeoffset <- function(x) is.na(get_year(x)) & is.na(get_month(x)) & is.na(get_day(x)) &
    is.na(get_hour(x)) & is.na(get_minute(x)) & is.na(get_second(x)) &
    is.na(get_nanosecond(x)) & is.na(get_subsecond_digits(x)) &
    is.na(get_hour_offset(x)) & is.na(get_minute_offset(x)) & is.na(get_tz(x))
