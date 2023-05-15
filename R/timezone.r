#' Get most common time zone
#'
#' 'mode_tz()' gets the most common time zone
#' in the datetime object.  If a tie we use the time zone used first.
#' Intended for use when coercing from a datetime object that supports
#' multiple heterogeneous time zones to a datetime object that
#' only supports one time zone
#' @param x A datetime object.
#' @param tz A timezone string to use for missing time zones.
#'           "" will be treated as equivalent to `Sys.timezone()`.
#' @param ... Ignored
#' @return Timezone string
#' @examples
#'   dt <- as_datetimeoffset(Sys.time())
#'   print(mode_tz(dt))
#'   if (all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames())) {
#'     dt <- as_datetimeoffset("2020-01-01",
#'                              tz = c("America/Los_Angeles", "America/New_York"))
#'     print(mode_tz(dt))
#'
#'     print(Sys.timezone()) # timezone to be used for missing time zones
#'     dt <- as_datetimeoffset("2020-01-01",
#'                              tz = c("America/New_York", NA_character_, NA_character_))
#'     print(mode_tz(dt))
#'   }
#' @export
mode_tz <- function(x, ...) {
    UseMethod("mode_tz")
}

#' @rdname mode_tz
#' @export
mode_tz.datetimeoffset <- function(x, tz = "", ...) {
    tz <- clean_tz(tz)
    x <- set_tz(x, ifelse(is.na(get_tz(x)), tz, get_tz(x)))
    x <- set_tz(x, ifelse(get_tz(x) == "", Sys.timezone(), get_tz(x)))

    tzones <- na_omit(get_tz(x))
    keys <- unique(tzones)
    tbl <- tabulate(match(tzones, keys))
    value <- keys[which.max(tbl)]
    if (is.na(value))
        tz
    else
        value
}

#' @rdname mode_tz
#' @export
mode_tz.default <- function(x, ...) {
    tz <- get_tz(x)
    ifelse(tz == "", Sys.timezone(), tz)
}

#' Get/set UTC offset strings
#'
#' `get_utc_offsets()` and `set_utc_offsets()` gets/sets UTC offset strings
#' @param x A [datetimeoffset()] object
#' @param sep Separator between hour and minute offsets.  Either ":" or "".
#' @param value Replace UTC offset string
#' @name getset_utc_offsets
#' @return `get_utc_offsets()` returns a character string of UTC offset info.
#'         `set_utc_offsets()` returns a datetime (whose UTC offset info has been set).
#' @seealso [get_hour_offset()], [set_hour_offset()], [get_minute_offset()], and [set_minute_offset()]
#'          allow getting/setting the separate individual hour/minute offset components with integers.
#'          [fill_utc_offsets()] fills any missing UTC offsets using non-missing time zones.
#' @examples
#'   dt <- as_datetimeoffset("2020-01-01T01:01")
#'   get_utc_offsets(dt)
#'   dt <- set_utc_offsets(dt, "-07:00")
#'   get_utc_offsets(dt)
#'   dt <- set_utc_offsets(dt, "+0800")
#'   get_utc_offsets(dt)
#'   dt <- set_utc_offsets(dt, "+00")
#'   get_utc_offsets(dt)
#'   dt <- set_utc_offsets(dt, NA_character_)
#'   get_utc_offsets(dt)
#' @export
get_utc_offsets <- function(x, sep = ":") {
    stopifnot(sep %in% c(":", ""))
    if (inherits(x, "POSIXt")) {
        if (sep == ":")
            clock::date_format(x, format = "%Ez")
        else
            clock::date_format(x, format = "%z")
    }
    hour_offset <- get_hour_offset(x)
    ho <- my_format(hour_offset, width = 3, flag = "+0")
    mo <- my_format(get_minute_offset(x), prefix = sep)
    s <- paste0(ho, mo)
    is.na(s) <- is.na(hour_offset)
    s
}

#' @rdname getset_utc_offsets
#' @export
set_utc_offsets <- function(x, value) {
    stopifnot(is.character(value))
    stopifnot(all(na_omit(nchar(value)) %in% c(3L, 5L, 6L)))
    ho <- purrr::map_int(value, function(v) {
        dto_as_integer(substr(v, 1, 3))
    })
    mo <- purrr::map_int(value, function(v) {
        if (isTRUE(nchar(v) > 3L))
            dto_as_integer(substr(v, nchar(v) - 1L, nchar(v)))
        else
            NA_integer_
    })
    x <- set_hour_offset(x, ho)
    x <- set_minute_offset(x, mo)
    x
}

#' Fill in missing time zones and/or UTC offsets
#'
#' `fill_tz()` fills in missing time zones.
#' `fill_utc_offsets()` fills in missing UTC offsets.
#'
#' @param x A datetime object
#' @param tz Timezone used to fill in missing time zones
#' @examples
#' dts <- as_datetimeoffset(c("2020-01-01T01:01:01", "2020-01-01T01:01:01Z"))
#' fill_tz(dts, "UTC")
#' fill_tz(dts, Sys.timezone())
#' clock::as_sys_time(dts)
#' clock::as_sys_time(fill_tz(dts, "UTC"))
#' clock::as_zoned_time(dts)
#' clock::as_zoned_time(fill_tz(dts, ""))
#'
#' if ("America/New_York" %in% OlsonNames()) {
#'   # non-ambiguous UTC offsets
#'   dt <- as_datetimeoffset("2020-11-01T12:30:00[America/New_York]")
#'   cat("unfilled: ", get_utc_offsets(dt), "\n")
#'   dt <- fill_utc_offsets(dt)
#'   cat("filled: ", get_utc_offsets(dt), "\n")
#'
#'   # ambiguous UTC offsets due to DST
#'   dt0 <- as_datetimeoffset("2020-11-01T01:30:00[America/New_York]")
#'   dt <- fill_utc_offsets(dt0)
#'   cat('`ambiguous = "NA"` (default): ', get_utc_offsets(dt), "\n")
#'   dt <- fill_utc_offsets(dt0, ambiguous = "earliest")
#'   cat('`ambiguous = "earliest"`: ', get_utc_offsets(dt), "\n")
#'   dt <- fill_utc_offsets(dt0, ambiguous = "latest")
#'   cat('`ambiguos = "latest"`: ', get_utc_offsets(dt), "\n")
#' }
#' @return A datetime object
#' @export
fill_tz <- function(x, tz = "") {
    set_tz(x, ifelse(is.na(get_tz(x)) & is.na(get_hour_offset(x)), clean_tz(tz), get_tz(x)))
}

#' @rdname fill_tz
#' @inheritParams as_zoned_time.datetimeoffset
#' @export
fill_utc_offsets <- function(x, ambiguous = "NA") {
    # Fill missing minute offset to zero if hour offset is not missing
    x <- set_minute_offset(x, ifelse(!is.na(get_hour_offset(x)) & is.na(get_minute_offset(x)),
                                     0L,
                                     get_minute_offset(x)))

    tz <- get_tz(x)
    id_tz <- which(!is.na(tz) & is.na(get_hour_offset(x)))
    if (length(id_tz) > 0L) {
        df <- data.frame(x = as_ymd_hms_str(x[id_tz]), tz = tz[id_tz],
                         stringsAsFactors = FALSE)
        offsets <- purrr::pmap_chr(df, function(x, tz) {
                                       dt <- suppressWarnings(clock::naive_time_parse(x))
                                       dt <- clock::as_zoned_time(dt, tz,
                                                                  ambiguous = ambiguous, nonexistent = "error")
                                       format(dt, format = "%z")
                                   })
        x[id_tz] <- set_hour_offset(x[id_tz], as.integer(substr(offsets, 1, 3)))
        x[id_tz] <- set_minute_offset(x[id_tz], as.integer(substr(offsets, 4, 5)))
    }
    x
}

clean_tz <- function(tz, na = NA_character_) {
    tz <- as.character(tz)
    if (isTRUE(any(tz == "")))
        tz[which(tz == "")] <- Sys.timezone()
    if (any(is.na(tz)))
        tz[which(is.na(tz))] <- na
    # stopifnot(is_valid_tz(tz))
    tz
}

#' Change time zones while preserving UTC time
#'
#' `datetime_at_tz()` changes time zones while preserving UTC time (instead of clock time).
#'
#' @param x A datetime object.
#' @param tz The target timezone to change to.
#' @inheritParams as_zoned_time.datetimeoffset
#' @examples
#' if(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames())) {
#'   dt0 <- as_datetimeoffset("2020-01-01T01:01[America/Los_Angeles]")
#'   dt <- datetime_at_tz(dt0, "America/New_York")
#'   print(dt)
#'   dt <- datetime_at_tz(as.POSIXct(dt0), "America/New_York")
#'   print(dt)
#'   dt <- datetime_at_tz(clock::as_zoned_time(dt0), "America/New_York")
#'   print(dt)
#'
#'   # Can also use `lubridate::with_tz()`
#'   if (requireNamespace("lubridate")) {
#'     dt <- lubridate::with_tz(dt0, "America/New_York")
#'     print(dt)
#'   }
#' }
#' @return A datetime object.  The UTC time should be the same but with a different time zone.
#' @seealso [set_tz()] changes time zones while preserving clock time (instead of UTC time).
#' @export
datetime_at_tz <- function(x, tz = "", ...) {
    UseMethod("datetime_at_tz")
}

#' @rdname datetime_at_tz
#' @export
datetime_at_tz.datetimeoffset <- function(x, tz = "", ...,
                                             ambiguous = "error", nonexistent = "error",
                                             fill = NA_character_) {
    tz <- clean_tz(tz, na = NA_character_)
    n <- max(length(x), length(tz))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(tz) < n)
        tz <- rep(tz, length.out = n)
    df <- data.frame(dt = x, tz = tz, stringsAsFactors = FALSE)
    purrr::pmap_vec(df, with_tz_helper,
                    ambiguous = ambiguous, nonexistent = nonexistent, fill = fill,
                    .ptype = datetimeoffset())
}

#' @rdname datetime_at_tz
#' @export
datetime_at_tz.clock_zoned_time <- function(x, tz = "", ...) {
    clock::zoned_time_set_zone(x, tz)
}

#' @rdname datetime_at_tz
#' @export
datetime_at_tz.POSIXt <- function(x, tz = "", ...) {
    clock::date_time_set_zone(x, tz)
}

#' @rdname datetime_at_tz
#' @export
datetime_at_tz.default <- function(x, tz = "", ...) {
    assert_suggested("lubridate")
    lubridate::with_tz(x, tz)
}

with_tz_helper <- function(dt, tz, ambiguous = "error", nonexistent = "error", fill = NA_character_) {
    st <- as_sys_time.datetimeoffset(dt, ambiguous = ambiguous, nonexistent = nonexistent, fill = fill)
    dto <- as_datetimeoffset.clock_zoned_time(clock::as_zoned_time(st, tz))
    datetime_narrow.datetimeoffset(dto, datetime_precision.datetimeoffset(dt))
}

with_tz.datetimeoffset <- function(x, tzone = "", ...,
                                   ambiguous = "error", nonexistent = "error", fill = NA_character_) {
    datetime_at_tz.datetimeoffset(x, tzone,
                                     ambiguous = ambiguous, nonexistent = nonexistent, fill = fill)
}

# is_valid_tz <- function(tz) {
#     all(na_omit(tz) %in% OlsonNames())
# }
