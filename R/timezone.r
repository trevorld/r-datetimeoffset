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

#' Fill in missing time zones and/or UTC offsets
#'
#' `fill_tz()` fills in missing time zones.
#' `fill_offsets()` fills in missing UTC offsets.
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
#'   get_offsets <- function(x) {
#'     paste(formatC(get_hour_offset(x), width = 3, format = "d", flag = "+0"),
#'           formatC(get_minute_offset(x), width = 2, format = "d", flag = "0"),
#'           sep = ":")
#'   }
#'   # non-ambiguous UTC offsets
#'   dt <- as_datetimeoffset("2020-11-01T12:30:00[America/New_York]")
#'   cat("unfilled: ", get_offsets(dt), "\n")
#'   dt <- fill_offsets(dt)
#'   cat("filled: ", get_offsets(dt), "\n")
#'
#'   # ambiguous UTC offsets due to DST
#'   dt0 <- as_datetimeoffset("2020-11-01T01:30:00[America/New_York]")
#'   dt <- fill_offsets(dt0)
#'   cat('`ambiguous = "NA"` (default): ', get_offsets(dt), "\n")
#'   dt <- fill_offsets(dt0, ambiguous = "earliest")
#'   cat('`ambiguous = "earliest"`: ', get_offsets(dt), "\n")
#'   dt <- fill_offsets(dt0, ambiguous = "latest")
#'   cat('`ambiguos = "latest"`: ', get_offsets(dt), "\n")
#' }
#' @return A datetime object
#' @export
fill_tz <- function(x, tz = "") {
    set_tz(x, ifelse(is.na(get_tz(x)) & is.na(get_hour_offset(x)), clean_tz(tz), get_tz(x)))
}

#' @rdname fill_tz
#' @inheritParams as_sys_time.datetimeoffset
#' @export
fill_offsets <- function(x, ambiguous = "NA") {
    # Fill missing minute offset to zero if hour offset is not missing
    x <- set_minute_offset(x, ifelse(!is.na(get_hour_offset(x)) & !is.na(get_minute_offset(x)),
                                     0L,
                                     get_minute_offset(x)))

    tz <- get_tz(x)
    id_tz <- which(!is.na(tz) & is.na(get_hour_offset(x)))
    if (length(id_tz) > 0L) {
        df <- data.frame(x = as_ymd_hms_str(x[id_tz]), tz = tz[id_tz],
                         stringsAsFactors = FALSE)
        offsets <- purrr::pmap_chr(df, function(x, tz) {
                                       dt <- clock::naive_time_parse(x)
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
#' @inheritParams as_sys_time.datetimeoffset
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
#'   if (require("lubridate")) {
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
    clock::date_set_zone(x, tz)
}

#' @rdname datetime_at_tz
#' @export
datetime_at_tz.default <- function(x, tz = "", ...) {
    assert_suggested("lubridate")
    lubridate::with_tz(x, tz)
}

with_tz_helper <- function(dt, tz, ambiguous = "error", nonexistent = "error", fill = NA_character_) {
    st <- as_sys_time_dto(dt, ambiguous = ambiguous, nonexistent = nonexistent, fill = fill)
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
