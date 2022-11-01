#' Get most common time zone
#'
#' 'mode_tz()' gets the most common time zone
#' in the datetime object.  If a tie we use the time zone used first.
#' Intended for use when coercing from a datetime object that supports
#' multiple heterogeneous time zones to a datetime object that
#' only supports one time zone
#' @param time A datetime object.
#' @param tzone A timezone string to use for missing time zones.
#'              "" will be treated as equivalent to `Sys.timezone()`.
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
mode_tz <- function(time, tzone = "", ...) {
    UseMethod("mode_tz")
}

#' @rdname mode_tz
#' @export
mode_tz.datetimeoffset <- function(time, tzone = "", ...) {
    tzone <- clean_tz(tzone)
    time <- set_tz(time, ifelse(is.na(get_tz(time)), tzone, get_tz(time)))
    time <- set_tz(time, ifelse(get_tz(time) == "", Sys.timezone(), get_tz(time)))

    tzones <- get_tz(time)
    keys <- unique(tzones)
    tbl <- tabulate(match(tzones, keys))
    keys[which.max(tbl)]
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

# is_valid_tz <- function(tz) {
#     all(na_omit(tz) %in% OlsonNames())
# }
