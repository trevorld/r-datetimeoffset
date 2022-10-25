#' Get most common time zone
#'
#' 'mode_tz()' gets the most common time zone
#' in the datetime object.  If a tie we use the time zone used first.
#' Intended for use when coercing from a datetime object that supports
#' multiple heterogeneous time zones to a datetime object that
#' only supports one time zone
#' @param time A datetime object.
#' @param tzone A timezone string to use for missing time zones.
#' @param ... Ignored
#' @return Timezone string
#' @examples
#'   dt <- as_datetimeoffset(Sys.time())
#'   print(mode_tz(dt))
#'   if (all(c("US/Pacific", "US/Eastern") %in% OlsonNames())) {
#'     dt <- as_datetimeoffset("2020-01-01",
#'                              tz = c("US/Pacific", "US/Eastern"))
#'     print(mode_tz(dt))
#'
#'     dt <- as_datetimeoffset("2020-01-01",
#'                              tz = c("US/Pacific", "US/Eastern", NA_character_, NA_character_))
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
    time <- set_zone(time, ifelse(is.na(get_zone(time)), tzone, get_zone(time)))
    time <- set_zone(time, ifelse(get_zone(time) == "", Sys.timezone(), get_zone(time)))

    tzones <- get_zone(time)
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
