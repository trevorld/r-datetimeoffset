#' Change timezones
#'
#' `force_tz()` returns a datetime with the same clock time as the input but in the new time zone
#' (so will likely result in a different global UTC datetime).
#' `with_tz()` returns a datetime with the same global UTC datetime as the input but in the new time zone.
#'
#' Since `lubridate` doesn't make [lubridate::force_tz()] or [lubridate::with_tz()] generic we
#' define a generic version which
#' by default uses the `lubridate` version but also has a special [datetime_offset()] method.
#' @param time A datetime object.
#' @param tzone A timezone string.
#'              `force_tz.datetime_offset()` allows a vector of different valued time zones (in contrast [lubridate::force_tz()] allows only one).
#' @param ... In `with_tz.datetime_format()` passed to \code{\link[=from_datetime_offset]{as.nanotime()}}.
#' @param roll Used by [lubridate::force_tz()] but ignored by `force_tz.datetime_offset()`.
#' @seealso \link[=tz]{getset_lubridate} and \link[=tz<-]{getset_lubridate}.
#' @examples
#'  dt0 <- as_datetime_offset("1918-11-11T11:11:11", tz = "GMT")
#'  print(dt0)
#'  if ("US/Pacific" %in% OlsonNames()) {
#'    # `force_tz()` doesn't change "clock" time but may change global UTC time
#'    dt <- force_tz(dt0, "US/Pacific")
#'    print(dt)
#'    # `with_tz()` doesn't change global UTC time but my change "clock" time
#'    dt <- with_tz(dt0, "US/Pacific")
#'    print(dt)
#'  }
#' @name timezone
NULL


# Because `lubridate::force_tz()` is not generic must export our own `tz<-` and `force_tz()`

#' @rdname timezone
#' @export
force_tz <- function(time, tzone = "", roll = FALSE) {
    UseMethod("force_tz")
}

#' @rdname timezone
#' @export
force_tz.default <- function(time, tzone = "", roll = FALSE) {
    lubridate::force_tz(time, tzone, roll)
}

#' @rdname timezone
#' @export
force_tz.datetime_offset <- function(time, tzone = "", roll = FALSE) {
    tzone <- clean_tz(tzone)
    field(time, "tz") <- tzone
    time
}

#' @rdname timezone
#' @export
with_tz <- function(time, tzone = "", ...) {
    UseMethod("with_tz")
}

#' @rdname timezone
#' @export
with_tz.default <- function(time, tzone = "", ...) {
    lubridate::with_tz(time, tzone)
}

#' @rdname timezone
#' @export
with_tz.datetime_offset <- function(time, tzone = "", ...) {
    tzone <- clean_tz(tzone)
    as_datetime_offset(as.nanotime(time, ...), tz = tzone)
}

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
#'   dt <- as_datetime_offset(Sys.time())
#'   print(mode_tz(dt))
#'   if (all(c("US/Pacific", "US/Eastern") %in% OlsonNames())) {
#'     dt <- as_datetime_offset("2020-01-01",
#'                              tz = c("US/Pacific", "US/Eastern"))
#'     print(mode_tz(dt))
#'
#'     dt <- as_datetime_offset("2020-01-01",
#'                              tz = c("US/Pacific", "US/Eastern", NA_character_, NA_character_))
#'     print(mode_tz(dt))
#'   }
#' @export
mode_tz <- function(time, tzone = "", ...) {
    UseMethod("mode_tz")
}

#' @rdname mode_tz
#' @export
mode_tz.datetime_offset <- function(time, tzone = "", ...) {
    tzone <- clean_tz(tzone)
    tz(time) <- ifelse(is.na(tz(time)), tzone, tz(time))
    tz(time) <- ifelse(tz(time) == "", Sys.timezone(), tz(time))

    tzones <- tz(time)
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
