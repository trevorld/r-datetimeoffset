#' Get/set datetime components
#'
#' S3 methods for [datetime_offset()] objects for the following
#' datetime component getter/setter functions from `lubridate`:
#'
#' * [lubridate::date()]
#' * [lubridate::year()]
#' * [lubridate::month()]
#' * [lubridate::mday()] and its alias [lubridate::day()]
#' * [lubridate::hour()]
#' * [lubridate::minute()]
#' * [lubridate::second()]
#' * [lubridate::tz()]
#'
#' @param x A [datetime_offset()] object.
#' @param value Replacement value
#' @name getset_lubridate
#' @examples
#' library("lubridate", exclude = c("date", "force_tz", "tz<-"))
#' dt <- datetime_offset(1984)
#' year(dt) <- 1918
#' year(dt)
#' month(dt) <- 11
#' month(dt)
#' day(dt) <- 11
#' day(dt)
#' hour(dt) <- 11
#' hour(dt)
#' minute(dt) <- 11
#' minute(dt)
#' second(dt) <- 11
#' second(dt)
#' if ("Europe/Paris" %in% OlsonNames()) {
#'   tz(dt) <- "Europe/Paris"
#'   tz(dt)
#' }
#'
NULL

methods::setOldClass("datetime_offset") # needed for {lubridate}'s setters

#' @rdname getset_lubridate
#' @export
date <- function(x) {
    UseMethod("date")
}

#' @rdname getset_lubridate
#' @export
date.datetime_offset <- function(x) {
    year <- field(x, "year")
    month <- field(x, "month")
    day <- field(x, "day")
    stopifnot(all(!is.na(month)), all(!is.na(day)))
    s <- sprintf("%04d-%02d-%02d", year, month, day)
    as.Date(s)
}

#' @rdname getset_lubridate
#' @importFrom lubridate date<-
methods::setMethod("date<-", "datetime_offset", function(x, value) {
    if (!inherits(value, "Date"))
        value <- as.Date(value)
    l_ymd <- strsplit(format(value), "-")
    field(x, "year") <- vapply(l_ymd, function(x) as.integer(x[1]), integer(1), USE.NAMES = FALSE)
    field(x, "month") <- vapply(l_ymd, function(x) as.integer(x[2]), integer(1), USE.NAMES = FALSE)
    field(x, "day") <- vapply(l_ymd, function(x) as.integer(x[3]), integer(1), USE.NAMES = FALSE)
    x
})

#' @importFrom lubridate year
#' @rdname getset_lubridate
#' @export
year.datetime_offset <- function(x) {
    field(x, "year")
}

#' @rdname getset_lubridate
#' @importFrom lubridate year<-
methods::setMethod("year<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "year") <- value
    x
})

#' @importFrom lubridate month
#' @rdname getset_lubridate
#' @export
month.datetime_offset <- function(x) {
    field(x, "month")
}

#' @rdname getset_lubridate
#' @importFrom lubridate month<-
methods::setMethod("month<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "month") <- value
    x
})

#' @rdname getset_lubridate
#' @importFrom lubridate mday day
#' @export
mday.datetime_offset <- function(x) {
    field(x, "day")
}

#' @rdname getset_lubridate
#' @importFrom lubridate day<-
#' @export
methods::setMethod("day<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "day") <- value
    x
})

#' @importFrom lubridate hour
#' @rdname getset_lubridate
#' @export
hour.datetime_offset <- function(x) {
    field(x, "hour")
}

#' @rdname getset_lubridate
#' @importFrom lubridate hour<-
methods::setMethod("hour<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "hour") <- value
    x
})

#' @importFrom lubridate minute
#' @rdname getset_lubridate
#' @export
minute.datetime_offset <- function(x) {
    field(x, "minute")
}

#' @rdname getset_lubridate
#' @importFrom lubridate minute<-
methods::setMethod("minute<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "minute") <- value
    x
})

#' @importFrom lubridate second
#' @rdname getset_lubridate
#' @export
second.datetime_offset <- function(x) {
    field(x, "second")
}

#' @importFrom lubridate second<-
#' @rdname getset_lubridate
methods::setMethod("second<-", "datetime_offset", function(x, value) {
    value <- as.integer(value)
    field(x, "second") <- value
    x
})


# Not a {lubridate} generic
# `force_tz()` (which is used by `tz<-`) and `with_tz()`

#' @importFrom lubridate tz
#' @rdname getset_lubridate
#' @export
tz.datetime_offset <- function(x) {
    field(x, "tz")
}

# Because `lubridate::force_tz()` is not generic must export our own `tz<-` and `force_tz()`

#' @rdname getset_lubridate
#' @export
"tz<-" <- function(x, value) {
  force_tz(x, value)
}

#' Change timezones
#'
#' `force_tz()` returns a datetime with the same clock time as the input but in the new time zone
#' (so will likely result in a different UTC datetime).
#'
#' Since `lubridate` doesn't make [lubridate::force_tz()] generic we define a generic version which
#' by default uses the `lubridate` version but has a special [datetime_offset()] method.
#' @param time A datetime object.
#' @param tzone A timezone string.
#'              `force_tz.datetime_offset()` allows a vector of different valued time zones (in contrast [lubridate::force_tz()] allows only one).
#' @param roll Used by [lubridate::force_tz()] but ignored by `force_tz.datetime_offset()`.
#' @seealso \link[=tz]{getset_lubridate} and \link[=tz<-]{getset_lubridate}.
#' @examples
#'  dt <- as_datetime_offset("1918-11-11T11:11:11")
#'  print(dt)
#'  if ("Europe/Paris" %in% OlsonNames()) {
#'    dt <- force_tz(dt, "Europe/Paris")
#'    print(dt)
#'  }
#'  # `force_tz()` doesn't change "clock" time but may change global UTC time
#'  if ("US/Pacific" %in% OlsonNames()) {
#'    dt <- force_tz(dt, "US/Pacific")
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
    tz <- as.character(tzone)
    if (isTRUE(any(tz == "")))
        tz[which(tz == "")] <- Sys.timezone()
    stopifnot(all(na_omit(tz) %in% OlsonNames()))
    field(time, "tz") <- tz
    time
}
