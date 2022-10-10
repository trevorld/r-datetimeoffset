#' Coerce to datetime with possible UTC offset object
#'
#' `as_datetime_offset()` coerces to [datetime_offset()] objects.
#'
#' @param x An R object that can be coerced to a [datetime_offset()] object
#'          such as a string in pdfmark date or ISO 8601 datetime formats
#'          or something with an [base::as.POSIXlt()] method.
#' @param tz Time zone to use for the conversion.
#' @param ... Further arguments to certain methods.
#' @examples
#' # ISO 8601 examples
#' as_datetime_offset("2020-05-15")
#' as_datetime_offset("20200515")
#' as_datetime_offset("2020-05-15T08:23:16")
#' as_datetime_offset("20200515T082316")
#' as_datetime_offset("2020-05-15T08:23:16Z")
#' as_datetime_offset("20200515T082316Z")
#' as_datetime_offset("2020-05-15T08:23:16+03:30")
#' as_datetime_offset("20200515T082316+0330")
#'
#' # Misc supported `as.POSIXlt()` `tryFormats` examples
#' as_datetime_offset("2020/05/15 08:23:16")
#'
#' # pdfmark datetime examples
#' as_datetime_offset("D:20200515")
#' as_datetime_offset("D:20200515082316")
#' as_datetime_offset("D:20200515082316+0330")
#' @export
as_datetime_offset <- function(x, tz = "", ...) {
    UseMethod("as_datetime_offset")
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.Date <- function(x, tz = "", ...) {
    as_datetime_offset(format(x))
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.default <- function(x, tz = "", ...) {
    as_datetime_offset(format(x, tz = tz, ...))
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.character <- function(x, tz = NA_character_, ...) {
    l <- lapply(x, as_dtos_character_helper)
    df <- do.call(rbind, l)
    tz <- ifelse(is.na(df$tz), tz, df$tz)
    datetime_offset(df$year, df$month, df$day,
                    df$hour, df$minute, df$second,
                    df$hour_offset, df$minute_offset, tz)
}

as_dtos_character_helper <- function(x) {
    if (grepl("^D:[[:digit:]+-]{4,}$", x)) # pdfmark prefix
        s <- substr(x, 3L, nchar(x))
    else
        s <- x
    l <- list(year = NA_integer_, month = NA_integer_, day = NA_integer_,
              hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
              hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_)
    # "2020-05-15T08:23:16-07:00"
    if (grepl("^[[:digit:]]{4}$", s)) { # "2020"
        l$year <- as.integer(s)
    } else if (grepl("^[[:digit:]]{6}$", s)) { # "202005"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}$", s)) { # "2020-05"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
    } else if (grepl("^[[:digit:]]{8}$", s)) { # "20200515"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}$", s)) { # "2020-05-15"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]]{10}$", s)) { # "2020051508"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}$", s)) { # "2020-05-15T08"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{2}$", s)) { # "20200515T08"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
    } else if (grepl("^[[:digit:]]{12}$", s)) { # "202005150823"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}$", s)) { # "20200515T0823"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}Z$", s)) { # "2020-05-15T08:23Z"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}Z$", s)) { # "20200515T0823Z"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08:23-07"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$hour_offset <- as.integer(substr(s, 17L, 19L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}[+-][[:digit:]]{2}$", s)) { # "20200515T0823-07"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$hour_offset <- as.integer(substr(s, 14L, 16L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}[+-][[:digit:]]{4}$", s)) { # "20200515T0823-0700"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$hour_offset <- as.integer(substr(s, 14L, 16L))
        l$minute_offset <- as.integer(substr(s, 17L, 18L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}[[:digit:]]{2}$", s)) { # "2020-05-15T08:23-0700"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$hour_offset <- as.integer(substr(s, 17L, 19L))
        l$minute_offset <- as.integer(substr(s, 20L, 21L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23-07:00"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$hour_offset <- as.integer(substr(s, 17L, 19L))
        l$minute_offset <- as.integer(substr(s, 21L, 22L))
    } else if (grepl("^[[:digit:]]{14}$", s)) { # "20200515082316"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}$", s)) { # "2020-05-15T08:23:16"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}$", s)) { # "20200515T082316"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$second <- as.integer(substr(s, 14L, 15L))
    } else if (grepl("^[[:digit:]]{14}Z$", s)) { # "20200515082316Z"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}Z$", s)) { # "2020-05-15T08:23:16Z"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}Z$", s)) { # "20200515T082316Z"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$second <- as.integer(substr(s, 14L, 15L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{14}[+-][[:digit:]]{2}$", s)) { # "20200515082316-07"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
        l$hour_offset <- as.integer(substr(s, 15L, 17L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16-07"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
        l$hour_offset <- as.integer(substr(s, 20L, 22L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}[+-][[:digit:]]{2}$", s)) { # "20200515T082316-07"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$second <- as.integer(substr(s, 14L, 15L))
        l$hour_offset <- as.integer(substr(s, 16L, 18L))
    } else if (grepl("^[[:digit:]]{14}[+-][[:digit:]]{4}$", s)) { # "20200515082316-0700"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
        l$hour_offset <- as.integer(substr(s, 15L, 17L))
        l$minute_offset <- as.integer(substr(s, 18L, 19L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{4}$", s)) { # "2020-05-15T08:23:16-0700"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
        l$hour_offset <- as.integer(substr(s, 20L, 22L))
        l$minute_offset <- as.integer(substr(s, 23L, 24L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16-07:00"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
        l$hour_offset <- as.integer(substr(s, 20L, 22L))
        l$minute_offset <- as.integer(substr(s, 24L, 25L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}[+-][[:digit:]]{4}$", s)) { # "20200515T082316-0700"
        l$year <- as.integer(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
        l$second <- as.integer(substr(s, 14L, 15L))
        l$hour_offset <- as.integer(substr(s, 16L, 18L))
        l$minute_offset <- as.integer(substr(s, 19L, 20L))
    } else {
        stop(paste("Can't parse datetime", x))
    }
    as.data.frame(l)
}
