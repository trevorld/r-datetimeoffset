#' Coerce to "datetime_offset" objects
#'
#' `as_datetime_offset()` coerces to [datetime_offset()] objects.
#'
#' @param x An R object that can reasonably be coerced to a [datetime_offset()] object
#'          such as a string in pdfmark date or ISO 8601 datetime formats
#'          or something with an [as.nanotime()] or [as.POSIXct()] method.
#' @param tz Time zone to use for the conversion.
#'           Ignored by `as_datetime_offset.Date()`.
#'           Need not be a single value for `as_datetime_offset.nanotime()`.
#' @param ... Further arguments to certain methods.
#' @examples
#' # ISO 8601 examples
#' as_datetime_offset("2020-05-15")
#' as_datetime_offset("20200515")
#' as_datetime_offset("2020-05-15T08:23:16")
#' as_datetime_offset("20200515T082316")
#' as_datetime_offset("2020-05-15T08:23:16.003Z")
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
as_datetime_offset.Date <- function(x, tz = NA_character_, ...) {
    as_datetime_offset(format(x), tz = tz)
}

is_datetime_offset <- function(x) inherits(x, "datetime_offset")

#' @rdname as_datetime_offset
#' @importFrom nanotime as.nanotime
#' @export
as_datetime_offset.default <- function(x, tz = lubridate::tz(as.POSIXct(x)), ...) {
    dt <- as.POSIXct(x)
    as_datetime_offset(as.nanotime(x), tz = tz)
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.POSIXct <- function(x, tz = lubridate::tz(x), ...) {
    as_datetime_offset(as.nanotime(x), tz = tz)
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.POSIXlt <- function(x, tz = lubridate::tz(x), ...) {
    as_datetime_offset(as.nanotime(x), tz = tz)
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.character <- function(x, tz = NA_character_, ...) {
    l <- lapply(x, as_dtos_character_helper)
    df <- do.call(rbind, l)

    tz_df <- df$tz
    n <- max(length(tz_df), length(tz))
    tz_df <- rep_len(tz_df, n)
    tz <- rep_len(tz, n)
    tz <- ifelse(is.na(tz_df), tz, tz_df)

    datetime_offset(df$year, df$month, df$day,
                    df$hour, df$minute, df$second, df$nanosecond,
                    df$hour_offset, df$minute_offset, tz)
}

#' @rdname as_datetime_offset
#' @export
as_datetime_offset.nanotime <- function(x, tz = "GMT", ...) {
    tz <- clean_tz(tz)
    df <- data.frame(dt = x, tz = tz)
    l <- purrr::pmap(df, function(dt, tz) {
                       as_datetime_offset(format(dt, tz = tz), tz = tz)
                     })
    do.call(c, l)
}

get_ns <- function(x) {
    v <- rep_len(NA_integer_, length(x))
    ns <- gsub("([^.]*)(\\.[[:digit:]]{1,}){0,1}(.*)", "\\2", x)
    idx <- which(ns != "")
    if (length(idx) > 0L) {
        ns <- substr(ns, 2L, nchar(ns))
        stopifnot(any(nchar(ns) <= 9L))
        ns <- vapply(ns, function(x) {
                           n <- nchar(x)
                           x <- paste0(x, paste(rep_len("0", 9L - n), collapse = ""))
                           as.integer(x)
                     },
                     integer(1L), USE.NAMES = FALSE)
        v[idx] <- ns
    }
    v
}

strip_ns <- function(x) {
    ns <- gsub("([^.]*)(\\.[[:digit:]]{1,}){0,1}(.*)", "\\1\\3", x)
    ns
}

as_dtos_character_helper <- function(x) {
    if (grepl("^D:[[:digit:]+-]{4,}$", x)) # pdfmark prefix
        s <- substr(x, 3L, nchar(x))
    else
        s <- x
    l <- list(year = NA_integer_, month = NA_integer_, day = NA_integer_,
              hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
              nanosecond = NA_integer_,
              hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_)
    # "2020-05-15T08:23:16-07:00"
    if (grepl("^[[:digit:]]{4}$", s)) { # "2020"
        l$year <- as.integer(s)
    } else if (grepl("^[[:digit:]]{6}$", s)) { # "202005"
        l <- as_dtos_character_helper(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}$", s)) { # "2020-05"
        l <- as_dtos_character_helper(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 6L, 7L))
    } else if (grepl("^[[:digit:]]{8}$", s)) { # "20200515"
        l <- as_dtos_character_helper(substr(s, 1L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}$", s)) { # "2020-05-15"
        l <- as_dtos_character_helper(substr(s, 1L, 7L))
        l$day <- as.integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]]{10}$", s)) { # "2020051508"
        l <- as_dtos_character_helper(substr(s, 1L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}$", s)) { # "2020-05-15T08"
        l <- as_dtos_character_helper(substr(s, 1L, 10L))
        l$hour <- as.integer(substr(s, 12L, 13L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}Z$", s)) { # "2020-05-15T08Z"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08-07"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$hour_offset <- as.integer(substr(s, 14L, 16L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}[+-][[:digit:]]{4}$", s)) { # "2020-05-15T08-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$minute_offset <- as.integer(substr(s, 17L, 18L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08-07:00"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$minute_offset <- as.integer(substr(s, 18L, 19L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{2}$", s)) { # "20200515T08"
        l <- as_dtos_character_helper(substr(s, 1L, 8L))
        l$hour <- as.integer(substr(s, 10L, 11L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{2}Z$", s)) { # "20200515T08Z"
        l <- as_dtos_character_helper(substr(s, 1L, 11L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{2}[+-][[:digit:]]{2}$", s)) { # "20200515T08-07"
        l <- as_dtos_character_helper(substr(s, 1L, 11L))
        l$hour_offset <- as.integer(substr(s, 12L, 14L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{2}[+-][[:digit:]]{4}$", s)) { # "20200515T08-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 14L))
        l$minute_offset <- as.integer(substr(s, 15L, 16L))
    } else if (grepl("^[[:digit:]]{12}$", s)) { # "202005150823"
        l <- as_dtos_character_helper(substr(s, 1L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$minute <- as.integer(substr(s, 15L, 16L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}$", s)) { # "20200515T0823"
        l <- as_dtos_character_helper(substr(s, 1L, 11L))
        l$minute <- as.integer(substr(s, 12L, 13L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}Z$", s)) { # "2020-05-15T08:23Z"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}Z$", s)) { # "20200515T0823Z"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08:23-07"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$hour_offset <- as.integer(substr(s, 17L, 19L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}[+-][[:digit:]]{2}$", s)) { # "20200515T0823-07"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$hour_offset <- as.integer(substr(s, 14L, 16L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{4}[+-][[:digit:]]{4}$", s)) { # "20200515T0823-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$minute_offset <- as.integer(substr(s, 17L, 18L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}[[:digit:]]{2}$", s)) { # "2020-05-15T08:23-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 19L))
        l$minute_offset <- as.integer(substr(s, 20L, 21L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}:[[:digit:]]{2}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23-07:00"
        l <- as_dtos_character_helper(substr(s, 1L, 19L))
        l$minute_offset <- as.integer(substr(s, 21L, 22L))
    } else if (grepl("^[[:digit:]]{14}$", s)) { # "20200515082316"
        l <- as_dtos_character_helper(substr(s, 1L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}$", s)) { # "2020-05-15T08:23:16"
        l <- as_dtos_character_helper(substr(s, 1L, 16L))
        l$second <- as.integer(substr(s, 18L, 19L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}\\.[[:digit:]]{1,}$", s)) { # "2020-05-15T08:23:16.003"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}$", s)) { # "20200515T082316"
        l <- as_dtos_character_helper(substr(s, 1L, 13L))
        l$second <- as.integer(substr(s, 14L, 15L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}\\.[[:digit:]]{1,}$", s)) { # "20200515T082316.003"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{14}Z$", s)) { # "20200515082316Z"
        l <- as_dtos_character_helper(substr(s, 1L, 14L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}Z$", s)) { # "2020-05-15T08:23:16Z"
        l <- as_dtos_character_helper(substr(s, 1L, 19L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}\\.[[:digit:]]{1,}Z$", s)) { # "2020-05-15T08:23:16.003Z"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}Z$", s)) { # "20200515T082316Z"
        l <- as_dtos_character_helper(substr(s, 1L, 15L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}\\.[[:digit:]]{1,}Z$", s)) { # "20200515T082316.003Z"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{14}[+-][[:digit:]]{2}$", s)) { # "20200515082316-07"
        l <- as_dtos_character_helper(substr(s, 1L, 14L))
        l$hour_offset <- as.integer(substr(s, 15L, 17L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16-07"
        l <- as_dtos_character_helper(substr(s, 1L, 19L))
        l$hour_offset <- as.integer(substr(s, 20L, 22L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}\\.[[:digit:]]{1,}[+-][[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16.003-07"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}[+-][[:digit:]]{2}$", s)) { # "20200515T082316-07"
        l <- as_dtos_character_helper(substr(s, 1L, 15L))
        l$hour_offset <- as.integer(substr(s, 16L, 18L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}\\.[[:digit:]]{1,}[+-][[:digit:]]{2}$", s)) { # "20200515T082316.003-07"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{14}[+-][[:digit:]]{4}$", s)) { # "20200515082316-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 17L))
        l$minute_offset <- as.integer(substr(s, 18L, 19L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{4}$", s)) { # "2020-05-15T08:23:16-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 22L))
        l$minute_offset <- as.integer(substr(s, 23L, 24L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}\\.[[:digit:]]{1,}[+-][[:digit:]]{4}$", s)) { # "2020-05-15T08:23:16.003-0700"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16-07:00"
        l <- as_dtos_character_helper(substr(s, 1L, 22L))
        l$minute_offset <- as.integer(substr(s, 24L, 25L))
    } else if (grepl("^[[:digit:]]{4}[-/][[:digit:]]{2}[-/][[:digit:]]{2}[T ][[:digit:]]{2}(:[[:digit:]]{2}){2}\\.[[:digit:]]{1,}[+-][[:digit:]]{2}:[[:digit:]]{2}$", s)) { # "2020-05-15T08:23:16.003-07:00"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}[+-][[:digit:]]{4}$", s)) { # "20200515T082316-0700"
        l <- as_dtos_character_helper(substr(s, 1L, 18L))
        l$minute_offset <- as.integer(substr(s, 19L, 20L))
    } else if (grepl("^[[:digit:]]{8}[T ][[:digit:]]{6}\\.[[:digit:]]{1,}[+-][[:digit:]]{4}$", s)) { # "20200515T082316.003-0700"
        l <- as_dtos_character_helper(strip_ns(s))
        l$nanosecond <-  get_ns(s)
    } else {
        stop(paste("Can't parse datetime", x))
    }
    as.data.frame(l)
}
