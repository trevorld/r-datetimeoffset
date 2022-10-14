#' Coerce to "datetimeoffset" objects
#'
#' `as_datetimeoffset()` coerces to [datetimeoffset()] objects.
#'
#' @param x An R object that can reasonably be coerced to a [datetimeoffset()] object
#'          such as a string in pdfmark date or ISO 8601 datetime formats
#'          or something with an [as.nanotime()] or [as.POSIXct()] method.
#' @param tz Time zone to use for the conversion.
#'           Ignored by `as_datetimeoffset.Date()`.
#'           Need not be a single value for `as_datetimeoffset.nanotime()`.
#' @param ... Further arguments to certain methods.
#' @examples
#' # ISO 8601 examples
#' as_datetimeoffset("2020-05-15")
#' as_datetimeoffset("20200515")
#' as_datetimeoffset("2020-05-15T08:23:16")
#' as_datetimeoffset("20200515T082316")
#' as_datetimeoffset("2020-05-15T08:23:16.003Z")
#' as_datetimeoffset("20200515T082316Z")
#' as_datetimeoffset("2020-05-15T08:23:16+03:30")
#' as_datetimeoffset("20200515T082316+0330")
#'
#' # Misc supported `as.POSIXlt()` `tryFormats` examples
#' as_datetimeoffset("2020/05/15 08:23:16")
#'
#' # pdfmark datetime examples
#' as_datetimeoffset("D:20200515")
#' as_datetimeoffset("D:20200515082316")
#' as_datetimeoffset("D:20200515082316+0330")
#' @export
as_datetimeoffset <- function(x, ...) {
    UseMethod("as_datetimeoffset")
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.datetimeoffset <- function(x, ...) {
    x
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.Date <- function(x, tz = NA_character_, ...) {
    as_datetimeoffset(format(x), tz = tz)
}

#' @rdname as_datetimeoffset
#' @importFrom nanotime as.nanotime
#' @export
as_datetimeoffset.default <- function(x, tz = lubridate::tz(as.POSIXct(x)), ...) {
    as_datetimeoffset(as.nanotime(as.POSIXct(x)), tz = tz)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.POSIXct <- function(x, tz = lubridate::tz(x), ...) {
    as_datetimeoffset(as.nanotime(x), tz = tz)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.POSIXlt <- function(x, tz = lubridate::tz(x), ...) {
    as_datetimeoffset(as.nanotime(x), tz = tz)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.character <- function(x, tz = NA_character_, ...) {
    l <- lapply(x, as_dtos_character)
    df <- do.call(rbind, l)

    tz_df <- df$tz
    n <- max(length(tz_df), length(tz))
    tz_df <- rep_len(tz_df, n)
    tz <- rep_len(tz, n)
    tz <- ifelse(is.na(tz_df), tz, tz_df)

    datetimeoffset(df$year, df$month, df$day,
                    df$hour, df$minute, df$second, df$nanosecond,
                    df$hour_offset, df$minute_offset, tz)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.nanotime <- function(x, tz = "GMT", ...) {
    tz <- clean_tz(tz, na = "GMT")
    n <- length(tz)
    if (length(x) < n)
        x <- rep(x, length.out = n)
    df <- data.frame(dt = x, tz = tz, stringsAsFactors = FALSE)
    purrr::pmap_vec(df, function(dt, tz) {
                       as_datetimeoffset(format(dt, tz = tz, format = "%Y-%m-%dT%H:%M:%E9S%Ez"), tz = tz)
                    },
                    .ptype = datetimeoffset())
}

parse_nanoseconds <- function(x) {
    x <- substr(x, 2L, nchar(x))
    stopifnot(nchar(x) <= 9L)
    n <- nchar(x)
    x <- paste0(x, paste(rep_len("0", 9L - n), collapse = ""))
    as.integer(x)
}

as_dtos_character <- function(x) {
    tryCatch(as_dtos_character_helper(x),
             error = function(e) stop(paste("Can't parse datetime string", sQuote(x)))
             )
}

as_dtos_character_helper <- function(x) {
    if (grepl("^D:[[:digit:]+-]{4,}$", x)) # pdfmark prefix
        s <- substr(x, 3L, nchar(x))
    else
        s <- x
    s <- sub("^([[:digit:]]{4})[-/]([[:digit:]]{2})", "\\1\\2", s)
    s <- sub("^([[:digit:]]{6})[-/]([[:digit:]]{2})", "\\1\\2", s)
    s <- gsub("([[:digit:]])[Tt ]([[:digit:]+-])", "\\1\\2", s)
    s <- gsub(":", "", s)
    l <- list(year = NA_integer_, month = NA_integer_, day = NA_integer_,
              hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
              nanosecond = NA_integer_,
              hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_)
    # "2020-05-15T08:23:16-07:00"
    if (s == "") {
        invisible(NULL)
    } else if (grepl("^.+[Zz]$", s)) { # ends in Z or z means "GMT" time
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 1L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^.+\\[.+\\]$", s)) { # ends in [US/Pacific] means "US/Pacific" time zone
        l <- as_dtos_character_helper(gsub("^(.*)(\\[.+\\])$", "\\1", s))
        l$tz <- gsub("^.*\\[(.+)\\]$", "\\1", s)
    } else if (grepl("^.+[+-][[:digit:]]{2}$", s)) { # ends in -07 or +07 means hour offset
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 3L))
        l$hour_offset <- as.integer(substr(s, nchar(s) - 2L, nchar(s)))
    } else if (grepl("^.+[+-][[:digit:]]{4}$", s)) { # ends in -0700 or +0700 means hour/minute offset
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 5L))
        l$hour_offset <- as.integer(substr(s, nchar(s) - 4L, nchar(s) - 2L))
        l$minute_offset <- as.integer(substr(s, nchar(s) - 1L, nchar(s)))
    } else if (grepl("^[[:digit:]]{4}$", s)) { # "2020"
        l$year <- as.integer(s)
    } else if (grepl("^[[:digit:]]{6}$", s)) { # "202005"
        l <- as_dtos_character_helper(substr(s, 1L, 4L))
        l$month <- as.integer(substr(s, 5L, 6L))
    } else if (grepl("^[[:digit:]]{8}$", s)) { # "20200515"
        l <- as_dtos_character_helper(substr(s, 1L, 6L))
        l$day <- as.integer(substr(s, 7L, 8L))
    } else if (grepl("^[[:digit:]]{10}$", s)) { # "2020051508"
        l <- as_dtos_character_helper(substr(s, 1L, 8L))
        l$hour <- as.integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]]{12}$", s)) { # "202005150823"
        l <- as_dtos_character_helper(substr(s, 1L, 10L))
        l$minute <- as.integer(substr(s, 11L, 12L))
    } else if (grepl("^[[:digit:]]{14}$", s)) { # "20200515082316"
        l <- as_dtos_character_helper(substr(s, 1L, 12L))
        l$second <- as.integer(substr(s, 13L, 14L))
    } else if (grepl("^[[:digit:]]{14}\\.[[:digit:]]{1,}$", s)) { # "20200515082316.003"
        l <- as_dtos_character_helper(substr(s, 1L, 14L))
        l$nanosecond <-  parse_nanoseconds(substr(s, 15L, nchar(s)))
    } else {
        stop(paste("Can't parse datetime", x))
    }
    as.data.frame(l)
}
