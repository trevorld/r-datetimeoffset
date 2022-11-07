#' Coerce to "datetimeoffset" objects
#'
#' `as_datetimeoffset()` coerces to [datetimeoffset()] objects.
#'
#' @param x An R object that can reasonably be coerced to a [datetimeoffset()] object
#'          such as a string in pdfmark date or ISO 8601 datetime formats
#'          or something with an [as.POSIXct()] method.
#' @param tz Time zone to use for the conversion.
#'           Ignored by `as_datetimeoffset.Date()`.
#'           Generally need not be a single value.
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
#' as_datetimeoffset("D:20200515082316+03'30'")
#'
#' as_datetimeoffset(Sys.time())
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
#' @export
as_datetimeoffset.default <- function(x, ...) {
    as_datetimeoffset(as.POSIXct(x))
}

#' @rdname as_datetimeoffset
#' @param precision What precision to treat the `POSIXt` datetime as.
#'        Either "second" or "microsecond".
#'        `precision = "microsecond"` requires that the suggested package `nanotime` is installed.
#' @export
as_datetimeoffset.POSIXt <- function(x, ..., precision = ifelse(requireNamespace("nanotime", quietly = TRUE), "microsecond", "second")) {
    precision <- match.arg(precision, c("second", "microsecond"))
    if (precision == "second") {
        as_datetimeoffset(clock::as_zoned_time(x))
    } else {
        assert_suggested("nanotime")
        as_datetimeoffset.nanotime(nanotime::as.nanotime(x), tz = get_tz(x))
    }
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
    assert_suggested("nanotime")
    tz <- clean_tz(tz, na = "GMT")
    n <- max(length(x), length(tz))
    if (length(x) < n)
        x <- rep(x, length.out = n)
    if (length(tz) < n)
        tz <- rep(tz, length.out = n)
    df <- data.frame(dt = x, tz = tz, stringsAsFactors = FALSE)
    purrr::pmap_vec(df, function(dt, tz) {
                       as_datetimeoffset(format(dt, tz = tz, format = "%Y-%m-%dT%H:%M:%E9S%Ez"),
                                         tz = ifelse(is.na(dt), NA_character_, tz))
                    },
                    .ptype = datetimeoffset())
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_month_day <- function(x, ...) {
    as_datetimeoffset(format(x))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_month_weekday <- function(x, ...) {
    as_datetimeoffset(format(as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_iso_year_week_day <- function(x, ...) {
    as_datetimeoffset(format(as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_quarter_day <- function(x, ...) {
    as_datetimeoffset(format(as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_day <- function(x, ...) {
    as_datetimeoffset(format(as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_naive_time <- function(x, ...) {
    as_datetimeoffset(format(x))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_sys_time <- function(x, ...) {
    set_tz(as_datetimeoffset(format(x)),
           ifelse(is.na(x), NA_character_, "GMT"))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_zoned_time <- function(x, ...) {
    as_datetimeoffset(format(x))
}

parse_nanoseconds <- function(x) {
    x <- substr(x, 2L, nchar(x))
    stopifnot(nchar(x) <= 9L)
    n <- nchar(x)
    x <- paste0(x, paste(rep_len("0", 9L - n), collapse = ""))
    dto_as_integer(x)
}

as_dtos_character <- function(x) {
    tryCatch(as_dtos_character_helper(x),
             error = function(e) stop(paste("Can't parse datetime string", sQuote(x)))
             )
}

as_dtos_character_helper <- function(x) {
    if (grepl("^D:[[:digit:]+-\\']{4,}$", x)) { # pdfmark prefix
        s <- gsub("'", "", x)
        s <- substr(s, 3L, nchar(s))
    } else {
        s <- x
    }
    s <- sub("^([[:digit:]X]{4})[-/]([[:digit:]X]{2})", "\\1\\2", s)
    s <- sub("^([[:digit:]X]{6})[-/]([[:digit:]X]{2})", "\\1\\2", s)
    s <- gsub("([[:digit:]X])[Tt ]([[:digit:]+-X])", "\\1\\2", s)
    s <- gsub(":", "", s)
    l <- list(year = NA_integer_, month = NA_integer_, day = NA_integer_,
              hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
              nanosecond = NA_integer_,
              hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_)
    # "2020-05-15T08:23:16-07:00"
    if (is.na(s) || s == "") {
        invisible(NULL)
    } else if (grepl("^.+[Zz]$", s)) { # ends in Z or z means "GMT" time
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 1L))
        l$hour_offset <- 0
        l$minute_offset <- 0
        l$tz <- "GMT"
    } else if (grepl("^.+\\[.+\\]$", s)) { # ends in [America/Los_Angeles] means "America/Los_Angeles" time zone
        l <- as_dtos_character_helper(gsub("^(.*)(\\[.+\\])$", "\\1", s))
        tz <- gsub("^.*\\[(.+)\\]$", "\\1", s)
        l$tz <- ifelse(tz == "X", NA_character_, tz)
    } else if (grepl("^.+[+-][[:digit:]X]{2}$", s)) { # ends in -07 or +07 means hour offset
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 3L))
        l$hour_offset <- dto_as_integer(substr(s, nchar(s) - 2L, nchar(s)))
    } else if (grepl("^.+[+-][[:digit:]X]{4}$", s)) { # ends in -0700 or +0700 means hour/minute offset
        l <- as_dtos_character_helper(substr(s, 1L, nchar(s) - 5L))
        l$hour_offset <- dto_as_integer(substr(s, nchar(s) - 4L, nchar(s) - 2L))
        l$minute_offset <- dto_as_integer(substr(s, nchar(s) - 1L, nchar(s)))
    } else if (grepl("^[[:digit:]X]{4}$", s)) { # "2020"
        l$year <- dto_as_integer(s)
    } else if (grepl("^[[:digit:]X]{6}$", s)) { # "202005"
        l <- as_dtos_character_helper(substr(s, 1L, 4L))
        l$month <- dto_as_integer(substr(s, 5L, 6L))
    } else if (grepl("^[[:digit:]X]{8}$", s)) { # "20200515"
        l <- as_dtos_character_helper(substr(s, 1L, 6L))
        l$day <- dto_as_integer(substr(s, 7L, 8L))
    } else if (grepl("^[[:digit:]X]{10}$", s)) { # "2020051508"
        l <- as_dtos_character_helper(substr(s, 1L, 8L))
        l$hour <- dto_as_integer(substr(s, 9L, 10L))
    } else if (grepl("^[[:digit:]X]{12}$", s)) { # "202005150823"
        l <- as_dtos_character_helper(substr(s, 1L, 10L))
        l$minute <- dto_as_integer(substr(s, 11L, 12L))
    } else if (grepl("^[[:digit:]X]{14}$", s)) { # "20200515082316"
        l <- as_dtos_character_helper(substr(s, 1L, 12L))
        l$second <- dto_as_integer(substr(s, 13L, 14L))
    } else if (grepl("^[[:digit:]X]{14}\\.[[:digit:]X]{1,}$", s)) { # "20200515082316.003"
        l <- as_dtos_character_helper(substr(s, 1L, 14L))
        l$nanosecond <-  parse_nanoseconds(substr(s, 15L, nchar(s)))
    } else {
        stop(paste("Can't parse datetime", x))
    }
    as.data.frame(l)
}

# XX should cast to NA_integer_ without warning
dto_as_integer <- function(s) {
    suppressWarnings(as.integer(s))
}
