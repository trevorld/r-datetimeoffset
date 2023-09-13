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
#' @return A [datetimeoffset()] vector
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
#' @export
as_datetimeoffset.integer <- function(x, ...) {
    datetimeoffset(x)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.numeric <- function(x, ...) {
    datetimeoffset(trunc(x))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.POSIXt <- function(x, ...) {
    purrr::map_vec(x, as_dto_posix, .ptype = datetimeoffset(0))
}

as_dto_posix <- function(x) {
    if (is.na(x)) return(NA_datetimeoffset_)
    lt <- as.POSIXlt(x)
    seconds <- trunc(lt[, "sec"])
    microseconds <- formatC(round(1e6 * lt[, "sec"]),
                            width = 8L, format = "d", flag = "0")
    seconds_ms <- dto_as_integer(substr(microseconds, 1L, 2L))
    microseconds <- ifelse(seconds_ms == seconds,
                           substr(microseconds, 3L, 8L),
                           "999999")
    if (seconds >= 60L) { # {clock} doesn't seem to handle leap seconds
        tz <- clean_tz(get_tz(lt))
        FT <- format(lt, format = "%FT%T", digits = 0L, tz = tz)
        offset <- get_utc_offsets(x)
        s <- paste0(FT, ".", microseconds, offset, "[", tz, "]")
        as_datetimeoffset(s)
    } else {
        ymd <- clock::as_year_month_day(clock::as_sys_time(x))
        ymd <- clock::set_microsecond(ymd, dto_as_integer(microseconds))
        as_datetimeoffset(clock::as_zoned_time(clock::as_sys_time(ymd),
                                               zone = clock::date_time_zone(x)))
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
                    df$hour, df$minute, df$second,
                    df$nanosecond, df$subsecond_digits,
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
as_datetimeoffset.partial_time <- function(x, ...) {
    sec <- trunc(x[, "sec"])
    ns <- trunc(1e9 * (x[, "sec"] - sec))
    ho <- trunc(x[, "tzhour"])
    mo <- as.integer(60 * abs(x[, "tzhour"] - ho))
    datetimeoffset(x[, "year"], x[, "month"], x[, "day"],
                   x[, "hour"], x[, "min"], sec,
                   ns, NA_integer_,
                   ho, mo, NA_character_)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_month_day <- function(x, ...) {
    as_datetimeoffset(plus_format(x))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_month_weekday <- function(x, ...) {
    as_datetimeoffset(plus_format(clock::as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_iso_year_week_day <- function(x, ...) {
    as_datetimeoffset(plus_format(clock::as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_quarter_day <- function(x, ...) {
    as_datetimeoffset(plus_format(clock::as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_year_day <- function(x, ...) {
    as_datetimeoffset(plus_format(clock::as_year_month_day(x)))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_naive_time <- function(x, ...) {
    as_datetimeoffset(plus_format(x))
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_sys_time <- function(x, ...) {
    s <- paste0(plus_format(x), "Z")
    is.na(s) <- is.na(x)
    as_datetimeoffset(s)
}

#' @rdname as_datetimeoffset
#' @export
as_datetimeoffset.clock_zoned_time <- function(x, ...) {
    as_datetimeoffset(plus_format(x))
}

# 12345-10-10 to +12345-10-10
plus_format <- function(x) {
    s <- format(x)
    idx <- which(substr(x, 1L, 1L) != "-")
    if (length(idx) > 0)
        s[idx] <- paste0("+", s[idx])
    s
}

parse_nanoseconds <- function(x) {
    n <- nchar(x)
    if (n > 9L) {
        # warn/message?
        x <- substr(x, 1L, 9L)
    }
    if (n < 9L) {
        x <- paste0(x, paste(rep_len("0", 9L - n), collapse = ""))
    }
    dto_as_integer(x)
}

as_dtos_character <- function(x) {
    tryCatch(as_dtos_character_helper(x),
             error = function(e) stop(paste("Can't parse datetime string", sQuote(x)))
             )
}

as_ymd_weekdate <- function(x, l) {
    x <- gsub("[W-]", "", x)
    year <- as.integer(substr(x, 1, 4))
    week <- as.integer(substr(x, 5, 6))
    if (nchar(x) > 6) {
        day <- as.integer(substr(x, 7, 7))
        ywd <- clock::as_year_month_day(clock::iso_year_week_day(year, week, day))
        l$year <- clock::get_year(ywd)
        l$month <- clock::get_month(ywd)
        l$day <- clock::get_day(ywd)
    } else {
        first <- clock::as_year_month_day(clock::iso_year_week_day(year, week, 1L))
        last <- clock::as_year_month_day(clock::iso_year_week_day(year, week, 7L))
        l$year <- ifelse(clock::get_year(first) == clock::get_year(last),
                         get_year(first), NA_integer_)
        l$month <- ifelse(clock::get_month(first) == clock::get_month(last),
                          get_month(first), NA_integer_)
    }
    l
}

as_ymd_ordinal <- function(x, l) {
    year <- substr(x, 1, 4)
    day <- substr(x, nchar(x) - 2L, nchar(x))
    yd <- clock::year_day(dto_as_integer(year), dto_as_integer(day))
    ymd <- clock::as_year_month_day(yd)
    l$year <- clock::get_year(ymd)
    l$month <- clock::get_month(ymd)
    l$day <- clock::get_day(ymd)
    l
}

as_dtos_character_helper <- function(x) {
    l <- list(year = NA_integer_, month = NA_integer_, day = NA_integer_,
              hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
              nanosecond = NA_integer_, subsecond_digits = NA_integer_,
              hour_offset = NA_integer_, minute_offset = NA_integer_, tz = NA_character_)
    if (grepl("^D:[[:digit:]+-\\']{4,}$", x)) { # pdfmark prefix
        s <- substr(x, 3L, nchar(x))
    } else if (grepl("^[+-][[:digit:]]{4,}$", x)) {
        l$year <- dto_as_integer(x)
        return(as.data.frame(l))
    } else if (grepl("^[+-][[:digit:]]{4,}", x)) {
        year <- sub("^([+-][[:digit:]]+)(.*)", "\\1", x)
        s <- sub("^([+-][[:digit:]]+)(.*)", "0000\\2", x)
        l <- as_dtos_character_helper(s)
        l$year <- dto_as_integer(year)
        return(as.data.frame(l))
    } else if (grepl("^[[:digit:]]{4}[-]*[[:digit:]]{3}$", x)) {
        l <- as_ymd_ordinal(x, l)
        return(as.data.frame(l))
    } else if (grepl("^[[:digit:]]{4}[-]*[[:digit:]]{3}[Tt ]", x)) {
        s <- sub("^([[:digit:]]{4}[-]*[[:digit:]]{3})[Tt ](.*)", "0000-01-01T\\2", x)
        x <- sub("^([[:digit:]]{4}[-]*[[:digit:]]{3})[Tt ](.*)", "\\1", x)
        l <- as_dtos_character_helper(s)
        l <- as_ymd_ordinal(x, l)
        return(as.data.frame(l))
    } else if (grepl("^[[:digit:]]{4}[-]*W[[:digit:]]{2}[-]*[[:digit:]]", x)) {
        s <- sub("^([[:digit:]]{4}[-]*W[[:digit:]]{2}[-]*[[:digit:]])(.*)", "0000-01-01\\2", x)
        x <- sub("^([[:digit:]]{4}[-]*W[[:digit:]]{2}[-]*[[:digit:]])(.*)", "\\1", x)
        l <- as_dtos_character_helper(s)
        l <- as_ymd_weekdate(x, l)
        return(as.data.frame(l))
    } else if (grepl("^[[:digit:]]{4}[-]*W[[:digit:]]{2}", x)) {
        l <- as_ymd_weekdate(x, l)
        return(as.data.frame(l))
    } else {
        s <- x
    }
    s <- gsub("'", "", s)
    s <- sub("^--", "XXXX", s)
    s <- sub("^([[:digit:]X]{4})[-/]([[:digit:]X]{2})", "\\1\\2", s)
    s <- sub("^([[:digit:]X]{6})[-/]([[:digit:]X]{2})", "\\1\\2", s)
    s <- sub("^([[:digit:]]{2}:)", "XXXXXXXX\\1", s)
    s <- sub("^([Tt])", "XXXXXXXX\\1", s)
    s <- gsub("([[:digit:]X])[Tt ]([[:digit:]+-X])", "\\1\\2", s)
    s <- gsub(":", "", s)
    # "2020-05-15T08:23:16-07:00"
    if (is.na(s) || s == "") {
        invisible(NULL)
    } else if (grepl("^.+[Zz](00){0,2}$", s)) { # ends in Z or z means "GMT" time
        if (substr(s, nchar(s) - 1L, nchar(s)) == "00") # observed pdfmark ending in Z00'00'
            l <- as_dtos_character_helper(substr(s, 1, nchar(s) - 2L))
        else
            l <- as_dtos_character_helper(substr(s, 1, nchar(s) - 1L))
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
        subseconds <- substr(s, 16L, nchar(s))
        l$nanosecond <-  parse_nanoseconds(subseconds)
        l$subsecond_digits <- min(nchar(subseconds), 9L)
    } else {
        stop(paste("Can't parse datetime", x))
    }
    as.data.frame(l)
}

# XX should cast to NA_integer_ without warning
dto_as_integer <- function(s) {
    suppressWarnings(as.integer(s))
}
