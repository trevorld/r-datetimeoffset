#' Convert datetime objects to character
#'
#' `format()` returns a datetime string
#'  with as much **known** information possible (RFC 3339 with de facto standard time zone extension).
#' `format_iso8601()` returns an ISO 8601 datetime string.
#' `format_pdfmark()` returns a pdfmark datetime string with as much **known** information possible.
#' `format_strftime()` allows [base::strftime()] style formatting.
#' `format_nanotime()` allows CCTZ style formatting.
#' `format_edtf()` returns an Extended Date Time Format (EDTF) string.
#' `format_exiftool()` returns the date/time string expected by `exiftool`.
#' @name format
#' @param x A [datetimeoffset()] object.
#' @param format For `format_strftime()` see [base::strftime()].
#'               For `format_nanotime()` see <https://github.com/google/cctz/blob/6e09ceb/include/time_zone.h#L197>.
#' @param tz A character string specifying the time zone to be used for the conversion.
#'           Can be a length greater than one.
#' @param offsets Include the UTC offsets in the formatting
#' @param usetz Include the time zone in the formatting
#' @param precision The amount of precision: either "year", "month", "day", "hour", "minute", "second",
#'                  "decisecond", "centisecond", "millisecond",
#'                  "hundred microseconds", "ten microseconds", "microsecond",
#'                  "hundred nanoseconds", "ten nanoseconds", or "nanosecond".
#'        If `NULL` then full precision for the object is shown.
#' @param prefix Prefix to use.  Either `"D:"` (default) or `""`.
#' @param ... Ignored
#' @return A character vector
#' @examples
#'   # ISO 8601 datetimes
#'   format_iso8601(as_datetimeoffset("2020-05"))
#'   format_iso8601(as_datetimeoffset("2020-05-10 20:15"))
#'   format_iso8601(as_datetimeoffset("2020-05-10 20:15:05-07"))
#'   if (requireNamespace("lubridate"))
#'     lubridate::format_ISO8601(as_datetimeoffset("2020-05-10 20:15:05-07"))
#'
#'   # pdfmark datetimes
#'   format_pdfmark(as_datetimeoffset("2020-05"))
#'   format_pdfmark(as_datetimeoffset("2020-05-10 20:15"))
#'   format_pdfmark(as_datetimeoffset("2020-05-10 20:15:05-07"))
#'
#'   # strftime style formatting
#'   dt <- as_datetimeoffset("2020-05-10 20:15")
#'   format_strftime(dt)
#'   format_strftime(dt, format = "%c")
#'
#'   # CCTZ style formatting
#'   if (requireNamespace("nanotime")) {
#'     dt <- as_datetimeoffset(Sys.time())
#'     format_nanotime(dt, format = "%F %H:%M:%E7S %Ez") # SQL Server datetimeoffset
#'   }
#'
#'   # EDTF style formatting
#'   format_edtf(as_datetimeoffset("2020-05"))
#'   format_edtf(as_datetimeoffset("2020-05-10T20:15:05-07"))
#'   dt <- datetimeoffset(2020, NA_integer_, 10)
#'   format_edtf(dt)
#'
#'   # `exiftool` formatting
#'   format_exiftool(as_datetimeoffset("2020:05:10"))
#'   format_exiftool(as_datetimeoffset("2020:05:10 20:15"))
#'   format_exiftool(as_datetimeoffset("2020:05:10 20:15:05-07:00"))
NULL

#' @rdname format
#' @export
format.datetimeoffset <- function(x, ...) {
    x <- update_nas(x, allow_times = TRUE)
    s <- purrr::map_chr(x, format_helper)
    is.na(s) <- is.na(field(x, "year")) & !is_time(x)
    s
}

#' @rdname format
#' @param sep UTC offset separator.  Either ":" or "".
#' @param mode  If `mode = "pdf"` only output supported PDF docinfo datetime values.
#'              If `mode = "toml"` only output supported TOML datetime values.
#'              If `mode = "xmp"` only output valid XMP metadata datetime values.
#' @export
format_iso8601 <- function(x, offsets = TRUE, precision = NULL, sep = ":",
                           mode = c("normal", "toml", "xmp"), ...) {
    mode <- match.arg(mode)
    purrr::map_chr(x, format_iso8601_helper, offsets = offsets, precision = precision, sep = sep, mode = mode)
}

format_ISO8601.datetimeoffset <- function(x, usetz = FALSE, precision = NULL, ...) {
   if (!is.null(precision))
       precision <- switch(precision,
                           y = "year", ym = "month", ymd = "day",
                           ymdh = "hour", ymdhm = "minute", ymdhms = "second",
                           stop(paste("Don't recognize precision", sQuote(precision))))
   format_iso8601(x, offsets = usetz, precision = precision, sep = "")
}


#' @rdname format
#' @export
format_pdfmark <- function(x, prefix = "D:") {
    x <- as_datetimeoffset(x)
    x <- update_nas(x, pdfmark = TRUE)
    s <- purrr::map_chr(x, format_pdfmark_helper, prefix = prefix)
    is.na(s) <- is.na(field(x, "year"))
    s
}

#' @rdname format
#' @export
format_edtf <- function(x, offsets = TRUE, precision = NULL, usetz = FALSE, ...) {
    purrr::map_chr(x, format_edtf_helper, offsets = offsets, precision = precision, usetz = usetz)
}

#' @rdname format
#' @export
format_exiftool <- function(x, mode = c("normal", "xmp", "pdf"), ...) {
    mode <- match.arg(mode)
    purrr::map_chr(x, format_exiftool_helper, mode = mode)
}

#' @rdname format
#' @inheritParams as_zoned_time.datetimeoffset
#' @export
format_strftime <- function(x, format = "%Y-%m-%d %H:%M:%S", tz = get_tz(x),
                            usetz = FALSE, fill = mode_tz(x)) {
    tz <- clean_tz(tz, na = Sys.timezone())
    fill <- clean_tz(fill, na = NA_character_)
    x <- fill_tz(x, fill)
    x <- as.POSIXct(x)
    df <- data.frame(x = x, format = format, tz = tz, usetz = usetz, stringsAsFactors = FALSE)
    purrr::pmap_chr(df, strftime)
}

#' @rdname format
#' @export
format_nanotime <- function(x, format = "%Y-%m-%dT%H:%M:%E9S%Ez", tz = get_tz(x), fill = "") {
    assert_suggested("nanotime")
    tz <- clean_tz(tz, na = Sys.timezone())
    x <- as.nanotime.datetimeoffset(x, fill = fill)
    df <- data.frame(x = x, format = format, tz = tz, stringsAsFactors = FALSE)
    purrr::pmap_chr(df, base::format)
}

format_helper <- function(x) {
    year_str <- my_format_year(field(x, "year"))
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"), field(x, "subsecond_digits"))
    offset_str <- my_format_tz(x, add_tz = TRUE)
    paste0(year_str, month_str, day_str,
           hour_str, minute_str, second_str, nanosecond_str,
           offset_str)
}

my_format_year <- function(year, ...) {
    if (isTRUE(year < 0L)) {
        width <- 5L
    } else {
        width <- 4L
    }
    if (isTRUE(year > 9999L)) {
        flag <- "0+"
    } else {
        flag <- "0"
    }
    my_format(year, width = width, flag = flag, ...)
}

my_format_nanosecond <- function(ns, sd, edtf = FALSE, blank = FALSE) {
    s <- character(length(ns))
    if (blank) return(s)
    idx <- which(!is.na(ns))
    if (length(idx > 0L)) {
        df <- data.frame(ns = ns[idx], sd = sd[idx], stringsAsFactors = FALSE)
        s[idx] <- purrr::pmap_chr(df, my_format_ns_helper)
    }
    idx <- which(is.na(ns))
    if (edtf && length(idx))
        s[idx] <- rep_len(".XXXXXXXXX", length(idx))
    s
}

my_format_ns_helper <- function(ns, sd) {
    s_ns <- formatC(ns, format = "d", flag = "0", width = 9L)
    if (is.na(sd)) {
        s_ns <- gsub("0{1,}$", "", s_ns)
        s_ns <- ifelse(s_ns == "", "0", s_ns)
    } else {
        s_ns <- substr(s_ns, 1L, sd)
    }
    paste0(".", s_ns)
}


format_iso8601_helper <- function(x, offsets = TRUE, precision = NULL, sep = ":",
                                  mode = "normal", ...) {
    if (is.na(x)) return(NA_character_)

    precision <- precision %||%
        ifelse(is_time(x), datetime_precision(x, unspecified = TRUE), datetime_precision(x))
    stopifnot(precision %in% c("year", "month", "day", "hour", "minute", "second",
                               "decisecond", "centisecond", "millisecond",
                               "hundred microseconds", "ten microseconds", "microsecond",
                               "hundred nanoseconds", "ten nanoseconds", "nanosecond"))
    stopifnot(sep %in% c(":", ""))
    x <- datetime_narrow(x, precision)
    if (isFALSE(offsets) || (is_time(x) && mode == "toml")) {
        x <- set_hour_offset(x, NA_integer_)
        x <- set_minute_offset(x, NA_integer_)
        x <- set_tz(x, NA_character_)
    }
    x <- update_nas(x, allow_times = TRUE, toml = (mode == "toml"))
    if (mode %in% c("toml", "xmp")) {
        if (is.na(field(x, "minute")) && !is.na(field(x, "hour")))
            field(x, "minute") <- 0L
        if (is.na(field(x, "minute_offset")) && !is.na(field(x, "hour_offset")))
            field(x, "minute_offset") <- 0L
    }
    year_str <- my_format_year(field(x, "year"))
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    if (is_time(x) && mode == "toml")
        hour_str <- my_format(field(x, "hour"), prefix = "")
    else
        hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"), field(x, "subsecond_digits"))
    offset_str <- my_format_tz(x, sep = sep)
    s <- paste0(year_str, month_str, day_str,
                hour_str, minute_str, second_str, nanosecond_str,
                offset_str)
    if (s == "")
        s <- NA_character_
    s
}

format_pdfmark_helper <- function(x, prefix = "D:") {
    stopifnot(prefix %in% c("D:", ""))
    year_str <- my_format_year(field(x, "year"))
    month_str <- my_format(field(x, "month"), prefix = "")
    day_str <- my_format(field(x, "day"), prefix = "")
    hour_str <- my_format(field(x, "hour"), prefix = "")
    minute_str <- my_format(field(x, "minute"), prefix = "")
    second_str <- my_format(field(x, "second"), prefix = "")
    offset_str <- my_format_tz(x, sep = "'", no_zulu = TRUE)
    offset_str <- ifelse(offset_str == "", "", paste0(offset_str, "'"))
    paste0(prefix, year_str, month_str, day_str, hour_str, minute_str, second_str, offset_str)
}

format_exiftool_helper <- function(x, mode = "normal", ...) {
    if (is.na(x)) return(NA_character_)

    if (mode == "pdf")
        field(x, "nanosecond") <- NA_integer_
    if (mode != "xmp")
        x <- datetime_widen(x, "second")
    x <- update_nas(x)
    if (is.na(field(x, "minute")) && !is.na(field(x, "hour")))
        field(x, "minute") <- 0L
    if (is.na(field(x, "minute_offset")) && !is.na(field(x, "hour_offset")))
        field(x, "minute_offset") <- 0L
    year_str <- my_format_year(field(x, "year"))
    month_str <- my_format(field(x, "month"), prefix = ":")
    day_str <- my_format(field(x, "day"), prefix = ":")
    hour_str <- my_format(field(x, "hour"), prefix = " ")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"), field(x, "subsecond_digits"))
    offset_str <- my_format_tz(x, sep = ":")
    s <- paste0(year_str, month_str, day_str,
                hour_str, minute_str, second_str, nanosecond_str,
                offset_str)
    s
}


format_edtf_helper <- function(x, offsets, precision, usetz) {
    precision <- precision %||% datetime_precision(x, unspecified = TRUE)
    stopifnot(precision %in% c("missing", "year", "month", "day", "hour", "minute", "second",
                               "decisecond", "centisecond", "millisecond",
                               "hundred microseconds", "ten microseconds", "microsecond",
                               "hundred nanoseconds", "ten nanoseconds", "nanosecond"))
    x <- datetime_narrow(x, precision)
    if (isFALSE(offsets)) {
        x <- set_hour_offset(x, NA_integer_)
        x <- set_minute_offset(x, NA_integer_)
        x <- set_tz(x, NA_character_)
    }
    precision <- precision_to_int(precision)
    year_str <- my_format_year(field(x, "year"), edtf = TRUE, blank = precision < PRECISION_YEAR)
    month_str <- my_format(field(x, "month"), prefix = "-", edtf = TRUE, blank = precision < PRECISION_MONTH)
    day_str <- my_format(field(x, "day"), prefix = "-", edtf = TRUE, blank = precision < PRECISION_DAY)
    hour_str <- my_format(field(x, "hour"), prefix = "T", edtf = TRUE, blank = precision < PRECISION_HOUR)
    minute_str <- my_format(field(x, "minute"), prefix = ":", edtf = TRUE, blank = precision < PRECISION_MINUTE)
    second_str <- my_format(field(x, "second"), prefix = ":", edtf = TRUE, blank = precision < PRECISION_SECOND)
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"), field(x, "subsecond_digits"),
                                           edtf = TRUE, blank = precision <= PRECISION_SECOND)
    offset_str <- my_format_tz(x, edtf = offsets, add_tz = usetz)
    paste0(year_str, month_str, day_str,
           hour_str, minute_str, second_str, nanosecond_str,
           offset_str)
}

is_time <- function(x) {
    is.na(get_year(x)) & is.na(get_month(x)) & is.na(get_day(x)) & !is.na(get_hour(x))
}

update_nas <- function(x, allow_times = FALSE, pdfmark = FALSE, toml = FALSE) {
    if (pdfmark || toml) { # pdfmark years in between 0 and 9999
        year <- get_year(x)
        is.na(x) <- is.na(x) | (year < 0L) | (year > 9999L)
    }
    # TOML doesn't allow YYYY or YYYY-MM dates as well as HH or HH:MM times
    # If HH:MM we'll assume 00 seconds
    # Other three cases we'll coerce to NA_datetimeoffset_
    if (toml) {
        x <- set_month(x, ifelse(is.na(get_day(x)), NA_integer_, get_month(x)))
        x <- set_year(x, ifelse(is.na(get_month(x)), NA_integer_, get_year(x)))
        x <- set_hour(x, ifelse(is.na(get_minute(x)), NA_integer_, get_hour(x)))
        x <- set_second(x, ifelse(is.na(get_second(x)), 0L, get_second(x)))
    }
    # No smaller time units if missing bigger time units
    x <- set_month(x, ifelse(is.na(get_year(x)), NA_integer_, get_month(x)))
    x <- set_day(x, ifelse(is.na(get_month(x)), NA_integer_, get_day(x)))
    if (allow_times)
        x <- set_hour(x, ifelse(!is_time(x) & is.na(get_day(x)), NA_integer_, get_hour(x)))
    else
        x <- set_hour(x, ifelse(is.na(get_day(x)), NA_integer_, get_hour(x)))
    x <- set_minute(x, ifelse(is.na(get_hour(x)), NA_integer_, get_minute(x)))
    x <- set_second(x, ifelse(is.na(get_minute(x)), NA_integer_, get_second(x)))
    x <- set_nanosecond(x, ifelse(is.na(get_second(x)), NA_integer_, get_nanosecond(x)))
    x <- set_minute_offset(x, ifelse(is.na(get_hour_offset(x)), NA_integer_, get_minute_offset(x)))

    # no time zones and offsets if no hours
    x <- set_hour_offset(x, ifelse(is.na(get_hour(x)), NA_integer_, get_hour_offset(x)))
    x <- set_minute_offset(x, ifelse(is.na(get_hour(x)), NA_integer_, get_minute_offset(x)))
    x <- set_tz(x, ifelse(is.na(get_hour(x)), NA_character_, get_tz(x)))

    if (pdfmark) { # if missing seconds then pdfmark offsets are missing
        x <- set_hour_offset(x, ifelse(is.na(get_second(x)), NA_integer_, get_hour_offset(x)))
        x <- set_minute_offset(x, ifelse(is.na(get_second(x)), NA_integer_, get_minute_offset(x)))
        x <- set_tz(x, ifelse(is.na(get_second(x)), NA_character_, get_tz(x)))
    }
    x
}

as_ymd_hms_str <- function(x, ...) {
    x <- datetime_cast(x, "second")

    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    paste0(year_str, month_str, day_str, hour_str, minute_str, second_str)
}

my_format <- function(x, prefix = "", width = 2L, flag = "0", edtf = FALSE, blank = FALSE) {
    s <- character(length(x))
    if (blank) return(s)
    idx <- which(!is.na(x))
    if (length(idx))
        s[idx] <- paste0(prefix, formatC(x[idx], format = "d", flag = flag, width = width))
    idx <- which(is.na(x))
    if (edtf && length(idx))
        s[idx] <- paste0(prefix, strrep("X", width))
    s
}

my_format_tz <- function(x, sep = ":", no_zulu = FALSE, edtf = FALSE, add_tz = FALSE) {
    tz <- field(x, "tz")
    hour_offset <- field(x, "hour_offset")
    minute_offset <- field(x, "minute_offset")
    hos <- my_format(hour_offset, width = 3L, flag = "0+")
    hos <- ifelse(is.na(hour_offset) & ((!is.na(minute_offset) & edtf) | (edtf & add_tz)), "+XX", hos)
    mos <- my_format(minute_offset, prefix = sep)
    mos <- ifelse(is.na(minute_offset) & edtf & add_tz, ":XX", mos)

    s <- paste0(hos, mos)

    id_zulu <- which(is_utc(tz))
    if (length(id_zulu) > 0L) {
        if (no_zulu) {
            s[id_zulu] <- paste0("+00", sep, "00")
        } else {
            s[id_zulu] <- "Z"
        }
    }

    id_tz <- which(!is.na(tz) & !is_utc(tz))
    if (length(id_tz) > 0L) {
        tz_id <- tz[id_tz]

        x[id_tz] <- fill_utc_offsets(x[id_tz])
        hour_offset <- field(x[id_tz], "hour_offset")
        minute_offset <- field(x[id_tz], "minute_offset")
        hos <- my_format(hour_offset, width = 3L, flag = "0+")
        mos <- my_format(minute_offset, prefix = sep)

        s_offsets <- paste0(hos, mos)
        s_offsets <- ifelse(is.na(hour_offset), s[id_tz][is.na(hour_offset)], s_offsets)
        if (add_tz) {
            s_offsets <- paste0(s_offsets, "[", tz_id, "]")
        }
        s[id_tz] <- s_offsets
    }
    idx <- which(is.na(tz))
    if (edtf && add_tz && length(idx))
        s[idx] <- paste0(s[idx], "[X]")
    s
}
