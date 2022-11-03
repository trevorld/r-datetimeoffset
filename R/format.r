#' Convert datetime objects to character
#'
#' `format()` returns a datetime string
#'  with as much **known** information possible (RFC 3339 with de facto standard time zone extension).
#' `format_iso8601()` returns an ISO 8601 datetime string.
#' `format_pdfmark()` returns a pdfmark datetime string with as much **known** information possible.
#' `format_strftime()` allows [base::strftime()] style formatting.
#' `format_nanotime()` allows CCTZ style formatting.
#' `format_edtf()` returns an Extended Date Time Format (EDTF) string.
#' @name format
#' @param x A [datetimeoffset()] object.
#' @param format For `format_strftime()` see [base::strftime()].
#'               For `format_nanotime()` see <https://github.com/google/cctz/blob/6e09ceb/include/time_zone.h#L197>.
#' @param tz A character string specifying the time zone to be used for the conversion.
#'           Can be a length greater than one.
#' @param offsets Include the UTC offsets in the formatting
#' @param usetz Include the time zone in the formatting
#' @param precision The amount of precision: either "year", "month", "day", "hour", "minute", "second", or "nanosecond".
#'        If `NULL` then full precision for the object is shown.
#' @param ... Ignored
#' @return A character vector
#' @examples
#'   # ISO 8601 datetimes
#'   format_iso8601(as_datetimeoffset("2020-05"))
#'   format_iso8601(as_datetimeoffset("2020-05-10 20:15"))
#'   format_iso8601(as_datetimeoffset("2020-05-10 20:15:05-07"))
#'   if (require("lubridate"))
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
#'   if (require("nanotime")) {
#'     dt <- as_datetimeoffset(Sys.time())
#'     format_nanotime(dt, format = "%F %H:%M:%E7S %Ez") # SQL Server datetimeoffset
#'   }
#'
#'   # EDTF style formatting
#'   format_edtf(as_datetimeoffset("2020-05"))
#'   format_edtf(as_datetimeoffset("2020-05-10T20:15:05-07"))
#'   dt <- datetimeoffset(2020, NA_integer_, 10)
#'   format_edtf(dt)
#'   # Not a valid EDTF string but useful serialization of `datetimeoffset()`
#'   format_edtf(dt, precision = "nanosecond", usetz = TRUE)
NULL

#' @rdname format
#' @export
format.datetimeoffset <- function(x, ...) {
    x <- update_nas(x)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"))
    offset_str <- my_format_tz(x, add_tz = TRUE)
    s <- paste0(year_str, month_str, day_str,
                hour_str, minute_str, second_str, nanosecond_str,
                offset_str)
    is.na(s) <- is.na(field(x, "year"))
    s
}

my_format_nanosecond <- function(ns, edtf = FALSE, blank = FALSE) {
    s <- character(length(ns))
    if (blank) return(s)
    idx <- which(!is.na(ns))
    if (length(idx > 0L)) {
        s_ns <- formatC(ns[idx], format = "d", flag = "0", width = 9L)
        stopifnot(all(nchar(s_ns) <= 9L))
        s_ns <- gsub("0{1,}$", "", s_ns)
        s_ns <- ifelse(s_ns == "", "0", s_ns)
        s[idx] <- paste0(".", s_ns)
    }
    idx <- which(is.na(ns))
    if (edtf && length(idx))
        s[idx] <- rep_len(".XXXXXXXXX", length(idx))
    s
}

#' @rdname format
#' @param sep UTC offset separator.  Either ":" or "".
#' @export
format_iso8601 <- function(x, offsets = TRUE, precision = NULL, sep = ":", ...) {
    precision <- precision %||% "nanosecond"
    stopifnot(precision %in% c("year", "month", "day", "hour", "minute", "second", "nanosecond"))
    stopifnot(sep %in% c(":", ""))
    x <- datetime_narrow(x, precision)
    if (isFALSE(offsets)) {
        x <- set_hour_offset(x, NA_integer_)
        x <- set_minute_offset(x, NA_integer_)
        x <- set_tz(x, NA_character_)
    }
    x <- update_nas(x)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"))
    offset_str <- my_format_tz(x, sep = sep)
    s <- paste0(year_str, month_str, day_str,
                hour_str, minute_str, second_str, nanosecond_str,
                offset_str)
    is.na(s) <- is.na(field(x, "year"))
    s
}

#' @rdname format
#' @export
format_pdfmark <- function(x) {
    x <- as_datetimeoffset(x)
    x <- update_nas(x, pdfmark = TRUE)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "")
    day_str <- my_format(field(x, "day"), prefix = "")
    hour_str <- my_format(field(x, "hour"), prefix = "")
    minute_str <- my_format(field(x, "minute"), prefix = "")
    second_str <- my_format(field(x, "second"), prefix = "")
    offset_str <- my_format_tz(x, sep = "'", no_zulu = TRUE)
    offset_str <- ifelse(offset_str == "", "", paste0(offset_str, "'"))
    s <- paste0("D:", year_str, month_str, day_str, hour_str, minute_str, second_str, offset_str)
    is.na(s) <- is.na(field(x, "year"))
    s
}

#' @rdname format
#' @export
format_strftime <- function(x, format = "%Y-%m-%d %H:%M:%S", tz = get_tz(x), usetz = FALSE) {
    tz <- clean_tz(tz, na = Sys.timezone())
    x <- as.POSIXct(x)
    df <- data.frame(x = x, format = format, tz = tz, usetz = usetz, stringsAsFactors = FALSE)
    purrr::pmap_chr(df, strftime)
}

#' @rdname format
#' @export
format_nanotime <- function(x, format = "%Y-%m-%dT%H:%M:%E9S%Ez", tz = get_tz(x)) {
    assert_suggested("nanotime")
    tz <- clean_tz(tz, na = Sys.timezone())
    x <- nanotime::as.nanotime(x)
    df <- data.frame(x = x, format = format, tz = tz, stringsAsFactors = FALSE)
    purrr::pmap_chr(df, base::format)
}

#' @rdname format
#' @export
format_edtf <- function(x, offsets = TRUE, precision = NULL, usetz = FALSE, ...) {
    purrr::map_chr(x, format_edtf_helper, offsets = offsets, precision = precision, usetz = usetz)
}

format_edtf_helper <- function(x, offsets, precision, usetz) {
    precision <- precision %||% datetime_precision(x, unspecified = TRUE)
    stopifnot(precision %in% c("missing", "year", "month", "day", "hour", "minute", "second", "nanosecond"))
    x <- datetime_narrow(x, precision)
    if (isFALSE(offsets)) {
        x <- set_hour_offset(x, NA_integer_)
        x <- set_minute_offset(x, NA_integer_)
        x <- set_tz(x, NA_character_)
    }
    precision <- dto_precision_integer(precision)
    year_str <- my_format(field(x, "year"), width = 4L, edtf = TRUE, blank = precision < PRECISION_YEAR)
    month_str <- my_format(field(x, "month"), prefix = "-", edtf = TRUE, blank = precision < PRECISION_MONTH)
    day_str <- my_format(field(x, "day"), prefix = "-", edtf = TRUE, blank = precision < PRECISION_DAY)
    hour_str <- my_format(field(x, "hour"), prefix = "T", edtf = TRUE, blank = precision < PRECISION_HOUR)
    minute_str <- my_format(field(x, "minute"), prefix = ":", edtf = TRUE, blank = precision < PRECISION_MINUTE)
    second_str <- my_format(field(x, "second"), prefix = ":", edtf = TRUE, blank = precision < PRECISION_SECOND)
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"), edtf = TRUE, blank = precision < PRECISION_NANOSECOND)
    offset_str <- my_format_tz(x, edtf = offsets, add_tz = usetz)
    paste0(year_str, month_str, day_str,
           hour_str, minute_str, second_str, nanosecond_str,
           offset_str)
}

update_nas <- function(x, pdfmark = FALSE) {
    # No smaller time units if missing bigger time units
    x <- set_day(x, ifelse(is.na(get_month(x)), NA_integer_, get_day(x)))
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
    x <- set_minute(x, update_missing(get_minute(x), 0L))
    x <- set_second(x, update_missing(get_second(x), 0L))

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
        df <- data.frame(x = as_ymd_hms_str(x[id_tz]), tz = tz_id,
                         stringsAsFactors = FALSE)
        offsets <- purrr::pmap_chr(df, function(x, tz) {
                                       dt <- clock::naive_time_parse(x)
                                       dt <- clock::as_zoned_time(dt, tz,
                                                                  ambiguous = "NA", nonexistent = "error")
                                       format(dt, format = "%z")
                                   })
        s_offsets <- paste0(substr(offsets, 1, 3), sep, substr(offsets, 4, 5))
        s_offsets <- ifelse(is.na(offsets), "", s_offsets)
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
