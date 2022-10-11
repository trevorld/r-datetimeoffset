#' Convert datetime objects to character
#'
#' `format()` returns an ISO 8601 datetime string with as much **known** information possible.
#' `format_ISO8601()` returns an ISO 8601 datetime string.
#' `format_pdfmark()` returns a pdfmark datetime string with as much **known** information possible.
#' @name format
#' @param x A [datetime_offset()] object.
#' @param usetz Include the time zone in the formatting (of outputs including
#'        time; date outputs never include time zone information).
#' @param precision The amount of precision to represent with substrings of
#'        "ymdhmsn", as "y"ear, "m"onth, "d"ay, "h"our, "m"inute,
#'        "s"econd, and "n"anosecond. (e.g. "ymdhm" would show precision through minutes.
#'        When ‘NULL’, full precision for the object is shown.
#' @param ... Ignored
#' @return A character vector
#' @examples
#'   # ISO 8601 datetimes
#'   format_ISO8601(as_datetime_offset("2020-05"))
#'   format_ISO8601(as_datetime_offset("2020-05-10 20:15"))
#'   format_ISO8601(as_datetime_offset("2020-05-10 20:15:05-07"))
#'
#'   # pdfmark datetimes
#'   format_pdfmark(as_datetime_offset("2020-05"))
#'   format_pdfmark(as_datetime_offset("2020-05-10 20:15"))
#'   format_pdfmark(as_datetime_offset("2020-05-10 20:15:05-07"))
NULL

#' @rdname format
#' @export
format.datetime_offset <- function(x, ...) {
    x <- update_nas(x)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    nanosecond_str <- my_format_nanosecond(field(x, "nanosecond"))
    offset_str <- my_format_tz(x)
    paste0(year_str, month_str, day_str,
           hour_str, minute_str, second_str, nanosecond_str,
           offset_str)
}

my_format_nanosecond <- function(ns) {
    s <- character(length(ns))
    idx <- which(!is.na(ns))
    if (length(idx > 0L)) {
        s_ns <- formatC(ns[idx], format = "d", flag = "0", width = 9L)
        stopifnot(all(nchar(s_ns) <= 9L))
        s_ns <- gsub("0{1,}$", "", s_ns)
        s[idx] <- paste0(".", s_ns)
    }
    s
}

setOldClass("datetime_offset")

#' @rdname format
#' @importFrom lubridate format_ISO8601
#' @export
methods::setMethod("format_ISO8601", signature = "datetime_offset", function(x, usetz = TRUE, precision = NULL, ...) {
    precision <- precision %||% "ymdhmsn"
    stopifnot(precision %in% c("y", "ym", "ymd", "ymdh", "ymdhm", "ymdhms", "ymdhmsn"))
    if (precision == "y")
        month(x) <- NA_integer_
    if (precision == "ym")
        day(x) <- NA_integer_
    if (precision == "ymd")
        hour(x) <- NA_integer_
    if (precision == "ymdh")
        minute(x) <- NA_integer_
    if (precision == "ymdhm")
        second(x) <- NA_integer_
    if (precision == "ymdhms")
        nanosecond(x) <- NA_integer_
    if (isFALSE(usetz)) {
        hour_offset(x) <- NA_integer_
        minute_offset(x) <- NA_integer_
        tz(x) <- NA_character_
    }
    x <- update_nas(x)
    format(x)
})

#' @rdname format
#' @export
format_pdfmark <- function(x, ...) {
    UseMethod("format_pdfmark")
}

#' @rdname format
#' @export
format_pdfmark.default <- function(x, ...) {
    format_pdfmark(as_datetime_offset(x))
}

#' @rdname format
#' @export
format_pdfmark.datetime_offset <- function(x, ...) {
    x <- update_nas(x, pdfmark = TRUE)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "")
    day_str <- my_format(field(x, "day"), prefix = "")
    hour_str <- my_format(field(x, "hour"), prefix = "")
    minute_str <- my_format(field(x, "minute"), prefix = "")
    second_str <- my_format(field(x, "second"), prefix = "")
    offset_str <- my_format_tz(x, sep = "", no_zulu = TRUE)
    paste0("D:", year_str, month_str, day_str, hour_str, minute_str, second_str, offset_str)
}

update_nas <- function(x, pdfmark = FALSE) {
    # No smaller time units if missing bigger time units
    day(x) <- ifelse(is.na(month(x)), NA_integer_, day(x))
    hour(x) <- ifelse(is.na(day(x)), NA_integer_, hour(x))
    minute(x) <- ifelse(is.na(hour(x)), NA_integer_, minute(x))
    second(x) <- ifelse(is.na(minute(x)), NA_integer_, second(x))
    minute_offset(x) <- ifelse(is.na(hour_offset(x)), NA_integer_, minute_offset(x))

    # no time zones and offsets if no hours
    hour_offset(x) <- ifelse(is.na(hour(x)), NA_integer_, hour_offset(x))
    minute_offset(x) <- ifelse(is.na(hour(x)), NA_integer_, minute_offset(x))
    tz(x) <- ifelse(is.na(hour(x)), NA_character_, tz(x))

    if (pdfmark) { # if missing seconds then pdfmark offsets are missing
        tz(x) <- ifelse(is.na(second(x)), NA_character_, tz(x))
        hour_offset(x) <- ifelse(is.na(second(x)), NA_integer_, hour_offset(x))
        minute_offset(x) <- ifelse(is.na(second(x)), NA_integer_, minute_offset(x))
    }

    x
}

as_ymd_hms_str <- function(x, ...) {
    minute(x) <- update_missing(minute(x), 0)
    second(x) <- update_missing(second(x), 0)

    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    paste0(year_str, month_str, day_str, hour_str, minute_str, second_str)
}

my_format <- function(x, prefix = "", width = 2L, flag = "0") {
    s <- character(length(x))
    idx <- which(!is.na(x))
    if (length(idx))
        s[idx] <- paste0(prefix, formatC(x, format = "d", flag = flag, width = width))
    s
}

my_format_tz <- function(x, sep = ":", no_zulu = FALSE) {
    tz <- field(x, "tz")
    hour_offset <- field(x, "hour_offset")
    minute_offset <- field(x, "minute_offset")

    s <- paste0(my_format(hour_offset, width = 3L, flag = "0+"),
                my_format(minute_offset, prefix = sep))

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
                                       dt <- as.nanotime(x, tz = tz)
                                       format(dt, format = "%z", tz = tz)
                                   })
        offsets <- ifelse(is_utc(tz_id), "Z",
                          paste0(substr(offsets, 1, 3), sep, substr(offsets, 4, 5)))
        s[id_tz] <- offsets
    }
    s
}
