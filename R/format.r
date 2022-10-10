#' @export
format.datetime_offset <- function(x, ...) {
    x <- update_nas(x)
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = "T")
    minute_str <- my_format(field(x, "minute"), prefix = ":")
    second_str <- my_format(field(x, "second"), prefix = ":")
    offset_str <- my_format_tz(x)
    paste0(year_str, month_str, day_str, hour_str, minute_str, second_str, offset_str)
}

update_nas <- function(x) {
    day(x) <- ifelse(is.na(month(x)), NA_integer_, day(x))
    hour(x) <- ifelse(is.na(day(x)), NA_integer_, hour(x))
    minute(x) <- ifelse(is.na(hour(x)), NA_integer_, minute(x))
    second(x) <- ifelse(is.na(minute(x)), NA_integer_, second(x))

    # no time zones and offsets if no hours
    hour_offset(x) <- ifelse(is.na(hour(x)), NA_integer_, hour_offset(x))
    minute_offset(x) <- ifelse(is.na(hour(x)), NA_integer_, minute_offset(x))
    tz(x) <- ifelse(is.na(hour(x)), NA_character_, tz(x))
    minute_offset(x) <- ifelse(is.na(hour_offset(x)), NA_integer_, minute_offset(x))
    minute_offset(x) <- ifelse(is.na(minute(x)), NA_integer_, minute_offset(x))
    x
}

as_ymd_hms_str <- function(x, ...) {
    year_str <- my_format(field(x, "year"), width = 4L)
    month_str <- my_format(field(x, "month"), prefix = "-")
    day_str <- my_format(field(x, "day"), prefix = "-")
    hour_str <- my_format(field(x, "hour"), prefix = " ")
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

my_format_tz <- function(x, sep = ":") {
    tz <- field(x, "tz")
    hour_offset <- field(x, "hour_offset")
    minute_offset <- field(x, "minute_offset")

    s <- paste0(my_format(hour_offset, width = 3L, flag = "0+"),
                my_format(minute_offset, prefix = ":"))

    id_zulu <- which(is_utc(tz))
    if (length(id_zulu) > 0L)
        s[id_zulu] <- "Z"

    id_tz <- which(!is.na(tz) && !is_utc(tz))
    if (length(id_tz) > 0L) {
        tz_id <- tz[id_tz]
        df <- data.frame(x = as_ymd_hms_str(x[id_tz]), tz = tz_id,
                         stringsAsFactors = FALSE)
        dts <- purrr::pmap(df, function(x, tz) lubridate::ymd_hms(x, tz = tz))
        offsets <- vapply(dts,
                          function(x) strftime(x, format = "%z", tz = tz(x)),
                          character(1), USE.NAMES = FALSE)
        offsets <- ifelse(is_utc(tz_id), "Z",
                          paste0(substr(offsets, 1, 3), sep, substr(offsets, 4, 5)))
        s[id_tz] <- offsets
    }
    s
}
