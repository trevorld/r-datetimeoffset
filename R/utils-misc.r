
na_omit <- function(x) Filter(Negate(is.na), x)

is_utc <- function(tz) {
    (tz %||% Sys.timezone()) %in% c("UTC", "GMT", "Etc/UTC", "Etc/GMT", "GMT-0", "GMT+0", "GMT0")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
