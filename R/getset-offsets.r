#' Get/set UTC offsets
#'
#' `hour_offset()` and `minute_offset()` can be used to get/set UTC offsets.
#' @param x Object to get UTC offsets from
#' @param value Replacement value
#' @name utc_offsets
#' @examples
#' dt <- as_datetime_offset("2020-04-08T20:12:16")
#' hour_offset(dt) <- -7
#' hour_offset(dt)
#' minute_offset(dt) <- 30
#' minute_offset(dt)
#' dt <- Sys.time()
#' hour_offset(dt)
#' minute_offset(dt)
NULL

#' @rdname utc_offsets
#' @export
hour_offset <- function(x) {
    UseMethod("hour_offset")
}

#' @rdname utc_offsets
#' @export
hour_offset.datetime_offset <- function(x) {
    field(x, "hour_offset")
}

#' @rdname utc_offsets
#' @export
hour_offset.POSIXlt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname utc_offsets
#' @export
hour_offset.POSIXct <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname utc_offsets
#' @export
hour_offset.default <- function(x) {
    hour_offset(as.POSIXlt(x))
}


#' @rdname utc_offsets
#' @export
"hour_offset<-" <- function(x, value) {
    UseMethod("hour_offset<-")
}

#' @rdname utc_offsets
#' @export
"hour_offset<-.datetime_offset" <- function(x, value) {
    field(x, "hour_offset") <- as.integer(value)
    x
}

"hour_offset<-.default" <- function(x, value) {
    stop(paste("Method not defined for an object of class", class(x)))
}

#' @rdname utc_offsets
#' @export
minute_offset <- function(x) {
    UseMethod("minute_offset")
}

#' @rdname utc_offsets
#' @export
minute_offset.datetime_offset <- function(x) {
    field(x, "minute_offset")
}

#' @rdname utc_offsets
#' @export
minute_offset.POSIXlt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname utc_offsets
#' @export
minute_offset.POSIXct <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname utc_offsets
#' @export
minute_offset.default <- function(x) {
    minute_offset(as.POSIXlt(x))
}

#' @rdname utc_offsets
#' @export
"minute_offset<-" <- function(x, value) {
    UseMethod("minute_offset<-")
}

#' @rdname utc_offsets
#' @export
"minute_offset<-.datetime_offset" <- function(x, value) {
    field(x, "minute_offset") <- as.integer(value)
    x
}

"minute_offset<-.default" <- function(x, value) {
    stop(paste("Method not defined for an object of class", class(x)))
}
