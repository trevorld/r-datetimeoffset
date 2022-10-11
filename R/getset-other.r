#' Get/set datetime components not supported by lubridate
#'
#' Datetime component getter/setter functions not supported by `lubridate`:
#'
#' * `nanosecond()` and `nanosecond<-()` can be used to get/set nanoseconds.
#' * `hour_offset()` and `hour_offset<-()` can be used to get/set UTC offset hours
#' * `minute_offset()` and `minute_offset<-()` can be used to get/set UTC offset minutes
#'
#' @param x A [datetime_offset()] object.
#' @param value Replacement value
#' @seealso [getset_lubridate]
#' @name getset_other
#' @examples
#'   nanosecond(Sys.Date())
#'   dt <- as_datetime_offset("2020-04-04T07:30:20")
#'   nanosecond(dt) <- 3e6 # 3 milliseconds
#'   nanosecond(dt)
#'   nanosecond(dt) <- 3e3 # 3 microseconds
#'   nanosecond(dt)
#'   nanosecond(dt) <- 3 # 3 nanoseconds
#'   nanosecond(dt)
#'   dt <- as_datetime_offset("2020-04-08T20:12:16")
#'   hour_offset(dt) <- -7
#'   hour_offset(dt)
#'   minute_offset(dt) <- 30
#'   minute_offset(dt)
#'   dt <- Sys.time()
#'   hour_offset(dt)
#'   minute_offset(dt)
NULL

#' @rdname getset_other
#' @export
nanosecond <- function(x) {
    UseMethod("nanosecond")
}

#' @rdname getset_other
#' @export
nanosecond.default <- function(x) {
    nanosecond(as_datetime_offset(x))
}

#' @rdname getset_other
#' @export
nanosecond.datetime_offset <- function(x) {
    field(x, "nanosecond")
}

#' @rdname getset_other
#' @export
"nanosecond<-" <- function(x, value) {
    UseMethod("nanosecond<-")
}

#' @rdname getset_other
#' @export
"nanosecond<-.default" <- function(x, value) {
    stop(paste("Don't support object of class", class(x)))
}

#' @rdname getset_other
#' @export
"nanosecond<-.datetime_offset" <- function(x, value) {
    value <- as.integer(value)
    s <- formatC(value, format = "d", flag = "0", width = 9L)
    stopifnot(isFALSE(any(nchar(s) > 9L)))
    field(x, "nanosecond") <- value
    x
}

#' @rdname getset_other
#' @export
hour_offset <- function(x) {
    UseMethod("hour_offset")
}

#' @rdname getset_other
#' @export
hour_offset.datetime_offset <- function(x) {
    field(x, "hour_offset")
}

#' @rdname getset_other
#' @export
hour_offset.POSIXlt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname getset_other
#' @export
hour_offset.POSIXct <- function(x) {
    as.integer(substr(format(x, format = "%z"), 1, 3))
}

#' @rdname getset_other
#' @export
hour_offset.default <- function(x) {
    hour_offset(as.POSIXlt(x))
}


#' @rdname getset_other
#' @export
"hour_offset<-" <- function(x, value) {
    UseMethod("hour_offset<-")
}

#' @rdname getset_other
#' @export
"hour_offset<-.datetime_offset" <- function(x, value) {
    field(x, "hour_offset") <- as.integer(value)
    x
}

#' @export
"hour_offset<-.default" <- function(x, value) {
    stop(paste("Method not defined for an object of class", class(x)))
}

#' @rdname getset_other
#' @export
minute_offset <- function(x) {
    UseMethod("minute_offset")
}

#' @rdname getset_other
#' @export
minute_offset.datetime_offset <- function(x) {
    field(x, "minute_offset")
}

#' @rdname getset_other
#' @export
minute_offset.POSIXlt <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname getset_other
#' @export
minute_offset.POSIXct <- function(x) {
    as.integer(substr(format(x, format = "%z"), 4, 5))
}

#' @rdname getset_other
#' @export
minute_offset.default <- function(x) {
    minute_offset(as.POSIXlt(x))
}

#' @rdname getset_other
#' @export
"minute_offset<-" <- function(x, value) {
    UseMethod("minute_offset<-")
}

#' @rdname getset_other
#' @export
"minute_offset<-.datetime_offset" <- function(x, value) {
    field(x, "minute_offset") <- as.integer(value)
    x
}

#' @export
"minute_offset<-.default" <- function(x, value) {
    stop(paste("Method not defined for an object of class", class(x)))
}
