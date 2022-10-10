#' Get/set nanoseconds
#'
#' `nanosecond()` and `nanosecond<-()` can be used to get/set nanoseconds.
#'
#' @param x Datetime object to get nanoseconds from
#' @param value Integer of nanoseconds
#' @examples
#'   nanosecond(Sys.Date())
#'   dt <- as_datetime_offset("2020-04-04T07:30:20")
#'   nanosecond(dt) <- 3e6 # 3 milliseconds
#'   nanosecond(dt)
#'   nanosecond(dt) <- 3e3 # 3 microseconds
#'   nanosecond(dt)
#'   nanosecond(dt) <- 3 # 3 nanoseconds
#'   nanosecond(dt)
#' @name nanosecond
NULL

#' @rdname nanosecond
#' @export
nanosecond <- function(x) {
    UseMethod("nanosecond")
}

#' @rdname nanosecond
#' @export
nanosecond.default <- function(x) {
    nanosecond(as_datetime_offset(x))
}

#' @rdname nanosecond
#' @export
nanosecond.datetime_offset <- function(x) {
    field(x, "nanosecond")
}

#' @rdname nanosecond
#' @export
"nanosecond<-" <- function(x, value) {
    UseMethod("nanosecond<-")
}

#' @rdname nanosecond
#' @export
"nanosecond<-.default" <- function(x, value) {
    stop(paste("Don't support object of class", class(x)))
}

#' @rdname nanosecond
#' @export
"nanosecond<-.datetime_offset" <- function(x, value) {
    value <- as.integer(value)
    s <- formatC(value, format = "d", flag = "0", width = 9L)
    stopifnot(isFALSE(any(nchar(s) > 9L)))
    field(x, "nanosecond") <- value
    x
}
