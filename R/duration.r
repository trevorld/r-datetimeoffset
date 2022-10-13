#' Add/subtract datetime durations
#'
#' We can add/substract [nanotime::nanoduration()] or [lubridate::duration()] objects with [datetimeoffset()] objects.
#' The difference of two [datetimeoffset()] objects is a [nanotime::nanoduration()] object.
#' One can use [vctrs::vec_arith()] to add/subtract [base::difftime()] objects with [datetimeoffset()] objects.
#'
#' @name duration
#' @param op,x,y See [vctrs::vec_arith()].
#' @param e1,e2 See [methods::Ops].
#' @param ... Ignored
#' @seealso [period] for adding/subtracting period objects.
#' @examples
#' dt <- as_datetimeoffset(Sys.time())
#'
#' # nanotime::nanoduration() durations
#' class(dt - dt)
#' nd <- nanotime::nanoduration(hours = 3, minutes = 0, seconds = 0, nanoseconds = 0)
#' dt + nd
#' nd + dt
#' dt - nd
#'
#' # lubridate::duration() durations
#' ld <- lubridate::duration(hour = 3)
#' dt + ld
#' ld + dt
#' dt - ld
#'
#' # base::difftime() durations
#' bd <- as.difftime(3, units = "hours")
#' vctrs::vec_arith("+", dt, bd)
#' vctrs::vec_arith("-", dt, bd)
#' vctrs::vec_arith("+", bd, dt)
#' dt + nanotime::as.nanoduration(bd)
#' dt + lubridate::as.duration(bd)
#'
#' # compare DST handling to periods
#' boundary <- as_datetimeoffset("2009-03-08 01:59:59", tz = "America/Chicago")
#' boundary + lubridate::days(1) # period
#' boundary + lubridate::ddays(1) # duration
#' boundary + nanotime::nanoperiod(day = 1) # period
#' boundary + nanotime::nanoduration(hour = 24, minute = 0, second = 0, nanosecond = 0) # duration
NULL

methods::setOldClass("datetimeoffset")

#' @rdname duration
#' @export vec_arith.datetimeoffset
#' @method vec_arith datetimeoffset
#' @export
vec_arith.datetimeoffset <- function(op, x, y, ...) {
    UseMethod("vec_arith.datetimeoffset", y)
}

#' @rdname duration
#' @method vec_arith.datetimeoffset default
#' @export
vec_arith.datetimeoffset.default <- function(op, x, y, ...) {
    stop_incompatible_op(op, x, y)
}

#' @rdname duration
#' @method vec_arith.datetimeoffset datetimeoffset
#' @export
vec_arith.datetimeoffset.datetimeoffset <- function(op, x, y, ...) {
    if (op != "-") stop_incompatible_op(op, x, y)
    as.nanotime(x) - as.nanotime(y)
}

#' @rdname duration
#' @method vec_arith.datetimeoffset numeric
#' @export
vec_arith.datetimeoffset.numeric <- function(op, x, y, ...) {
    if (op == "-") {
        as_datetimeoffset(as.nanotime(x, tz = tz(x)) - y, tz = tz(x))
    } else if (op == "+") {
        as_datetimeoffset(as.nanotime(x, tz = tz(x)) + y, tz = tz(x))
    } else {
        stop_incompatible_op(op, x, y)
    }
}

#' @rdname duration
#' @method vec_arith.numeric datetimeoffset
#' @export
vec_arith.numeric.datetimeoffset <- function(op, x, y, ...) {
    if (op == "+") {
        as_datetimeoffset(as.nanotime(y, tz = tz(y)) + x, tz = tz(y))
    } else {
        stop_incompatible_op(op, x, y)
    }
}

#' @rdname duration
#' @method vec_arith.datetimeoffset difftime
#' @export
vec_arith.datetimeoffset.difftime <- function(op, x, y, ...) {
    if (op == "-") {
        x - as.nanoduration(y)
    } else if (op == "+") {
        x + as.nanoduration(y)
    } else {
        stop_incompatible_op(op, x, y)
    }
}

#' @rdname duration
#' @method vec_arith.difftime datetimeoffset
#' @export
vec_arith.difftime.datetimeoffset <- function(op, x, y, ...) {
    if (op == "+") {
        y + as.nanoduration(x)
    } else {
        stop_incompatible_op(op, x, y)
    }
}

#' @rdname duration
#' @export
setMethod("+", c("datetimeoffset", "nanoduration"), function(e1, e2) {
    as_datetimeoffset(as.nanotime(e1) + e2, tz = tz(e1))
})

#' @rdname duration
#' @export
setMethod("-", c("datetimeoffset", "nanoduration"), function(e1, e2) {
    e1 + -e2
})

#' @rdname duration
#' @export
setMethod("+", c("nanoduration", "datetimeoffset"), function(e1, e2) {
    e2 + e1
})

#' @importFrom nanotime as.nanoduration
setMethod("as.nanoduration", "difftime", function(x) {
    x <- as.numeric(x, units = "secs")
    s <- as.integer(x) # seconds
    n <- as.integer(1e9 * (x - s)) # nanoseconds
    nd <- nanotime::nanoduration(hour = 0, minute = 0, second = s, nanosecond = n)
})

setMethod("as.nanoduration", "Duration", function(x) {
    x <- as.numeric(x, "seconds")
    s <- as.integer(x) # seconds
    n <- as.integer(1e9 * (x - s)) # nanoseconds
    nanotime::nanoduration(hour = 0, minute = 0, second = s, nanosecond = n)
})

#' @rdname duration
#' @export
setMethod("+", c("datetimeoffset", "Duration"), function(e1, e2) {
    nd <- as.nanoduration(e2)
    as_datetimeoffset(as.nanotime(e1) + nd, tz = tz(e1))
})

#' @rdname duration
#' @export
setMethod("-", c("datetimeoffset", "Duration"), function(e1, e2) {
    e1 + -e2
})

#' @rdname duration
#' @export
setMethod("+", c("Duration", "datetimeoffset"), function(e1, e2) {
    e2 + e1
})
