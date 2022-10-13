#' Add/subtract datetime periods
#'
#' We can add/substract [nanotime::nanoperiod()] or [lubridate::period()] objects with [datetimeoffset()] objects.
#'
#' @name period
#' @param e1,e2 See [methods::Ops].
#' @seealso [duration] for adding/subtracting duration objects.
#' @examples
#' dt <- as_datetimeoffset(Sys.time())
#' np <- nanotime::nanoperiod(months = 12)
#' dt + np
#' np + dt
#' dt - np
#' lp <- lubridate::period(year = 1)
#' dt + lp
#' lp + dt
#' dt - lp
#'
#' # compare DST handling to durations
#' boundary <- as_datetimeoffset("2009-03-08 01:59:59", tz = "America/Chicago")
#' boundary + lubridate::days(1) # period
#' boundary + lubridate::ddays(1) # duration
#' boundary + nanotime::nanoperiod(day = 1) # period
#' boundary + nanotime::nanoduration(hour = 24, minute = 0, second = 0, nanosecond = 0) # duration
NULL

methods::setOldClass("datetimeoffset")

#' @rdname period
#' @export
setMethod("+", c("datetimeoffset", "nanoperiod"), function(e1, e2) {
    tz <- clean_tz(tz(e1), na = Sys.timezone())
    n <- length(tz)
    if (length(e2) < n)
        e2 <- rep(e2, length.out = n)
    df <- data.frame(dt = as.nanotime(e1, tz = tz), tz = tz, np = e2,
                     stringsAsFactors = FALSE)
    purrr::pmap_vec(df, function(dt, tz, np) {
                        as_datetimeoffset(nanotime::plus(dt, np, tz), tz = tz)
                    },
                    .ptype = datetimeoffset())
})

#' @rdname period
#' @export
setMethod("-", c("datetimeoffset", "nanoperiod"), function(e1, e2) {
    e1 + -e2
})

#' @rdname period
#' @export
setMethod("+", c("nanoperiod", "datetimeoffset"), function(e1, e2) {
    e2 + e1
})

#' @rdname period
#' @export
setMethod("+", c("datetimeoffset", "Period"), function(e1, e2) {
    tz <- clean_tz(tz(e1), na = Sys.timezone())
    n <- length(tz)
    if (length(e2) < n)
        e2 <- rep(e2, length.out = n)
    df <- data.frame(dt = e1, tz = tz, lp = e2,
                     stringsAsFactors = FALSE)
    l <- purrr::pmap_vec(df, function(dt, tz, lp) {
                             lt <- as.POSIXct(dt, tz = tz)
                             as_datetimeoffset(lt + lp)
                         },
                         .ptype = datetimeoffset())
})

#' @rdname period
#' @export
setMethod("-", c("datetimeoffset", "Period"), function(e1, e2) {
    e1 + -e2
})

#' @rdname period
#' @export
setMethod("+", c("Period", "datetimeoffset"), function(e1, e2) {
    e2 + e1
})
