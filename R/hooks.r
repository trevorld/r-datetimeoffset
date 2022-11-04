.onLoad <- function(libname, pkgname) {
    vctrs::s3_register("lubridate::year", "datetimeoffset", get_year.datetimeoffset)
    vctrs::s3_register("lubridate::month", "datetimeoffset", get_month.datetimeoffset)
    vctrs::s3_register("lubridate::mday", "datetimeoffset", get_day.datetimeoffset)
    vctrs::s3_register("lubridate::hour", "datetimeoffset", get_hour.datetimeoffset)
    vctrs::s3_register("lubridate::minute", "datetimeoffset", get_minute.datetimeoffset)
    vctrs::s3_register("lubridate::second", "datetimeoffset", get_second.datetimeoffset)
    vctrs::s3_register("lubridate::tz", "datetimeoffset", get_tz.datetimeoffset)
    vctrs::s3_register("lubridate::date", "datetimeoffset", as.Date.datetimeoffset)
    vctrs::s3_register("lubridate::force_tz", "datetimeoffset",
                       function(time, tzone = "", ...) set_tz.datetimeoffset(time, tzone))

    if (requireNamespace("lubridate", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("format_ISO8601", lubridate::format_ISO8601)
        methods::setGeneric("year<-", lubridate::"year<-")
        methods::setGeneric("month<-", lubridate::"month<-")
        methods::setGeneric("day<-", lubridate::"day<-")
        methods::setGeneric("hour<-", lubridate::"hour<-")
        methods::setGeneric("minute<-", lubridate::"minute<-")
        methods::setGeneric("second<-", lubridate::"second<-")
        methods::setGeneric("date<-", lubridate::"date<-")
        methods::setMethod("format_ISO8601", signature = "datetimeoffset",
                           function(x, usetz = FALSE, precision = NULL, ...) {
                               if (!is.null(precision))
                                   precision <- switch(precision,
                                                       y = "year", ym = "month", ymd = "day",
                                                       ymdh = "hour", ymdhm = "minute", ymdhms = "second",
                                                       stop(paste("Don't recognize precision", sQuote(precision))))
                               format_iso8601(x, offsets = usetz, precision = precision, sep = "")
                           })
        methods::setMethod("year<-", "datetimeoffset",
                           function(x, value) set_year.datetimeoffset(x, value))
        methods::setMethod("month<-", "datetimeoffset",
                           function(x, value) set_month.datetimeoffset(x, value))
        methods::setMethod("day<-", "datetimeoffset",
                           function(x, value) set_day.datetimeoffset(x, value))
        methods::setMethod("hour<-", "datetimeoffset",
                           function(x, value) set_hour.datetimeoffset(x, value))
        methods::setMethod("minute<-", "datetimeoffset",
                           function(x, value) set_minute.datetimeoffset(x, value))
        methods::setMethod("second<-", "datetimeoffset",
                           function(x, value) set_second.datetimeoffset(x, value))
        methods::setMethod("date<-", "datetimeoffset",
            function(x, value) {
                x <- set_year(x, get_year(value))
                x <- set_month(x, get_month(value))
                x <- set_day(x, get_day(value))
                x
            })
    }

    if (requireNamespace("nanotime", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("as.nanotime", nanotime::as.nanotime)
        methods::setMethod("as.nanotime", methods::signature(from="datetimeoffset"),
            function(from, tz = "") as.nanotime.datetimeoffset(from, tz = tz))
    }
}

# Silence R CMD check note:
# ** checking whether the namespace can be loaded with stated dependencies ... NOTE
# Warning in .undefineMethod("initialize", Class, classWhere) :
#   no generic function 'initialize' found
#
# I'm not sure why this is necessary, but I suspect it's due to the use of
# setOldClass onLoad
#' @importFrom methods initialize
NULL
