.onLoad <- function(libname, pkgname) {
    vctrs::s3_register("lubridate::year", "datetimeoffset", get_year.datetimeoffset)
    vctrs::s3_register("lubridate::month", "datetimeoffset", month.datetimeoffset)
    vctrs::s3_register("lubridate::mday", "datetimeoffset", get_day.datetimeoffset)
    vctrs::s3_register("lubridate::hour", "datetimeoffset", get_hour.datetimeoffset)
    vctrs::s3_register("lubridate::minute", "datetimeoffset", get_minute.datetimeoffset)
    vctrs::s3_register("lubridate::second", "datetimeoffset", get_second.datetimeoffset)
    vctrs::s3_register("lubridate::tz", "datetimeoffset", get_tz.datetimeoffset)
    vctrs::s3_register("lubridate::date", "datetimeoffset", function(x) as.Date.datetimeoffset(x))
    vctrs::s3_register("lubridate::force_tz", "datetimeoffset", force_tz.datetimeoffset)
    vctrs::s3_register("lubridate::with_tz", "datetimeoffset", with_tz.datetimeoffset)
    vctrs::s3_register("lubridate::wday", "datetimeoffset", wday.datetimeoffset)
    vctrs::s3_register("lubridate::qday", "datetimeoffset", qday.datetimeoffset)
    vctrs::s3_register("lubridate::yday", "datetimeoffset", yday.datetimeoffset)
    vctrs::s3_register("parttime::vec_cast.partial_time", "datetimeoffset", vec_cast.partial_time.datetimeoffset)
    vctrs::s3_register("stats::update", "datetimeoffset", update.datetimeoffset)

    if (requireNamespace("lubridate", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("format_ISO8601", lubridate::format_ISO8601)
        methods::setMethod("format_ISO8601", signature = "datetimeoffset", format_ISO8601.datetimeoffset)
        methods::setGeneric("year<-", lubridate::"year<-")
        methods::setMethod("year<-", "datetimeoffset", function(x, value) set_year.datetimeoffset(x, value))
        methods::setGeneric("month<-", lubridate::"month<-")
        methods::setMethod("month<-", "datetimeoffset", function(x, value) set_month.datetimeoffset(x, value))
        methods::setGeneric("day<-", lubridate::"day<-")
        methods::setMethod("day<-", "datetimeoffset", function(x, value) set_day.datetimeoffset(x, value))
        methods::setGeneric("hour<-", lubridate::"hour<-")
        methods::setMethod("hour<-", "datetimeoffset", function(x, value) set_hour.datetimeoffset(x, value))
        methods::setGeneric("minute<-", lubridate::"minute<-")
        methods::setMethod("minute<-", "datetimeoffset", function(x, value) set_minute.datetimeoffset(x, value))
        methods::setGeneric("second<-", lubridate::"second<-")
        methods::setMethod("second<-", "datetimeoffset", function(x, value) set_second.datetimeoffset(x, value))
        methods::setGeneric("date<-", lubridate::"date<-")
        methods::setMethod("date<-", "datetimeoffset", `date<-.datetimeoffset`)
        methods::setGeneric("qday<-", lubridate::"qday<-")
        methods::setMethod("qday<-", "datetimeoffset", `qday<-.datetimeoffset`)
    }

    if (requireNamespace("nanotime", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("as.nanotime", nanotime::as.nanotime)
        methods::setMethod("as.nanotime", methods::signature(from="datetimeoffset"), as.nanotime.datetimeoffset)
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
