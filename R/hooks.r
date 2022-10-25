.onLoad <- function(libname, pkgname) {

    if (requireNamespace("lubridate", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("format_ISO8601", lubridate::format_ISO8601)
        methods::setMethod("format_ISO8601", signature = "datetimeoffset",
                           function(x, usetz = TRUE, precision = NULL, ...) {
                               format_iso8601(x, usetz = usetz, precision = precision)
                           })
    }

    if (requireNamespace("nanotime", quietly = TRUE)) {
        methods::setOldClass("datetimeoffset")
        methods::setGeneric("as.nanotime", nanotime::as.nanotime)
        methods::setMethod("as.nanotime", methods::signature(from="datetimeoffset"),
            function(from, ..., year = 0L, month = 1L, day = 1L,
                     hour = 0L, minute = 0L, second = 0L, nanosecond = 0L, tz = "") {
                x <- calendar_widen(from, "nanosecond",
                                    year = year, month = month, day = day,
                                    hour = hour, minute = minute, second = second, nanosecond = nanosecond)
                x <- update_missing_zone(x, tz = tz)
                nanotime::as.nanotime(format_iso8601(x))
        })
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
