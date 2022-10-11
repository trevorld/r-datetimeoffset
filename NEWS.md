datetimeoffset 0.1.0 (development)
==================================

Initial features
----------------

* `datetime_offset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with optional UTC offsets and/or (possibly heteregenous) time zones

    - Suitable for use as a column in data frames and tibbles
    - Separate `{vctrs}` accessible record "fields" for year, month, day, hour, 
      minute, second, nanosecond, hour\_offset, minute\_offset, and time zone all of which 
      can be missing except year and can all be accessed by `{lubridate}` (style) 
      accessor functions.  
    - Non-missing time zones need not all be the same value 

  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetime_offset()` converts from standard datetime strings and from other R datetime objects:

  + All pdfmark datetime strings
  + Decent subset of ISO 8601 datetime strings
  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime()` objects
  + Any other datetime objects with an `as.POSIXct()` method

* Support for formatting output strings:

    + `format.datetime_offset()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
    + `format_ISO8601().datetime_offset()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
    + `format_pdfmark()` returns [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings

      - `format_pdfmark.default()` anything convertible to `datetime_offset()`
      - `format_pdfmark.datetime_offset()`

* Support for converting to other R datetime objects:

  + `as.Date()` converts the local date to a `base::Date()` object
  + `as.POSIXct()` converts the datetime to a `base::POSIXct()` object
  + `as.POSIXlt()` converts the datetime to a `base::POSIXlt()` object
  + `as.nanotime()` converts the datetime to a `nanotime::nanotime()` object

* Support for several `{lubridate}` accessor functions

  + `date()` and `date()<-`

    - To avoid `R CMD check` WARNING we re-export `lubridate::date()`

  + `year()` and `year()<-`
  + `month()` and `month()<-`
  + `day()` and `day()<-`
  + `hour()` and `hour()<-`
  + `minute()` and `minute()<-`
  + `second()` and `second()<-`
  + `tz()` and `tz()<-`

    - We export a `force_tz()` S3 generic which defaults to `lubridate::force_tz()`
      but provides a special method for `datetime_offset()` objects
    - We export a `tz()<-` which uses the new generic `force_tz()`
      instead of always using `lubridate::force_tz()`
    - We export a `with_tz()` S3 generic which defaults to `lubridate::with_tz()`
      but provides a special method for `datetime_offset()` objects

* Some additional accessor functions

  + `nanosecond()` and `nanosecond()<-`
  + `hour_offset()` and `hour_offset()<-`
  + `minute_offset()` and `minute_offset()<-`

* Other utility functions:

  + `mode_tz()` gets most common time zone for a time date object
    that may support heteregeneous time zones.
