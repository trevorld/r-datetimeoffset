datetimeoffset 0.1.0 (development)
==================================

Initial features
----------------

* `datetime_offset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with possible UTC offsets
  + Suitable for use as a column in data frames and tibbles
  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetime_offset()`

  + All pdfmark datetime strings
  + Decent subset of ISO 8601 datetime strings
  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects

* Support for formatting output strings:

    + `format.datetime_offset()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
    + `format_pdfmark()` returns [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings

      - `format_pdfmark.default()` anything convertible to `datetime_offset()`
      - `format_pdfmark.datetime_offset()`

* Support for several `{lubridate}` accessor functions

  + `date()` and `date()<-`

    - To avoid `R CMD check` WARNING we export a `date()` that is same as `lubridate::date()`

  + `year()` and `year()<-`
  + `month()` and `month()<-`
  + `day()` and `day()<-`
  + `hour()` and `hour()<-`
  + `minute()` and `minute()<-`
  + `second()` and `second()<-`
  + `tz()`, `tz()<-`, and `force_tz()`

    - We export a `force_tz()` S3 generic which defaults to `lubridate::force_tz()`
      but provides a special method for `datetime_offset()` objects
    - We export a `tz()<-` which uses the new generic `force_tz()`
      instead of `lubridate::force_tz()`

* Some additional accessor functions

  + `hour_offset()` and `hour_offset()<-`
  + `minute_offset()` and `minute_offset()<-`
