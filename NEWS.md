datetimeoffset 0.1.1
====================

Initial features
----------------

* `datetimeoffset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones

    - Suitable for use as a column in data frames and tibbles
    - Separate `{vctrs}` accessible record "fields" for year, month, day, hour, 
      minute, second, nanosecond, hour\_offset, minute\_offset, and time zone all of which 
      can be missing except year and can all be accessed by `{clock}` (style) 
      accessor functions.  
      If suggested `{lubridate}` is installed can also use `{lubridate}` accessors/extractors.
    - Non-missing time zones need not all be the same value 

  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetimeoffset()` converts from standard datetime strings and from other R datetime objects:

  + All pdfmark datetime strings
  + Decent subset of ISO 8601 datetime strings

    - Also supports the extension of [specifying a named time zone at the end surrounded in brackets.](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/#cypher-temporal-specify-time-zone)

  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime::nanotime()` objects
  + `parttime::parttime()` objects
  + five `{clock}` calendars and three `{clock}` times
  + Any other datetime objects with an `as.POSIXct()` method

* Support for formatting output strings:

  + `format()` returns [RFC 3339 with de facto time zone extension](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/) strings
  + `format_edtf()` returns [Extended Date Time Format (EDTF)](https://www.loc.gov/standards/datetime/) strings

    - Supports unofficial extensions of "Unspecified Digit" feature to time components and time zones
    - `format_edtf(x, precision = "nanosecond", usetz = TRUE)` prints out **all** information

  + `format_iso8601()` and `lubridate::format_ISO8601()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
  + `format_pdfmark()` returns [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
  + `format_nanotime()` allows [CCTZ style formatting](https://github.com/google/cctz/blob/6e09ceb/include/time_zone.h#L197)

    - Can output [SQL Server / ODBC datetime literals](https://learn.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements?source=recommendations&view=sql-server-ver16)

  + `format_strftime()` allows `base::strftime()` style formatting 

* Support for converting to other R datetime objects:

  + `as.Date()` and `as_date()` converts the local date to a `base::Date()` object
  + `as.POSIXct()` and `as_date_time()` converts the datetime to a `base::POSIXct()` object
  + `as.POSIXlt()` converts the datetime to a `base::POSIXlt()` object
  + `as.nanotime()` converts the datetime to a `nanotime::nanotime()` object
  + `as.parttime()` converts the datetime to a `parttime::parttime()` object
  + `{clock}` calendars, times, and weekdays:

    - `as_iso_year_week_day()`, `as_year_day()`, `as_year_month_day()`, `as_year_month_weekday()`, `as_year_quarter_day()`
    - `as_naive_time()`, `as_sys_time()`, `as_zoned_time()`
    - `as_weekday()`

* Support for several accessor S3 methods from `{clock}`

  + `get_year()` and `set_year()`
  + `get_month()` and `set_month()`
  + `get_day()` and `set_day()`
  + `get_hour()` and `set_hour()`
  + `get_minute()` and `set_minute()`
  + `get_second()` and `set_second()`
  + `get_nanosecond()` and `set_nanosecond()`

* Support for several accessor methods from `{lubridate}`

  + `year()` and `year()<-`
  + `month()` and `month()<-`
  + `day()` and `day()<-`
  + `hour()` and `hour()<-`
  + `minute()` and `minute()<-`
  + `second()` and `second()<-`
  + `tz()` and `tz()<-`

* New accessor S3 methods:

  + `get_subsecond_digits()` and `set_subsecond_digits()`
  + `get_hour_offset()` and `set_hour_offset()`
  + `get_minute_offset()` and `set_minute_offset()`
  + `get_tz()` and `set_tz()` (changes system time, not clock time)

* Get/set datetime "precision" S3 methods

  + `datetime_cast()`
  + `datetime_narrow()`
  + `datetime_precision()`
  + `datetime_widen()`

* Additional subsecond accessors

  + `get_millisecond()` and `set_millisecond()`
  + `get_microsecond()` and `set_microsecond()`
  + `get_subsecond()` and `set_subsecond()`

* Other utilities:

  + `datetimeoffset_now()` returns the current time in the corresponding time zone(s).
  + `is_datetimeoffset()` and `NA_datetimeoffset_`
  + `fill_tz()` and `fill_utc_offsets()` fill in missing time zones and missing UTC offsets respectively.
  + `mode_tz()` is an S3 method that gets most common time zone for a datetime object
  + `datetime_at_tz()` can be used to change the timezone 
    (changes clock time, not system time).
  + `get_utc_offsets()` and `set_utc_offsets()` gets/sets UTC offset strings
  + Support for `{clock}` invalid datetime methods `invalid_detect()`, `invalid_any()`,
    `invalid_count()`, `invalid_remove()`, and `invalid_resolve()`.
  + `precision_to_int()` converts datetime precisions to an integer
  + Support for `{base}` datetime extractors `weekdays()`, `months()`, `quarters()`, and `julian()`
  + Support for `{lubridate}` datetime extractors `date()`, `date()<-`, `isoyear()`, `epiyear()`, 
    `quarter()`, `semester()`, `week()`, `isoweek()`, `epiweek()`,
    `qday()`, `qday<-()`, `wday()`, `wday<-()`, `yday()`, `yday<-()`,
    `am()`, `pm()`, `days_in_month()`, `dst()`, and `leap_year()`.
  + Support for `{lubridate}` `force_tz()` and `with_tz()`.
