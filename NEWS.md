datetimeoffset 0.1.0 (development)
==================================

Initial features
----------------

* `datetime_offset()`
* `as_datetime_offset()`

  + All pdfmark datetime strings
  + Decent subset of ISO 8601 datetime strings
  + `Date()` objects

* `format.datetime_offset()`
* Several `{lubridate}` accessor functions

  + `year()` and `year()<-`
  + `month()` and `month()<-`
  + `day()` and `day()<-`
  + `hour()` and `hour()<-`
  + `minute()` and `minute()<-`
  + `second()` and `second()<-`
  + `tz()`, `tz()<-`, and `force_tz()`

* Some additional accessor functions

  + `hour_offset()` and `hour_offset()<-`
  + `minute_offset()` and `minute_offset()<-`
