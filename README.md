# r-datetimeoffset

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/datetimeoffset)](https://cran.r-project.org/package=datetimeoffset)
[![R-CMD-check](https://github.com/trevorld/r-datetimeoffset/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-datetimeoffset/actions)
[![codecov](https://codecov.io/github/trevorld/r-datetimeoffset/branch/main/graph/badge.svg)](https://codecov.io/github/trevorld/r-datetimeoffset)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Importing/exporting datetime string formats](#formats)
  + [Durations / periods / heteregeneous time zones](#heteregeneous)

* [Features](#features)
* [Comparison with {clock}](#clock)

  + [Things {clock} can do that {datetimeoffset} can't do](#clock-advantages)
  + [Things {datetimeoffset} can do that {clock} can't do](#datetimeoffset-advantages)

* [External links](#links)

  + [Datetime standards with UTC offsets](#standards)
  + [Related Software](#similar)

## <a name="overview">Overview</a>

`{datetimeoffset}` provides support for datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones.  Strengths compared to other R datetime objects:

1. Import/export for a number of datetime string standards often including lossless re-export of any original reduced precision
2. Datetimes can be augmented with optional UTC offsets and/or (possibly heteregeneous) time zones.
3. Can support up to nanosecond precision.

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/r-datetimeoffset")
```

## <a name="examples">Examples</a>

### <a name="formats">Importing/exporting datetime string formats</a>

`{datetimeoffset}` can import/export a number of datetime formats. 
Supports lossless re-export of any original reduced precision for a number of formats such as
pdfmark datetime strings and ISO 8601 datetime strings.


```r
library("datetimeoffset", warn.conflicts = FALSE) # masks `date()`
# `{lubridate}` masks from `{base}` `date`, `intersect`, `setdiff`, `union`
library("lubridate", exclude = c("date", "force_tz", "tz<-", "with_tz"),
        warn.conflicts = FALSE)
```

#### ISO 8601 datetimes


```r
as_datetimeoffset("2020-05") |> format_ISO8601()
```

```
## [1] "2020-05"
```

```r
as_datetimeoffset("2020-05-10T20:10") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10"
```

```r
as_datetimeoffset("2020-05-10T20:10:15.003-07") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10:15.003-07"
```

```r
as_datetimeoffset("2020-05-10 20:10:15Z") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10:15Z"
```

#### pdfmark datetimes


```r
as_datetimeoffset("D:202005") |> format_pdfmark()
```

```
## [1] "D:202005"
```

```r
as_datetimeoffset("D:20200510201015+0000") |> format_pdfmark()
```

```
## [1] "D:20200510201015+0000"
```

#### neo4j Cypher datetimes


```r
as_datetimeoffset("2020-05-10T20:10:15.003[America/Los_Angeles]") |>
    format()
```

```
## [1] "2020-05-10T20:10:15.003-07:00[America/Los_Angeles]"
```

```r
as_datetimeoffset("2020-05-10T20:10-07:00[America/Los_Angeles]") |>
    format()
```

```
## [1] "2020-05-10T20:10-07:00[America/Los_Angeles]"
```

#### SQL Server/ODBC datetime string literals


```r
# SQL Server Date / ODBC SQL_TYPE_DATE / SQL_DATE
as_datetimeoffset("2020-05-10") |>
    format_CCTZ("%F")
```

```
## [1] "2020-05-10"
```

```r
# SQL Server Smalldatetime / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15") |>
    format_CCTZ("%F %T")
```

```
## [1] "2020-05-10 20:10:15"
```

```r
# SQL Server Datetime / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15.123") |>
    format_CCTZ("%F %H:%M:%E3S")
```

```
## [1] "2020-05-10 20:10:15.123"
```

```r
# SQL Server Datetime2 / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15.1234567") |>
    format_CCTZ("%F %H:%M:%E7S")
```

```
## [1] "2020-05-10 20:10:15.1234567"
```

```r
# SQL Server DatetimeOFFSET / ODBC SQL_SS_TIMESTAMPOFFSET
as_datetimeoffset("2020-05-10 20:10:15.1234567 -07:00") |>
    format_CCTZ("%F %H:%M:%E7S %Ez")
```

```
## [1] "2020-05-10 20:10:15.1234567 -07:00"
```

#### Miscellaneous datetimes


```r
as_datetimeoffset("1918/11/11 11:11") |>
    format_strftime(usetz = TRUE)
```

```
## [1] "1918-11-11 11:11:00 PST"
```

### <a name="heteregeneous">Durations / periods / heterogeneous time zones</a>

`datetimeoffset()` objects support heteregenous time zones as well as addition with `{lubridate}` and `{nanotime}` duration and period objects:


```r
# DST boundary in the continental United States
boundary <- as_datetimeoffset("2009-03-08 01:59:59",
                               tz = c("America/Los_Angeles", "America/Denver",
                                      "America/Chicago", "America/New_York"))
boundary + lubridate::days(1) # period
```

```
## <datetimeoffset[4]>
## [1] 2009-03-09T01:59:59.0-07:00[America/Los_Angeles]
## [2] 2009-03-09T01:59:59.0-06:00[America/Denver]     
## [3] 2009-03-09T01:59:59.0-05:00[America/Chicago]    
## [4] 2009-03-09T01:59:59.0-04:00[America/New_York]
```

```r
boundary + lubridate::ddays(1) # duration
```

```
## <datetimeoffset[4]>
## [1] 2009-03-09T02:59:59.0-07:00[America/Los_Angeles]
## [2] 2009-03-09T02:59:59.0-06:00[America/Denver]     
## [3] 2009-03-09T02:59:59.0-05:00[America/Chicago]    
## [4] 2009-03-09T02:59:59.0-04:00[America/New_York]
```

```r
boundary + nanotime::nanoperiod(day = 1) # period
```

```
## <datetimeoffset[4]>
## [1] 2009-03-09T01:59:59.0-07:00[America/Los_Angeles]
## [2] 2009-03-09T01:59:59.0-06:00[America/Denver]     
## [3] 2009-03-09T01:59:59.0-05:00[America/Chicago]    
## [4] 2009-03-09T01:59:59.0-04:00[America/New_York]
```

```r
boundary + nanotime::nanoduration(hour = 24, minute = 0, second = 0, nanosecond = 0) # duration
```

```
## <datetimeoffset[4]>
## [1] 2009-03-09T02:59:59.0-07:00[America/Los_Angeles]
## [2] 2009-03-09T02:59:59.0-06:00[America/Denver]     
## [3] 2009-03-09T02:59:59.0-05:00[America/Chicago]    
## [4] 2009-03-09T02:59:59.0-04:00[America/New_York]
```

## <a name="features">Features</a>

* `datetimeoffset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones

    - Suitable for use as a column in data frames and tibbles
    - Separate `{vctrs}` accessible record "fields" for year, month, day, hour, 
      minute, second, nanosecond, hour\_offset, minute\_offset, and time zone all of which 
      can all be accessed by `{clock}` or `{lubridate}` (style) 
      accessor functions (and the `{vctrs}` accessor functions).  
    - Non-missing time zones need not all be the same value 

  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetimeoffset()` converts from standard datetime strings and other R datetime objects:

  + All [pdfmark datetime](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
  + Decent subset of [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) datetime strings

    - Also supports the extension of [specifying a named time zone at the end surrounded in brackets.](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/#cypher-temporal-specify-time-zone)

  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime()` objects
  + Any other R datetime objects with an `as.POSIXct()` method

* Support for formatting output datetime strings:

  + `format()` returns [neo4j Cypher temporal value](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/) strings
  + `format_ISO8601()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
  + `format_pdfmark()` returns [pdfmark datetime](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
  + `format_CCTZ()` allows [CCTZ style formatting](https://github.com/google/cctz/blob/6e09ceb/include/time_zone.h#L197)

    - Can output [SQL Server / ODBC datetime literals](https://learn.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements?source=recommendations&view=sql-server-ver16)

  + `format_strftime()` allows `base::strftime()` style formatting 

* Support for converting to other R datetime objects:

  + `as.Date()` converts the local date to a `base::Date()` object
  + `as.POSIXct()` converts the datetime to a `base::POSIXct()` object
  + `as.POSIXlt()` converts the datetime to a `base::POSIXlt()` object
  + `as.nanotime()` converts the datetime to a `nanotime::nanotime()` object

* `datetimeoffset()` objects can add/subtract `{lubridate}` and `{nanotime}` duration and period objects

  - Can also add/subtract `difftime()` durations with `vctrs::vec_arith()`

* Support for several `{lubridate}` accessor functions

  + `date()` and `date()<-`

    - To avoid an `R CMD check` WARNING we export another `date()` S3 generic

  + `year()` and `year()<-`
  + `month()` and `month()<-`
  + `day()` and `day()<-`
  + `hour()` and `hour()<-`
  + `minute()` and `minute()<-`
  + `second()` and `second()<-`
  + `tz()`, `tz()<-`

    - We export a `force_tz()` S3 generic which defaults to `lubridate::force_tz()`
      but provides a special method for `datetimeoffset()` objects
    - We export a `tz()<-` which uses the new generic `force_tz()`
      instead of `lubridate::force_tz()`
    - We export a `with_tz()` S3 generic which defaults to `lubridate::with_tz()`
      but provides a special method for `datetimeoffset()` objects

* Some additional `{lubridate}` "style" accessor functions

  + `nanosecond()` and `nanosecond()<-`
  + `hour_offset()` and `hour_offset()<-`
  + `minute_offset()` and `minute_offset()<-`

* Support for several `{clock}` accessor functions

  + `get_year()` and `set_year()`
  + `get_month()` and `set_month()`
  + `get_day()` and `set_day()`
  + `get_hour()` and `set_hour()`
  + `get_minute()` and `set_minute()`
  + `get_second()` and `set_second()`
  + `get_nanosecond()` and `set_nanosecond()`

* Some additional `{clock}` "style" accessor functions

  + `get_zone()` and `set_zone()` (changes system time, not clock time)
  + `get_hour_offset()` and `set_hour_offset()`
  + `get_minute_offset()` and `set_minute_offset()`

* Other utilities:

  + `is_datetimeoffset()` and `NA_datetimeoffset_`
  + `mode_tz()` gets most common time zone for a time date object
    that may support heteregeneous time zones.

## <a name="clock">Comparison with {clock}</a>

**Note**: Please feel free to [open a pull request to fix any {clock} mis-understandings or statements that are now out-of-date](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

`{datetimeoffset}` is most similar to the excellent [{clock}](https://clock.r-lib.org/index.html):

* Both use [{vctrs}](https://vctrs.r-lib.org/index.html) "record" objects
* Both support variable precision datetimes
* Both support up to nanosecond precision
* Both have support for local times (with perhaps unknown time zone or UTC offset), UTC times, and times with time zones

### <a name="clock-advantages">Things {clock} can do that {datetimeoffset} can't do</a>

* `{datetimeoffset}` only supports what `{clock}` considers "year-month-day" "calendars".  `{clock}` supports a wider variety of "calendars":

  + `isoyear_week_day()`
  + `year_day()`
  + `year_month_day()`
  + `year_month_weekday()`
  + `year_quarter_day()`

* `{clock}` distinguishes between "millisecond", "microsecond", and "nanosecond" sub-second precisions while `{datetimeoffset}` does not distinguish such sub-second precisions (but default output formats omit trailing zeroes)

* `{clock}` has a large, verbose, and explicit API that will force you to explicitly cast your datetimes into unambiguous formats to ensure correctness with respect to invalid dates and daylight saving time issues:
 
  + Datetimes must either be "naive" datetime (roughly local time without UTC offsets or time zones), "sys" datetime (UTC time), and "zoned" datetime (roughly local time with time zone)
  + `{clock}` will often make you explicitly make casting decisions if necessary to avoid any possibly ambiguous datetimes or else throw an error
  + More explicit control over the expected format of input strings

### <a name="datetimeoffset-advantages">Things {datetimeoffset} can do that {clock} can't do</a>

* `{datetimeoffset}` vectors can have more than one time zone within it:

  
  ```r
  dts <- c("1970-01-01T00:00:00-08:00[America/Los_Angeles]",
           "1970-01-01T00:00:00-05:00[America/New_York]")
  as_datetimeoffset(dts)
  ```
  
  ```
  ## <datetimeoffset[2]>
  ## [1] 1970-01-01T00:00:00-08:00[America/Los_Angeles]
  ## [2] 1970-01-01T00:00:00-05:00[America/New_York]
  ```
  
  ```r
  clock::zoned_time_parse_complete(dts)
  ```
  
  ```
  ## Error:
  ## ! All elements of `x` must have the same time zone name. Found different zone names of: 'America/Los_Angeles' and 'America/New_York'.
  ```

* `{datetimeoffset}` vectors allow datetimes with varying precisions:

  
  ```r
  c(datetimeoffset(2020), datetimeoffset(2020, 1, 1))
  ```
  
  ```
  ## <datetimeoffset[2]>
  ## [1] 2020       2020-01-01
  ```
  
  ```r
  c(clock::year_month_day(2020),
    clock::year_month_day(2020, 1, 1))
  ```
  
  ```
  ## Error in `vec_c()`:
  ## ! Can't combine `..1` <year_month_day<year>> and `..2` <year_month_day<day>>.
  ## Can't combine calendars with different precisions.
  ```

* `{datetimeoffset}` vectors preserves UTC offsets even when the time zone is unknown:

  
  ```r
  as_datetimeoffset("1970-01-01T00:00:00-08:00")
  ```
  
  ```
  ## <datetimeoffset[1]>
  ## [1] 1970-01-01T00:00:00-08:00
  ```
  
  ```r
  clock::sys_time_parse_RFC_3339("1970-01-01T00:00:00-08:00", offset = "%Ez")
  ```
  
  ```
  ## <clock_sys_time[1]>
  ## [1] "1970-01-01T08:00:00"
  ```

  But note `{datetimeoffset}` will convert to UTC and lose UTC offsets (if no time zone is known) 
  when a duration or period is added to them:

  
  ```r
  dt <- as_datetimeoffset(c("1970-01-01T00:00:00-08:00",
                            "1970-01-01T00:00:00-08:00[America/Los_Angeles]"))
  dt + nanotime::nanoduration(0L, 0L, 0L, 0L)
  ```
  
  ```
  ## <datetimeoffset[2]>
  ## [1] 1970-01-01T08:00:00.0Z                          
  ## [2] 1970-01-01T00:00:00.0-08:00[America/Los_Angeles]
  ```

* `{datetimeoffset}` vectors can contain a mix of local/global datetimes with various knowledge of UTC offsets and/or time zones:

  
  ```r
  as_datetimeoffset(c("1970-01-01T00:00:00",
                      "1970-01-01T00:00:00Z",
                      "1970-01-01T00:00:00-08:00",
                      "1970-01-01T00:00:00-08:00[America/Los_Angeles]",
                      "1970-01-01T00:00:00[America/Los_Angeles]"))
  ```
  
  ```
  ## <datetimeoffset[5]>
  ## [1] 1970-01-01T00:00:00                           
  ## [2] 1970-01-01T00:00:00Z                          
  ## [3] 1970-01-01T00:00:00-08:00                     
  ## [4] 1970-01-01T00:00:00-08:00[America/Los_Angeles]
  ## [5] 1970-01-01T00:00:00-08:00[America/Los_Angeles]
  ```

## <a name="links">External links</a>

Please feel free to [open a pull request to add any missing relevant links](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

### <a name="standards">Datetime standards with UTC offsets</a>

* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations)
* [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo)
* [neo4j Cypher temporal values](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/)
* [SQL Server / ODBC datetime literals](https://learn.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements?source=recommendations&view=sql-server-ver16)

### <a name="similar">Related software</a>

#### R packages

* [clock](https://clock.r-lib.org/index.html)
* [lubridate](https://lubridate.tidyverse.org/index.html)
* [nanotime](https://eddelbuettel.github.io/nanotime)
* [timechange](https://github.com/vspinu/timechange/)
* [vctrs](https://vctrs.r-lib.org/index.html)
