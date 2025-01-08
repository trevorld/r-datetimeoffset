# r-datetimeoffset <img src="man/figures/logo.png" align="right" width="200px" alt="datetimeoffset hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/datetimeoffset)](https://cran.r-project.org/package=datetimeoffset)
[![R-CMD-check](https://github.com/trevorld/r-datetimeoffset/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/r-datetimeoffset/actions)
[![codecov](https://codecov.io/github/trevorld/r-datetimeoffset/branch/main/graph/badge.svg)](https://app.codecov.io/github/trevorld/r-datetimeoffset)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Importing/exporting datetime string formats](#formats)
  + [Heteregeneous time zones](#heteregeneous)
  + [Augmenting pdf datetime metadata](#pdf)

* [Features](#features)
* [Comparison with {clock}](#clock)

  + [Things {clock} can do that {datetimeoffset} can't do](#clock-advantages)
  + [Things {datetimeoffset} can do that {clock} can't do](#datetimeoffset-advantages)

* [Comparison with {parttime}](#parttime)
* [Serializing](#serializing)
* [External links](#links)

  + [Datetime standards with UTC offsets](#standards)
  + [Related Software](#similar)

## <a name="overview">Overview</a>

`{datetimeoffset}` provides support for datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones.  Strengths compared to other R datetime objects:

1. Import/export for a number of datetime string standards often including lossless re-export of any original reduced precision
2. Datetimes can be augmented with optional UTC offsets and/or (possibly heteregeneous) time zones.
3. Can support up to nanosecond precision.

The motivating use case for this package was the need for a datetime aware class that can losslessy import/export
pdf metadata datetimes for [{xmpdf}](https://github.com/trevorld/r-xmpdf).  pdf metadata datetimes
are local times with a wide range of legal precisions but with unknown time zones 
but a possibly known UTC offset.  Generally pre-existing R datetime classes either assume
knowledge of a (usually single) time zone or alternatively assumed it was acceptable to fully
convert to UTC time.

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
library("datetimeoffset")
```

#### ISO 8601 datetimes


```r
as_datetimeoffset("2020-05") |> format_iso8601()
```

```
## [1] "2020-05"
```

```r
as_datetimeoffset("2020-05-10T20:10") |> format_iso8601()
```

```
## [1] "2020-05-10T20:10"
```

```r
as_datetimeoffset("2020-05-10T20:10:15.003-07") |> format_iso8601()
```

```
## [1] "2020-05-10T20:10:15.003-07"
```

```r
as_datetimeoffset("2020-05-10 20:10:15Z") |> format_iso8601()
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
as_datetimeoffset("D:20200510201015+00'00'") |> format_pdfmark()
```

```
## [1] "D:20200510201015+00'00'"
```

#### RFC 3339 with de facto time zone extension datetimes


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
    format_nanotime("%F")
```

```
## [1] "2020-05-10"
```

```r
# SQL Server Smalldatetime / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15") |>
    format_nanotime("%F %T")
```

```
## [1] "2020-05-10 20:10:15"
```

```r
# SQL Server Datetime / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15.123") |>
    format_nanotime("%F %H:%M:%E3S")
```

```
## [1] "2020-05-10 20:10:15.123"
```

```r
# SQL Server Datetime2 / ODBC SQL_TYPE_TIMESTAMP / SQL_TIMESTAMP
as_datetimeoffset("2020-05-10 20:10:15.1234567") |>
    format_nanotime("%F %H:%M:%E7S")
```

```
## [1] "2020-05-10 20:10:15.1234567"
```

```r
# SQL Server DatetimeOFFSET / ODBC SQL_SS_TIMESTAMPOFFSET
as_datetimeoffset("2020-05-10 20:10:15.1234567 -07:00") |>
    format_nanotime("%F %H:%M:%E7S %Ez")
```

```
## [1] "2020-05-10 20:10:15.1234567 -07:00"
```

#### Extended Date Time Format (EDTF)


```r
as_datetimeoffset("2020-10-05T10:10:10") |> format_edtf()
```

```
## [1] "2020-10-05T10:10:10"
```

```r
as_datetimeoffset("2020-XX-05") |> format_edtf()
```

```
## [1] "2020-XX-05"
```

```r
# Lossy EDTF import situations
as_datetimeoffset("20XX-10-10") |> format_edtf()
```

```
## [1] "XXXX-10-10"
```

```r
as_datetimeoffset("2020-10-XX") == as_datetimeoffset("2020-10")
```

```
## [1] TRUE
```

```r
# Extensions to EDTF format
as_datetimeoffset("2020-XX-19T10:XX:10") |>
    format_edtf(precision = "nanosecond", usetz = TRUE)
```

```
## [1] "2020-XX-19T10:XX:10.XXXXXXXXX+XX:XX[X]"
```

#### Miscellaneous datetimes


```r
as_datetimeoffset("1918/11/11 11:11") |>
    format_strftime(usetz = TRUE)
```

```
## [1] "1918-11-11 11:11:00 PST"
```

### <a name="heteregeneous">Heterogeneous time zones</a>

`datetimeoffset()` objects support heteregenous time zones:


```r
# Current time in a number of time zones
datetimeoffset_now(c("America/Los_Angeles", "America/New_York",
                     "Europe/London", "Asia/Shanghai"))
```

```
## <datetimeoffset[4]>
## [1] 2022-12-21T18:42:38.737931859-08:00[America/Los_Angeles]
## [2] 2022-12-21T21:42:38.737931859-05:00[America/New_York]   
## [3] 2022-12-22T02:42:38.737931859+00:00[Europe/London]      
## [4] 2022-12-22T10:42:38.737931859+08:00[Asia/Shanghai]
```

### <a name="pdf">Augmenting pdf datetime metadata</a>

By default `grDevices::pdf()` stores the local datetime without any UTC offset information:


```r
library("grid")
library("xmpdf") # remotes::install_github("trevorld/r-xmpdf")

creation_date <- datetimeoffset_now()
print(creation_date)
```

```
## <datetimeoffset[1]>
## [1] 2022-12-21T18:42:38.886338373-08:00[America/Los_Angeles]
```

```r
# Create a two page pdf using `pdf()`
f <- tempfile(fileext = ".pdf")
pdf(f, onefile = TRUE)
grid.text("Page 1")
grid.newpage()
grid.text("Page 2")
Sys.sleep(5L) # sleep to confirm time matches start of `pdf()` call
invisible(dev.off())

di <- xmpdf::get_docinfo(f)[[1]]
print(di)
```

```
## Author: NULL
## CreationDate: 2022-12-21T18:42:38
## Creator: R
## Producer: R 4.2.2
## Title: R Graphics Output
## Subject: NULL
## Keywords: NULL
## ModDate: 2022-12-21T18:42:38
```

We can use `{datetimeoffset}` with `{xmpdf}` to augment the embedded datetime metadata to also include the UTC offset information:


```r
di$creation_date <- di$creation_date |>
    set_hour_offset(get_hour_offset(creation_date)) |>
    set_minute_offset(get_minute_offset(creation_date))
di$mod_date <- datetimeoffset_now() # Last modified metadata now
di$subject <- "Augmenting pdf metadata with UTC offsets"

xmpdf::set_docinfo(di, f)
di <- xmpdf::get_docinfo(f)[[1]]
print(di)
```

```
## Author: NULL
## CreationDate: 2022-12-21T18:42:38-08:00
## Creator: R
## Producer: GPL Ghostscript 9.55.0
## Title: R Graphics Output
## Subject: Augmenting pdf metadata with UTC offsets
## Keywords: NULL
## ModDate: 2022-12-21T18:42:44-08:00
```

## <a name="features">Features</a>

* `datetimeoffset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones

    - Suitable for use as a column in data frames and tibbles
    - Separate `{vctrs}` accessible record "fields" for year, month, day, hour, 
      minute, second, nanosecond, subsecond\_digits, hour\_offset, minute\_offset, and time zone 
      all of which can all be accessed by `{clock}` (style) 
      accessor functions (and the `{vctrs}` accessor functions).  
    - Non-missing time zones need not all be the same value 

  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetimeoffset()` converts from standard datetime strings and other R datetime objects:

  + All [pdfmark datetime](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
  + Decent subset of [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) datetime strings

    - Also supports the de facto RFC 3339 extension of specifying a named time zone at the end surrounded in brackets.

  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + The datetime strings output by the command-line tool `exiftool`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime::nanotime()` objects
  + `parttime::parttime()` objects
  + five `{clock}` calendars and three `{clock}` times
  + Any other R datetime objects with an `as.POSIXct()` method

* Support for formatting output datetime strings:

  + `format()` returns RFC 3339 with de facto time zone extension strings
  + `format_edtf()` returns [Extended Date Time Format (EDTF)](https://www.loc.gov/standards/datetime/) strings

    - Supports unofficial extensions of "Unspecified Digit" feature to time components and time zones
    - `format_edtf(x, precision = "nanosecond", usetz = TRUE)` prints out all information

  + `format_exiftool()` formats datetime strings as expected by the command-line tool `exiftool`
  + `format_iso8601()` and `lubridate::format_ISO8601()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
  + `format_pdfmark()` returns [pdfmark datetime](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
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
    As an alternative can also use `lubridate::with_tz()`.
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

## <a name="clock">Comparison with {clock}</a>

**Note**: Please feel free to [open a pull request to fix any {clock} mis-understandings or statements that are now out-of-date](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

`{datetimeoffset}` is most similar to the excellent [{clock}](https://clock.r-lib.org/index.html)
(which `{datetimeoffset}` uses internally):

* Both use [{vctrs}](https://vctrs.r-lib.org/index.html) "record" objects
* Both support variable precision datetimes
* Both support up to nanosecond precision
* Both have support for local times (with perhaps unknown time zone or UTC offset), UTC times, and times with time zones

### <a name="clock-advantages">Things {clock} can do that {datetimeoffset} can't do</a>

* `{datetimeoffset}` only supports what `{clock}` considers "year-month-day" "calendars".  `{clock}` supports a wider variety of "calendars":

  + `iso_year_week_day()`
  + `year_day()`
  + `year_month_day()`
  + `year_month_weekday()`
  + `year_quarter_day()`

* `{clock}` has a large, verbose, and explicit API that will force you to explicitly cast your datetimes into unambiguous formats to ensure correctness with respect to invalid dates and daylight saving time issues:
 
  + Datetimes must either be "naive" datetime (roughly local time without UTC offsets or time zones), "sys" datetime (UTC time), and "zoned" datetime (roughly local time with time zone)
  + `{clock}` will often make you explicitly make casting decisions if necessary to avoid any possibly ambiguous datetimes or else throw an error
  + More explicit control over the expected format of input strings

* `{clock}` is a lower-level library with lots of C++ code.  Will likely process large amounts of data faster with a lower memory overhead.

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

* `{datetimeoffset}` can import `POSIXt` objects at a microsecond precision instead of a second precision:

  
  ```r
  dts <- as.POSIXct(c("2019-01-01 01:00:00.1", 
                      "2019-01-01 01:00:00.123456",
                      "2019-01-01 01:00:00.3"),
                    tz = "America/New_York")
  as_datetimeoffset(dts)
  ```
  
  ```
  ## <datetimeoffset[3]>
  ## [1] 2019-01-01T01:00:00.100000-05:00[America/New_York]
  ## [2] 2019-01-01T01:00:00.123456-05:00[America/New_York]
  ## [3] 2019-01-01T01:00:00.300000-05:00[America/New_York]
  ```
  
  ```r
  clock::as_zoned_time(dts)
  ```
  
  ```
  ## <zoned_time<second><America/New_York>[3]>
  ## [1] "2019-01-01T01:00:00-05:00" "2019-01-01T01:00:00-05:00"
  ## [3] "2019-01-01T01:00:00-05:00"
  ```

* `{clock}` only distinguishes between "millisecond", "microsecond", and "nanosecond" sub-second precisions while `{datetimeoffset}` distinguishes all sub-second precisions up to "nanosecond".  For example a "SQL Server Datetime2" datetime supports exactly seven digits of subsecond precision (uses a hundred nanoseconds unit):

  
  ```r
  dt <- as_datetimeoffset("2020-05-10 20:10:15.1234567")
  print(dt)
  ```
  
  ```
  ## <datetimeoffset[1]>
  ## [1] 2020-05-10T20:10:15.1234567
  ```
  
  ```r
  datetime_precision(dt)
  ```
  
  ```
  ## [1] "hundred nanoseconds"
  ```
  
  ```r
  nt <- clock::naive_time_parse("2020-05-10 20:10:15.1234567", 
                                format = "%F %T", precision = "nanosecond")
  print(nt)
  ```
  
  ```
  ## <clock_naive_time[1]>
  ## [1] "2020-05-10T20:10:15.123456700"
  ```
  
  ```r
  datetime_precision(nt)
  ```
  
  ```
  ## [1] "nanosecond"
  ```

* `{datetimeoffset}` vectors allow lower precision elements to be missing:

  
  ```r
  datetimeoffset(2020, NA_integer_, 10) |> format_edtf()
  ```
  
  ```
  ## [1] "2020-XX-10"
  ```
  
  ```r
  clock::year_month_day(2020, NA_integer_, 10)
  ```
  
  ```
  ## <year_month_day<day>[1]>
  ## [1] NA
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

* `{datetimeoffset}` can import/export [leap seconds](https://en.wikipedia.org/wiki/Leap_second):

  
  ```r
  dt <- as_datetimeoffset("2005-12-31T23:59:60Z")
  format(dt)
  ```
  
  ```
  ## [1] "2005-12-31T23:59:60Z"
  ```
  
  ```r
  as.POSIXlt(dt) |> format()
  ```
  
  ```
  ## [1] "2005-12-31 23:59:60"
  ```
  
  ```r
  clock::sys_time_parse("2005-12-31T23:59:60")
  ```
  
  ```
  ## Warning: Failed to parse 1 string at location 1. Returning `NA` at that
  ## location.
  ```
  
  ```
  ## <clock_sys_time[1]>
  ## [1] NA
  ```
  
  ```r
  clock::year_month_day(2005, 12, 31, 23, 59, 60)
  ```
  
  ```
  ## Error:
  ## ! `second` must be within the range of [0, 59], not 60.
  ```

## <a name="parttime">Comparison with {parttime}</a>

**Note**: Please feel free to [open a pull request to fix any {parttime} mis-understandings or statements that are now out-of-date](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

A `{datetimeoffset}` is also similar to the excellent [{parttime}](https://dgkf.github.io/parttime/):

* Both are `{vctrs}` datetime objects that allow mixed precision datetimes including support for UTC offsets
* `{parttime}` supports more advanced [mixed precision comparisons](https://dgkf.github.io/parttime/#partial-datetime-comparisons)
* `{parttime}` uses 64-bit floating point numbers instead of 32-bit integers to store various fields so
  can theoretically support years greater than 2,147,483,647 as well as subseconds at greater than 
  nanosecond precision
  (although there are known issues with using floating point numbers such as [representation error](https://docs.python.org/3/tutorial/floatingpoint.html#representation-error) avoided by representing subseconds as an integer)
* `{datetimeoffset}` uses more fields to store UTC offsets and fractional seconds
  so there are cases where
  `{datetimeoffset}` will be more lossless importing/exporting certain datetime strings compared to `{parttime}`:

  
  ```r
  as_datetimeoffset("2020-01-02T03:04:05.10000+05")
  ```
  
  ```
  ## <datetimeoffset[1]>
  ## [1] 2020-01-02T03:04:05.10000+05
  ```
  
  ```r
  parttime::as.parttime("2020-01-02T03:04:05.10000+05")
  ```
  
  ```
  ## Initializing default timezone offset, which is assumed when timezone
  ## parts are missing.
  ## 
  ##     options("parttime.assume_tz_offset" = 0L)
  ```
  
  ```
  ## <partial_time<YMDhms+tz>[1]> 
  ## [1] "2020-01-02 03:04:05.100+05:00"
  ```

## <a name="serializing">Serializing</a>


```r
dts <- datetimeoffset(year = c(2020, 1980), month = c(NA, 10), day = c(15, NA))
format_edtf(dts)
```

```
## [1] "2020-XX-15" "1980-10"
```

```r
# serialize via data frame
df <- vctrs::vec_data(dts)
print(df)
```

```
##   year month day hour minute second nanosecond subsecond_digits hour_offset
## 1 2020    NA  15   NA     NA     NA         NA               NA          NA
## 2 1980    10  NA   NA     NA     NA         NA               NA          NA
##   minute_offset   tz
## 1            NA <NA>
## 2            NA <NA>
```

```r
dts_df <- do.call(datetimeoffset, as.list(df))
all.equal(dts, dts_df)
```

```
## [1] TRUE
```

```r
# serialize via base::serialize() or base::saveRDS()
x <- serialize(dts, NULL) # raw binary vector
dts_x <- unserialize(x)
all.equal(dts, dts_x)
```

```
## [1] TRUE
```

## <a name="links">External links</a>

Please feel free to [open a pull request to add any missing relevant links](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

### <a name="standards">Datetime standards with UTC offsets</a>

* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations)

  + [TOML datetimes](https://toml.io/en/v1.0.0#offset-date-time)
  + [XMP datetimes](https://github.com/adobe/xmp-docs/blob/master/XMPNamespaces/XMPDataTypes/CoreProperties.md#date)

* [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo)
* RFC 3339 with de facto time zone extension
* [SQL Server / ODBC datetime literals](https://learn.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements?source=recommendations&view=sql-server-ver16)
* [Extended Date Time Format (EDTF)](https://www.loc.gov/standards/datetime/)

### <a name="similar">Related software</a>

#### R packages

* [clock](https://clock.r-lib.org/index.html)
* [lubridate](https://lubridate.tidyverse.org/index.html)
* [nanotime](https://eddelbuettel.github.io/nanotime/)
* [parttime](https://github.com/dgkf/parttime/)
* [timechange](https://github.com/vspinu/timechange/)
* [vctrs](https://vctrs.r-lib.org/index.html)
