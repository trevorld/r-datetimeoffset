# r-datetimeoffset <img src="man/figures/logo.png" align="right" width="200px" alt="datetimeoffset hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/datetimeoffset)](https://cran.r-project.org/package=datetimeoffset)
[![R-CMD-check](https://github.com/trevorld/r-datetimeoffset/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-datetimeoffset/actions)
[![codecov](https://codecov.io/github/trevorld/r-datetimeoffset/branch/main/graph/badge.svg)](https://codecov.io/github/trevorld/r-datetimeoffset)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

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
# DST boundary in the continental United States
boundary <- as_datetimeoffset("2009-03-08 01:59:59",
                               tz = c("America/Los_Angeles", "America/Denver",
                                      "America/Chicago", "America/New_York"))
format(boundary)
```

```
## [1] "2009-03-08T01:59:59-08:00[America/Los_Angeles]"
## [2] "2009-03-08T01:59:59-07:00[America/Denver]"     
## [3] "2009-03-08T01:59:59-06:00[America/Chicago]"    
## [4] "2009-03-08T01:59:59-05:00[America/New_York]"
```

### <a name="pdf">Augmenting pdf datetime metadata</a>

By default `grDevices::pdf()` stores the local datetime without any UTC offset information:


```r
library("grid")
library("xmpdf") # remotes::install_github("trevorld/r-xmpdf")

creation_date <- Sys.time()
print(creation_date)
```

```
## [1] "2022-11-05 21:27:08 PDT"
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
## CreationDate: 2022-11-05T21:27:08
## Creator: R
## Producer: R 4.2.1
## Title: R Graphics Output
## Subject: NULL
## Keywords: NULL
## ModDate: 2022-11-05T21:27:08
```

We can use `{datetimeoffset}` with `{xmpdf}` to augment the embedded datetime metadata to also include the UTC offset information:


```r
di$creation_date <- di$creation_date |>
    set_hour_offset(get_hour_offset(creation_date)) |>
    set_minute_offset(get_minute_offset(creation_date))
di$mod_date <- Sys.time() # We've last modified pdf metadata now
di$subject <- "Augmenting pdf metadata with UTC offsets"

xmpdf::set_docinfo(di, f)
di <- xmpdf::get_docinfo(f)[[1]]
print(di)
```

```
## Author: NULL
## CreationDate: 2022-11-05T21:27:08-07:00
## Creator: R
## Producer: GPL Ghostscript 9.55.0
## Title: R Graphics Output
## Subject: Augmenting pdf metadata with UTC offsets
## Keywords: NULL
## ModDate: 2022-11-05T21:27:14-07:00
```

## <a name="features">Features</a>

* `datetimeoffset()` objects
 
  + A `{vctrs}` "record" object that supports datetimes with optional UTC offsets and/or (possibly heteregeneous) time zones

    - Suitable for use as a column in data frames and tibbles
    - Separate `{vctrs}` accessible record "fields" for year, month, day, hour, 
      minute, second, nanosecond, hour\_offset, minute\_offset, and time zone all of which 
      can all be accessed by `{clock}` (style) 
      accessor functions (and the `{vctrs}` accessor functions).  
    - Non-missing time zones need not all be the same value 

  + Supports lossless import/export of pdfmark datetime strings and a decent subset of 
    ISO 8601 datetime strings even when datetime elements are unknown

* `as_datetimeoffset()` converts from standard datetime strings and other R datetime objects:

  + All [pdfmark datetime](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings
  + Decent subset of [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) datetime strings

    - Also supports the de facto RFC 3339 extension of [specifying a named time zone at the end surrounded in brackets.](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/#cypher-temporal-specify-time-zone)

  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime()` objects
  + `{clock}` calendars and times
  + Any other R datetime objects with an `as.POSIXct()` method

* Support for formatting output datetime strings:

  + `format()` returns [RFC 3339 with de facto time zone extension](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/) strings
  + `format_edtf()` returns [Extended Date Time Format (EDTF)](https://www.loc.gov/standards/datetime/) strings

    - Supports unofficial extensions of "Unspecified Digit" feature to time components and time zones
    - `format_edtf(x, precision = "nanosecond", usetz = TRUE)` prints out all information

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
  + `{clock}` calendars and times

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
  + `date()` and `date()<-`
  + `tz()`, `tz()<-`, and `force_tz()`

* New accessor S3 methods:

  + `get_hour_offset()` and `set_hour_offset()`
  + `get_minute_offset()` and `set_minute_offset()`
  + `get_tz()` and `set_tz()` (changes system time, not clock time)

* Get/set datetime "precision" S3 methods

  + `datetime_narrow()`
  + `datetime_precision()`
  + `datetime_widen()`

* Other utilities:

  + `is_datetimeoffset()` and `NA_datetimeoffset_`
  + `mode_tz()` is an S3 method that gets most common time zone for a datetime object
  + `precision_to_int()` converts datetime precisions to an integer

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

## <a name="serializing">Serializing</a>


```r
dts <- datetimeoffset(year = c(2020, 1980), month = c(NA, 10), day = c(15, NA))
format_edtf(dts)
```

```
## [1] "2020-XX-15" "1980-10"
```

```r
# serialize via character vector
ch <- format_edtf(dts, precision = "nanosecond", usetz = TRUE)
print(ch)
```

```
## [1] "2020-XX-15TXX:XX:XX.XXXXXXXXX+XX:XX[X]"
## [2] "1980-10-XXTXX:XX:XX.XXXXXXXXX+XX:XX[X]"
```

```r
dts_ch <- as_datetimeoffset(ch)
all.equal(dts, dts_ch)
```

```
## [1] TRUE
```

```r
# serialize via data frame
df <- vctrs::vec_data(dts)
print(df)
```

```
##   year month day hour minute second nanosecond hour_offset minute_offset   tz
## 1 2020    NA  15   NA     NA     NA         NA          NA            NA <NA>
## 2 1980    10  NA   NA     NA     NA         NA          NA            NA <NA>
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
* [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo)
* [RFC 3339 with de facto time zone extension](https://neo4j.com/docs/cypher-manual/current/syntax/temporal/)
* [SQL Server / ODBC datetime literals](https://learn.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements?source=recommendations&view=sql-server-ver16)
* [Extended Date Time Format (EDTF)](https://www.loc.gov/standards/datetime/)

### <a name="similar">Related software</a>

#### R packages

* [clock](https://clock.r-lib.org/index.html)
* [lubridate](https://lubridate.tidyverse.org/index.html)
* [nanotime](https://eddelbuettel.github.io/nanotime)
* [timechange](https://github.com/vspinu/timechange/)
* [vctrs](https://vctrs.r-lib.org/index.html)
