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

* [Features](#features)
* [External links](#links)

  + [Datetime standards with UTC offsets](#standards)
  + [Related Software](#similar)

## <a name="overview">Overview</a>

`{datetimeoffset}` provides support for datetimes with optional UTC offsets and/or (possibly heteregenous) time zones.  Strengths compared to other R datetime objects:

1. Import/export for a number of datetime string standards including lossless re-export of any original reduced precision
2. Datetimes can be augmented with optional UTC offsets and/or (possibly heteregenous) time zones.
3. Can support up to nanosecond precision.

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/r-datetimeoffset")
```

## <a name="examples">Examples</a>

### <a name="formats">Importing/exporting datetime string formats</a>

`{datetimeoffset}` can import/export a number of datetime formats including
all pdfmark datetime strings and a decent subset of ISO 8601 strings.
Supports lossless re-export of any original reduced precision.


```r
library("datetimeoffset", warn.conflicts = FALSE) # masks `date()`
# `{lubridate}` masks from `{base}` `date`, `intersect`, `setdiff`, `union`
library("lubridate", exclude = c("date", "force_tz", "tz<-", "with_tz"),
        warn.conflicts = FALSE)
```

#### ISO 8601 datetimes


```r
as_datetime_offset("2020-05") |> format_ISO8601()
```

```
## [1] "2020-05"
```

```r
as_datetime_offset("2020-05-10T20:10") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10"
```

```r
as_datetime_offset("2020-05-10T20:10:15.003-07") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10:15.003-07"
```

```r
as_datetime_offset("2020-05-10 20:10:15Z") |> format_ISO8601()
```

```
## [1] "2020-05-10T20:10:15Z"
```

#### pdfmark datetimes


```r
as_datetime_offset("D:202005") |> format_pdfmark()
```

```
## [1] "D:202005"
```

```r
as_datetime_offset("D:20200510201015+0000") |> format_pdfmark()
```

```
## [1] "D:20200510201015+0000"
```

#### Miscellaneous datetimes


```r
as_datetime_offset("1918/11/11 11:11") |> format()
```

```
## [1] "1918-11-11T11:11"
```

```r
if ("Europe/Paris" %in% OlsonNames())
    as_datetime_offset("1918/11/11 11:11", tz = "Europe/Paris") |> format()
```

```
## [1] "1918-11-11T11:11+00:00"
```

```r
as_datetime_offset("1918/11/11 11:11:11.11") |> format()
```

```
## [1] "1918-11-11T11:11:11.11"
```

## <a name="features">Features</a>

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

* `as_datetime_offset()` converts from standard datetime strings and other R datetime objects:

  + All pdfmark datetime strings
  + Decent subset of ISO 8601 datetime strings
  + The datetime strings understood by the default `tryFormats` of `as.POSIXlt()`
  + `Date()` objects
  + `POSIXct()` objects
  + `POSIXlt()` objects
  + `nanotime()` objects
  + Any other datetime objects with an `as.POSIXct()` method

* Support for formatting output datetime strings:

    + `format.datetime_offset()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
    + `format_ISO8601().datetime_offset()` returns [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations) strings
    + `format_pdfmark()` returns [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) strings

      - `format_pdfmark.default()` anything convertible to `datetime_offset()`
      - `format_pdfmark.datetime_offset()`

    + `format_strftime()` allows `base::strftime()` style formatting 
    + `format_CCTZ()` allows [CCTZ style formatting](https://github.com/google/cctz/blob/6e09ceb/include/time_zone.h#L197)

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
  + `tz()`, `tz()<-`

    - We export a `force_tz()` S3 generic which defaults to `lubridate::force_tz()`
      but provides a special method for `datetime_offset()` objects
    - We export a `tz()<-` which uses the new generic `force_tz()`
      instead of `lubridate::force_tz()`
    - We export a `with_tz()` S3 generic which defaults to `lubridate::with_tz()`
      but provides a special method for `datetime_offset()` objects

* Some additional accessor functions

  + `nanosecond()` and `nanosecond()<-`
  + `hour_offset()` and `hour_offset()<-`
  + `minute_offset()` and `minute_offset()<-`

* Other utility functions:

  + `mode_tz()` gets most common time zone for a time date object
    that may support heteregeneous time zones.

## <a name="links">External links</a>

Please feel free to [open a pull request to add any missing relevant links](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

### <a name="standards">Datetime standards with UTC offsets</a>

* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations)
* [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo)

### <a name="similar">Related software</a>


* [lubridate](https://lubridate.tidyverse.org/index.html)
* [nanotime](https://eddelbuettel.github.io/nanotime)
* [timechange](https://github.com/vspinu/timechange/)
