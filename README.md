# r-datetimeoffset

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/datetimeoffset)](https://cran.r-project.org/package=datetimeoffset)
[![R-CMD-check](https://github.com/trevorld/r-datetimeoffset/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-datetimeoffset/actions)
[![codecov](https://codecov.io/github/trevorld/r-datetimeoffset/branch/main/graph/badge.svg)](https://codecov.io/github/trevorld/r-datetimeoffset)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

Datetimes with UTC offsets

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Importing/exporting datetime formats](#formats)

* [Features](#features)
* [Datetime standards with UTC offsets](#standards)
* [Related Software](#similar)

## <a name="overview">Overview</a>

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/r-datetimeoffset")
```

## <a name="examples">Examples</a>

### <a name="formats">Importing/exporting datetime formats</a>

`{datetimeoffset}` can import/export a number of datetime formats including
all pdfmark datetime strings and a decent subset of ISO 8601 strings.


```r
library("datetimeoffset", warn.conflicts = FALSE) # masks `date()`
# `{lubridate}` masks from `{base}` `date`, `intersect`, `setdiff`, `union`
library("lubridate", exclude = c("date", "force_tz", "tz<-"), warn.conflicts = FALSE)

# import/export for several datetime formats with possible UTC offsets
as_datetime_offset("2020-05") |> format()
```

```
## [1] "2020-05"
```

```r
as_datetime_offset("2020-05-10 20:10") |> format()
```

```
## [1] "2020-05-10T20:10"
```

```r
as_datetime_offset("2020-05-10 20:10:15-07") |> format()
```

```
## [1] "2020-05-10T20:10:15-07"
```

```r
as_datetime_offset("2020-05-10 20:10:15Z") |> format()
```

```
## [1] "2020-05-10T20:10:15Z"
```

```r
as_datetime_offset("2020-05") |> format_pdfmark()
```

```
## [1] "D:202005"
```

```r
as_datetime_offset("2020-05-10 20:10:15Z") |> format_pdfmark()
```

```
## [1] "D:20200510201015+0000"
```

```r
as_datetime_offset("D:20200510201015+0000") |> format()
```

```
## [1] "2020-05-10T20:10:15+00:00"
```

## <a name="features">Features</a>

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

## <a name="standards">Datetime standards with UTC offsets</a>

* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Combined_date_and_time_representations)
* [pdfmark datetimes](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo)
* [SQL Server datetimeoffset](https://learn.microsoft.com/en-us/sql/t-sql/data-types/datetimeoffset-transact-sql?view=sql-server-ver16)

## <a name="similar">Related software</a>

Please feel free to [open a pull request to add any missing relevant R packages](https://github.com/trevorld/r-datetimeoffset/edit/main/README.Rmd).

* [lubridate](https://lubridate.tidyverse.org/index.html)
* [nanotime](https://eddelbuettel.github.io/nanotime)
* [timechange](https://github.com/vspinu/timechange/)
