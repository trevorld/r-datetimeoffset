test_that("format()", {
    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    dt <- c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]"))
})

test_that("format_iso8601()", {
    # "2020-05-15T08:23:16-07:00"
    expect_equal(format_iso8601(as_datetimeoffset("2020")),
                 "2020")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05")),
                 "2020-05")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15")),
                 "2020-05-15")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08")),
                 "2020-05-15T08")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08:23")),
                 "2020-05-15T08:23")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08:23:16")),
                 "2020-05-15T08:23:16")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08:23:16Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08:23:16-07")),
                 "2020-05-15T08:23:16-07")
    expect_equal(format_iso8601(as_datetimeoffset("2020-05-15T08:23:16-0700")),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetimeoffset("2020-05-15T08:23:16.0-07:00")
    expect_equal(format_iso8601(dt, precision = "year", offsets = FALSE),
                 "2020")
    expect_equal(format_iso8601(dt, precision = "month", offsets = FALSE),
                 "2020-05")
    expect_equal(format_iso8601(dt, precision = "day", offsets = FALSE),
                 "2020-05-15")
    expect_equal(format_iso8601(dt, precision = "hour"),
                 "2020-05-15T08-07:00")
    expect_equal(format_iso8601(dt, precision = "minute"),
                 "2020-05-15T08:23-07:00")
    expect_equal(format_iso8601(dt, precision = "second"),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetimeoffset("D:20200515082316-0700")
    dt <- set_second(dt, NA_integer_)
    expect_equal(format_iso8601(dt), "2020-05-15T08:23-07:00")

    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    dt <- c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]")
    expect_equal(format_iso8601(as_datetimeoffset(dt)),
                 c("2020", "2020-05-15T08:23:16.0-07:00"))
    expect_equal(format_iso8601(as_datetimeoffset(dt), precision = "second"),
                 c("2020", "2020-05-15T08:23:16-07:00"))

    skip_if_not("America/New_York" %in% OlsonNames())
    # ambiguous time so not possible to compute offset
    dt <- "2020-11-01T01:30:00[America/New_York]"
    expect_equal(format_iso8601(as_datetimeoffset(dt)),
                 "2020-11-01T01:30:00")

    # non-ambiguous times so possible to compute offset
    dt <- "2020-03-08T03:30:00[America/New_York]"
    expect_equal(format_iso8601(as_datetimeoffset(dt)),
                 "2020-03-08T03:30:00-04:00")

    dt <- as_datetimeoffset("2020-03-08T01:30:00[America/New_York]")
    expect_equal(format_iso8601(dt), "2020-03-08T01:30:00-05:00")

    expect_equal(format_iso8601(NA_datetimeoffset_), NA_character_)

    skip_if_not_installed("lubridate")
    expect_equal(lubridate::format_ISO8601(dt),
                 "2020-03-08T01:30:00")
    expect_equal(lubridate::format_ISO8601(dt, usetz = TRUE),
                 "2020-03-08T01:30:00-0500")
    expect_equal(lubridate::format_ISO8601(dt, precision = "ymd"),
                 "2020-03-08")
    expect_equal(lubridate::format_ISO8601(NA_datetimeoffset_),
                 NA_character_)
})

test_that("format_pdfmark()", {
    # "2020-05-15T08:23:16-07:00"
    expect_equal(format_pdfmark(NA_datetimeoffset_), NA_character_)
    expect_equal(format_pdfmark(as.Date("2020-05-15")),
                 "D:20200515")
    expect_equal(format_pdfmark(as_datetimeoffset("D:2020")),
                 "D:2020")
    expect_equal(format_pdfmark(as_datetimeoffset("D:202005")),
                 "D:202005")
    expect_equal(format_pdfmark(as_datetimeoffset("D:20200515")),
                 "D:20200515")
    expect_equal(format_pdfmark(as_datetimeoffset("D:2020051508")),
                 "D:2020051508")
    expect_equal(format_pdfmark(as_datetimeoffset("D:202005150823")),
                 "D:202005150823")
    expect_equal(format_pdfmark(as_datetimeoffset("D:20200515082316")),
                 "D:20200515082316")
    expect_equal(format_pdfmark(as_datetimeoffset("2020-05-15T08:23:16Z")),
                 "D:20200515082316+00'00'")
    expect_equal(format_pdfmark(as_datetimeoffset("D:20200515082316-07'")),
                 "D:20200515082316-07'")
    expect_equal(format_pdfmark(as_datetimeoffset("D:20200515082316-07'00'")),
                 "D:20200515082316-07'00'")

    dt <- as_datetimeoffset("D:20200515082316-07'00'")
    dt <- set_second(dt, NA_integer_)
    expect_equal(format_pdfmark(dt), "D:202005150823")

    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    dt <- c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]")
    expect_equal(format_pdfmark(as_datetimeoffset(dt)),
                 c("D:2020", "D:20200515082316-07'00'"))
})

test_that("format_strftime()", {
    dt <- as_datetimeoffset("2020-04-04T10:10:10")
    expect_equal(format_strftime(dt), "2020-04-04 10:10:10")
    expect_equal(format_strftime(NA_datetimeoffset_), NA_character_)
})

test_that("format_nanotime()", {
    skip_if_not_installed("nanotime")
    dt <- as_datetimeoffset("2020-04-04T10:10:10Z")
    expect_equal(format_nanotime(dt, tz = "GMT"),
                 "2020-04-04T10:10:10.000000000+00:00")
    expect_equal(format_nanotime(dt, format = "%F %T %Ez", tz = "GMT"),
                 "2020-04-04 10:10:10 +00:00")
    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    expect_equal(format_nanotime(dt, tz = "America/Los_Angeles"),
                 "2020-04-04T03:10:10.000000000-07:00")
    skip_if_not_installed("RcppCCTZ", "0.2.12") # fixes bug with `as.nanotime(NA_character_)`
    expect_equal(format_nanotime(NA_datetimeoffset_), NA_character_)
})

test_that("format_edtf()", {
    # "2020-05-15T08:23:16-07:00"
    expect_equal(format_edtf(NA_datetimeoffset_), "")
    expect_equal(format_edtf(as_datetimeoffset("2020")),
                 "2020")
    expect_equal(format_edtf(as_datetimeoffset("2020-05")),
                 "2020-05")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15")),
                 "2020-05-15")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08")),
                 "2020-05-15T08")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08:23")),
                 "2020-05-15T08:23")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08:23:16")),
                 "2020-05-15T08:23:16")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08:23:16Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08:23:16-07")),
                 "2020-05-15T08:23:16-07")
    expect_equal(format_edtf(as_datetimeoffset("2020-05-15T08:23:16-0700")),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetimeoffset("2020-05-15T08:23:16.0-07:00")
    expect_equal(format_edtf(dt, precision = "year", offsets = FALSE),
                 "2020")
    expect_equal(format_edtf(dt, precision = "month", offsets = FALSE),
                 "2020-05")
    expect_equal(format_edtf(dt, precision = "day", offsets = FALSE),
                 "2020-05-15")
    expect_equal(format_edtf(dt, precision = "hour"),
                 "2020-05-15T08-07:00")
    expect_equal(format_edtf(dt, precision = "minute"),
                 "2020-05-15T08:23-07:00")
    expect_equal(format_edtf(dt, precision = "second"),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetimeoffset("D:20200515082316-0700")
    dt <- set_second(dt, NA_integer_)
    expect_equal(format_edtf(dt), "2020-05-15T08:23-07:00")

    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    dt <- c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]")
    expect_equal(format_edtf(as_datetimeoffset(dt)),
                 c("2020", "2020-05-15T08:23:16.000000000-07:00"))

    skip_if_not("America/New_York" %in% OlsonNames())
    # ambiguous time so not possible to compute offset
    dt <- "2020-11-01T01:30:00[America/New_York]"
    expect_equal(format_edtf(as_datetimeoffset(dt)),
                 "2020-11-01T01:30:00")

    # non-ambiguous times so possible to compute offset
    dt <- "2020-03-08T03:30:00[America/New_York]"
    expect_equal(format_edtf(as_datetimeoffset(dt)),
                 "2020-03-08T03:30:00-04:00")

    dt <- "2020-03-08T01:30:00[America/New_York]"
    expect_equal(format_edtf(as_datetimeoffset(dt)),
                 "2020-03-08T01:30:00-05:00")

    expect_equal(format_edtf(as_datetimeoffset("2020-XX-10T20:XX:05-07")),
                 "2020-XX-10T20:XX:05-07")
    expect_equal(format_edtf(as_datetimeoffset("2020-05"), precision = "nanosecond", usetz = TRUE),
                 "2020-05-XXTXX:XX:XX.XXXXXXXXX+XX:XX[X]")
})

test_that("negative/large years", {
    dts <- datetimeoffset(c(-123456L, -10000L, -1L, 0, 10000L, 123456L), 10L, 10L)
    es <- c("-123456-10-10", "-10000-10-10", "-0001-10-10",
            "0000-10-10", "+10000-10-10", "+123456-10-10")
    expect_equal(format(dts), es)
    expect_equal(format_iso8601(dts), es)
    expect_equal(format_edtf(dts), es)
    expect_equal(format_pdfmark(dts), c(NA_character_, NA_character_, NA_character_,
                                       "D:00001010", NA_character_, NA_character_))
})
