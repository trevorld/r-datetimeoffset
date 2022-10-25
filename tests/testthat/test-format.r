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
    expect_equal(format_iso8601(dt, precision = "y", usetz = FALSE),
                 "2020")
    expect_equal(format_iso8601(dt, precision = "ym", usetz = FALSE),
                 "2020-05")
    expect_equal(format_iso8601(dt, precision = "ymd", usetz = FALSE),
                 "2020-05-15")
    expect_equal(format_iso8601(dt, precision = "ymdh"),
                 "2020-05-15T08-07:00")
    expect_equal(format_iso8601(dt, precision = "ymdhm"),
                 "2020-05-15T08:23-07:00")
    expect_equal(format_iso8601(dt, precision = "ymdhms"),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetimeoffset("D:20200515082316-0700")
    dt <- set_second(dt, NA_integer_)
    expect_equal(format_iso8601(dt), "2020-05-15T08:23-07:00")

    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    dt <- c("2020", "2020-05-15T08:23:16.0-07:00[America/Los_Angeles]")
    expect_equal(format_iso8601(as_datetimeoffset(dt)),
                 c("2020", "2020-05-15T08:23:16.0-07:00"))
})

test_that("format_pdfmark()", {
    # "2020-05-15T08:23:16-07:00"
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
})

test_that("format_nanotime()", {
    dt <- as_datetimeoffset("2020-04-04T10:10:10Z")
    expect_equal(format_nanotime(dt, tz = "GMT"),
                 "2020-04-04T10:10:10.000000000+00:00")
    expect_equal(format_nanotime(dt, format = "%F %T %Ez", tz = "GMT"),
                 "2020-04-04 10:10:10 +00:00")
    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    expect_equal(format_nanotime(dt, tz = "America/Los_Angeles"),
                 "2020-04-04T03:10:10.000000000-07:00")
})
