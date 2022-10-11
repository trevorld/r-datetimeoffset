test_that("format_ISO8601()", {
    # "2020-05-15T08:23:16-07:00"
    expect_equal(format_ISO8601(as_datetime_offset("2020")),
                 "2020")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05")),
                 "2020-05")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15")),
                 "2020-05-15")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08")),
                 "2020-05-15T08")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08:23")),
                 "2020-05-15T08:23")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08:23:16")),
                 "2020-05-15T08:23:16")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08:23:16Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08:23:16-07")),
                 "2020-05-15T08:23:16-07")
    expect_equal(format_ISO8601(as_datetime_offset("2020-05-15T08:23:16-0700")),
                 "2020-05-15T08:23:16-07:00")

    dt <- as_datetime_offset("D:20200515082316-0700")
    second(dt) <- NA_integer_
    expect_equal(format_ISO8601(dt), "2020-05-15T08:23-07:00")
})

test_that("format_pdfmark()", {
    # "2020-05-15T08:23:16-07:00"
    expect_equal(format_pdfmark(as.Date("2020-05-15")),
                 "D:20200515")
    expect_equal(format_pdfmark(as_datetime_offset("D:2020")),
                 "D:2020")
    expect_equal(format_pdfmark(as_datetime_offset("D:202005")),
                 "D:202005")
    expect_equal(format_pdfmark(as_datetime_offset("D:20200515")),
                 "D:20200515")
    expect_equal(format_pdfmark(as_datetime_offset("D:2020051508")),
                 "D:2020051508")
    expect_equal(format_pdfmark(as_datetime_offset("D:202005150823")),
                 "D:202005150823")
    expect_equal(format_pdfmark(as_datetime_offset("D:20200515082316")),
                 "D:20200515082316")
    expect_equal(format_pdfmark(as_datetime_offset("2020-05-15T08:23:16Z")),
                 "D:20200515082316+0000")
    expect_equal(format_pdfmark(as_datetime_offset("D:20200515082316-07")),
                 "D:20200515082316-07")
    expect_equal(format_pdfmark(as_datetime_offset("D:20200515082316-0700")),
                 "D:20200515082316-0700")

    dt <- as_datetime_offset("D:20200515082316-0700")
    second(dt) <- NA_integer_
    expect_equal(format_pdfmark(dt), "D:202005150823")
})
