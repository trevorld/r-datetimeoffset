test_that("as_datetimeoffset()", {

    expect_error(as_datetimeoffset("This is obviously not a date"))

    # "2020-05-15T08:23:16-07:00"
    # Y
    expect_equal(format(as_datetimeoffset("D:2020")), # Y
                 "2020")
    # YM
    expect_equal(format(as_datetimeoffset("D:202005")),
                 "2020-05")
    expect_equal(format(as_datetimeoffset("2020-05")),
                 "2020-05")
    # YMD
    expect_equal(format(as_datetimeoffset("D:20200515")),
                 "2020-05-15")
    expect_equal(format(as_datetimeoffset("2020-05-15")),
                 "2020-05-15")
    expect_equal(format(as_datetimeoffset(as.Date("2020-05-15"))),
                 "2020-05-15")
    # YMDh
    expect_equal(format(as_datetimeoffset("D:2020051508")),
                 "2020-05-15T08")
    expect_equal(format(as_datetimeoffset("2020-05-15T08")),
                 "2020-05-15T08")
    expect_equal(format(as_datetimeoffset("2020/05/15 08")),
                 "2020-05-15T08")
    expect_equal(format(as_datetimeoffset("20200515T08")),
                 "2020-05-15T08")
    # YMDho (not allowed by pdfmark but is allowed by ISO 8601)
    expect_equal(format(as_datetimeoffset("2020-05-15T08Z")),
                 "2020-05-15T08Z")
    expect_equal(format(as_datetimeoffset("20200515T08Z")),
                 "2020-05-15T08Z")
    expect_equal(format(as_datetimeoffset("2020-05-15T08-07")),
                 "2020-05-15T08-07")
    expect_equal(format(as_datetimeoffset("2020-05-15T08-0700")),
                 "2020-05-15T08-07:00")
    expect_equal(format(as_datetimeoffset("2020-05-15T08-07:00")),
                 "2020-05-15T08-07:00")
    expect_equal(format(as_datetimeoffset("20200515T08-07")),
                 "2020-05-15T08-07")
    expect_equal(format(as_datetimeoffset("20200515T08-0700")),
                 "2020-05-15T08-07:00")
    # YMDhm
    expect_equal(format(as_datetimeoffset("D:202005150823")),
                 "2020-05-15T08:23")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23")),
                 "2020-05-15T08:23")
    expect_equal(format(as_datetimeoffset("2020/05/15 08:23")),
                 "2020-05-15T08:23")
    expect_equal(format(as_datetimeoffset("20200515T0823")),
                 "2020-05-15T08:23")

    # YMDhmo (not allowed by pdfmark but is allowed by ISO 8601)
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23+03")),
                 "2020-05-15T08:23+03")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23-03")),
                 "2020-05-15T08:23-03")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23+0330")),
                 "2020-05-15T08:23+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23-0330")),
                 "2020-05-15T08:23-03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23+03:30")),
                 "2020-05-15T08:23+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23-03:30")),
                 "2020-05-15T08:23-03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23Z")),
                 "2020-05-15T08:23Z")

    expect_equal(format(as_datetimeoffset("20200515T0823Z")),
                 "2020-05-15T08:23Z")
    expect_equal(format(as_datetimeoffset("20200515T0823+03")),
                 "2020-05-15T08:23+03")
    expect_equal(format(as_datetimeoffset("20200515T0823-03")),
                 "2020-05-15T08:23-03")
    expect_equal(format(as_datetimeoffset("20200515T0823+0330")),
                 "2020-05-15T08:23+03:30")
    expect_equal(format(as_datetimeoffset("20200515T0823-0330")),
                 "2020-05-15T08:23-03:30")

    # YMDhms
    expect_equal(format(as_datetimeoffset("D:20200515082316")),
                 "2020-05-15T08:23:16")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16")),
                 "2020-05-15T08:23:16")
    expect_equal(format(as_datetimeoffset("2020/05/15 08:23:16")),
                 "2020-05-15T08:23:16")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16.003")),
                 "2020-05-15T08:23:16.003")
    expect_equal(format(as_datetimeoffset("20200515T082316.003")),
                 "2020-05-15T08:23:16.003")

    # YMDhmsz
    if ("America/Los_Angeles" %in% OlsonNames())
        expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16[America/Los_Angeles]")),
                     "2020-05-15T08:23:16-07:00[America/Los_Angeles]")

    # YMDhmso
    expect_equal(format(as_datetimeoffset("D:20200515082316+03")),
                 "2020-05-15T08:23:16+03")
    expect_equal(format(as_datetimeoffset("D:20200515082316-03")),
                 "2020-05-15T08:23:16-03")
    expect_equal(format(as_datetimeoffset("D:20200515082316+0330")),
                 "2020-05-15T08:23:16+03:30")
    expect_equal(format(as_datetimeoffset("D:20200515082316-0330")),
                 "2020-05-15T08:23:16-03:30")

    expect_equal(format(as_datetimeoffset("D:20060926213913+02'00'")),
                 "2006-09-26T21:39:13+02:00")

    # lower-case "t" and "z"
    expect_equal(format(as_datetimeoffset("2020-05-15t08:23:16z")),
                 "2020-05-15T08:23:16Z")

    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16+03")),
                 "2020-05-15T08:23:16+03")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16.003+03")),
                 "2020-05-15T08:23:16.003+03")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16-03")),
                 "2020-05-15T08:23:16-03")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16+0330")),
                 "2020-05-15T08:23:16+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16.003+0330")),
                 "2020-05-15T08:23:16.003+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16-0330")),
                 "2020-05-15T08:23:16-03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16+03:30")),
                 "2020-05-15T08:23:16+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16.003+03:30")),
                 "2020-05-15T08:23:16.003+03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16-03:30")),
                 "2020-05-15T08:23:16-03:30")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16.003Z")),
                 "2020-05-15T08:23:16.003Z")

    expect_equal(format(as_datetimeoffset("20200515082316Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format(as_datetimeoffset("20200515T082316Z")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format(as_datetimeoffset("20200515T082316.003Z")),
                 "2020-05-15T08:23:16.003Z")
    expect_equal(format(as_datetimeoffset("20200515T082316+03")),
                 "2020-05-15T08:23:16+03")
    expect_equal(format(as_datetimeoffset("20200515T082316.003+03")),
                 "2020-05-15T08:23:16.003+03")
    expect_equal(format(as_datetimeoffset("20200515T082316-03")),
                 "2020-05-15T08:23:16-03")
    expect_equal(format(as_datetimeoffset("20200515T082316+0330")),
                 "2020-05-15T08:23:16+03:30")
    expect_equal(format(as_datetimeoffset("20200515T082316.003+0330")),
                 "2020-05-15T08:23:16.003+03:30")
    expect_equal(format(as_datetimeoffset("20200515T082316-0330")),
                 "2020-05-15T08:23:16-03:30")

    # Date
    expect_equal(format(as_datetimeoffset(as.Date("2020-05-15"))),
                 "2020-05-15")

    # nanotime
    expect_equal(format(as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16.03Z"))),
                 "2020-05-15T08:23:16.03Z")
    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"))
    expect_equal(nanosecond(dt), 0L)

    skip_if_not("US/Eastern" %in% OlsonNames())

    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"),
                            tz = c("GMT", "US/Eastern"))
    expect_equal(format(dt),
                 c("2020-05-15T08:23:16.0Z",
                   "2020-05-15T04:23:16.0-04:00[US/Eastern]"))

    # POSIXct
    dt <- as.POSIXct("2022-10-10 10:00:00", tz = "US/Eastern")
    expect_equal(format(as_datetimeoffset(dt)),
                 "2022-10-10T10:00:00.0-04:00[US/Eastern]")

    # POSIXlt
    dt <- as.POSIXlt("2022-10-10 10:00:00", tz = "US/Eastern")
    expect_equal(format(as_datetimeoffset(dt)),
                 "2022-10-10T10:00:00.0-04:00[US/Eastern]")
})
