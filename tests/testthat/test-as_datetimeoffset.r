test_that("as_datetimeoffset.character()", {

    expect_error(as_datetimeoffset("This is obviously not a date"))
    expect_error(as_datetimeoffset(complex(20)))
    expect_error((function() assert_suggested("this.package.does.not.exist"))())

    expect_equal(as_datetimeoffset(""), NA_datetimeoffset_)
    expect_equal(is.na(as_datetimeoffset(c("", NA_character_, "2020-10"))),
                 c(TRUE, TRUE, FALSE))

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

    # EDTF
    expect_equal(format_edtf(as_datetimeoffset("2020-XX-10")),
                 "2020-XX-10")
    expect_equal(format_edtf(as_datetimeoffset("20XX-XX-10")),
                 "XXXX-XX-10")
    dt <- as_datetimeoffset("2020-XX-10T10:10:10[X]")
    expect_equal(get_tz(dt), NA_character_)
    dt <- as_datetimeoffset("2020-XX-10T10:10:10+XX:10[X]")
    expect_equal(get_hour_offset(dt), NA_integer_)
    expect_equal(get_minute_offset(dt), 10L)
    expect_equal(get_tz(dt), NA_character_)
    dt <- as_datetimeoffset("2020-XX-10T10:10:10.003+XX:10[X]")
    expect_equal(get_nanosecond(dt), 3e6)
    expect_equal(get_hour_offset(dt), NA_integer_)
    expect_equal(get_minute_offset(dt), 10L)
    expect_equal(get_tz(dt), NA_character_)

    # YMDhmsz
    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16[America/Los_Angeles]")),
                 "2020-05-15T08:23:16-07:00[America/Los_Angeles]")

})

test_that("base R classes", {
    # Date
    s <- c("2020-05-15", NA_character_)
    expect_equal(format(as_datetimeoffset(as.Date(s))), s)

    skip_if_not("America/New_York" %in% OlsonNames())
    # POSIXct
    dt <- as.POSIXct(c("2022-10-10 10:00:00", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00-04:00[America/New_York]", NA_character_))

    # POSIXlt
    dt <- as.POSIXlt(c("2022-10-10 10:00:00", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00-04:00[America/New_York]", NA_character_))


    dt <- as.POSIXct(c("2019-01-01 01:00:00.1", "2019-01-01 01:00:00.3"), tz = "America/New_York")

    dt <- as.POSIXct(c("2022-10-10 10:00:00.123456", NA_character_), tz = "America/New_York")
    # format(dt, tz = "America/New_York", digits = 6L)
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00-04:00[America/New_York]", NA_character_))
    expect_equal(format(as_datetimeoffset(as.nanotime(dt), tz = get_tz(dt))),
                 c("2022-10-10T10:00:00.123456-04:00[America/New_York]", NA_character_))
})

test_that("{clock} classes", {
    ymd <- clock::year_month_day(2020, c(NA, 10), 10)
    dto <- as_datetimeoffset(ymd)
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_year_month_weekday(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_iso_year_week_day(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_year_quarter_day(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_year_day(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_naive_time(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_sys_time(ymd))
    expect_equal(format(dto), c(NA_character_, "2020-10-10"))
    expect_equal(is.na(dto), c(TRUE, FALSE))
    dto <- as_datetimeoffset(clock::as_zoned_time(clock::as_sys_time(ymd), Sys.timezone()))
    expect_equal(format(dto)[1], c(NA_character_))
    expect_equal(is.na(dto), c(TRUE, FALSE))
})

test_that("as_datetimeoffset.nanotime()", {
    skip_if_not_installed("nanotime")
    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16.03Z"))
    expect_equal(format(dt), "2020-05-15T08:23:16.03Z")
    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"))
    expect_equal(get_nanosecond(dt), 0L)
    dt <- as_datetimeoffset(nanotime::as.nanotime(c(NA_integer_, 2000L)))
    expect_equal(format(dt), c(NA_character_, "1970-01-01T00:00:00.000002Z"))
    expect_equal(is.na(dt), c(TRUE, FALSE))

    skip_if_not("America/New_York" %in% OlsonNames())

    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"),
                            tz = c("GMT", "America/New_York"))
    expect_equal(format(dt),
                 c("2020-05-15T08:23:16.0Z",
                   "2020-05-15T04:23:16.0-04:00[America/New_York]"))
})
