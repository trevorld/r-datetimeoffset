test_that("as_datetimeoffset.integer()", {
    expect_equal(format(as_datetimeoffset(2020L)), "2020")
    expect_equal(format(as_datetimeoffset(-2020L)), "-2020")
    expect_equal(format(as_datetimeoffset(20200L)), "+20200")
    expect_equal(format(as_datetimeoffset(2020)), "2020")
    expect_equal(format(as_datetimeoffset(-2020)), "-2020")
    expect_equal(format(as_datetimeoffset(20200)), "+20200")
})

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

    expect_equal(format(as_datetimeoffset("-2020")),
                 "-2020")
    expect_equal(format(as_datetimeoffset("+12020")),
                 "+12020")

    # YM
    expect_equal(format(as_datetimeoffset("D:202005")),
                 "2020-05")
    expect_equal(format(as_datetimeoffset("2020-05")),
                 "2020-05")
    expect_equal(format(as_datetimeoffset("-2020-05")),
                 "-2020-05")
    expect_equal(format(as_datetimeoffset("+12020-05")),
                 "+12020-05")
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
    expect_equal(format(as_datetimeoffset("20060926213913+02'00'")),
                 "2006-09-26T21:39:13+02:00")

    # Time (without day)
    expect_equal(format(as_datetimeoffset("T20")), "T20")

    for (time in c("T20:20", "20:20", "T2020"))
        expect_equal(format(as_datetimeoffset(time)), "T20:20")

    for (time in c("T20:20:04", "20:20:04", "T202004"))
        expect_equal(format(as_datetimeoffset(time)), "T20:20:04")

    for (time in c("T20:20:04.004", "20:20:04.004", "T202004.004"))
        expect_equal(format(as_datetimeoffset(time)), "T20:20:04.004")

    # Weird pdfmark ending found in `ghostscript` output in UTC time locale
    expect_equal(format(as_datetimeoffset("D:20060926213913Z00'00'")),
                 "2006-09-26T21:39:13Z")

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

    expect_equal(format_edtf(as_datetimeoffset("--10-19")),
                 "XXXX-10-19")
    expect_equal(format_edtf(as_datetimeoffset("--1019")),
                 "XXXX-10-19")
    expect_equal(format_edtf(as_datetimeoffset("--1019101010")),
                 "XXXX-10-19T10:10:10")

    # YMDhmsz
    skip_if_not("America/Los_Angeles" %in% OlsonNames())
    expect_equal(format(as_datetimeoffset("2020-05-15T08:23:16[America/Los_Angeles]")),
                 "2020-05-15T08:23:16-07:00[America/Los_Angeles]")

})

test_that("ordinal dates", {
    expect_equal(format(as_datetimeoffset("2020-150")), "2020-05-29")
    expect_equal(format(as_datetimeoffset("2020150")), "2020-05-29")
    expect_equal(format(as_datetimeoffset("2020-150T10:10")), "2020-05-29T10:10")
    expect_equal(format(as_datetimeoffset("2020150 04:04")), "2020-05-29T04:04")
})

test_that("ISO week dates", {
    expect_equal(format(as_datetimeoffset("2009-W01-1")), "2008-12-29")
    expect_equal(format(as_datetimeoffset("2009W011")), "2008-12-29")
    dt <- as_datetimeoffset("2009-W01")
    expect_true(is.na(get_year(dt)))
    dt <- as_datetimeoffset("2009W01")
    expect_true(is.na(get_year(dt)))
    dt <- as_datetimeoffset("2009-W02")
    expect_equal(get_year(dt), 2009)
    expect_equal(get_month(dt), 1)
    expect_equal(format(as_datetimeoffset("2009-W53-7")), "2010-01-03")
    expect_equal(format(as_datetimeoffset("2009-W53-7T10:10")), "2010-01-03T10:10")
    expect_equal(format(as_datetimeoffset("2009-W53-7T10:11:12+05:30")), "2010-01-03T10:11:12+05:30")
    expect_equal(format(as_datetimeoffset("2009W537")), "2010-01-03")
    expect_equal(format(as_datetimeoffset("2009W537T1010")), "2010-01-03T10:10")
})

test_that("base R classes", {
    # Date
    s <- c("2020-05-15", NA_character_)
    expect_equal(format(as_datetimeoffset(as.Date(s))), s)

    skip_if_not("America/New_York" %in% OlsonNames())
    # POSIXct
    dt <- as.POSIXct(c("2022-10-10 10:00:00", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00.000000-04:00[America/New_York]", NA_character_))

    dt <- as.POSIXct(c("2022-10-10 10:00:00.123456", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00.123456-04:00[America/New_York]", NA_character_))
    dt <- as.POSIXct(c("2019-01-01 01:00:00.1", "2019-01-01 01:00:00.3"), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2019-01-01T01:00:00.100000-05:00[America/New_York]",
                   "2019-01-01T01:00:00.300000-05:00[America/New_York]"))

    dtn <- as.POSIXct("2022-10-10 10:00:00", tz = "GMT") - as.difftime(10000 * 365 * 24, units = "hours")
    dtn1 <- as_datetimeoffset(dtn)
    expect_equal(format(dtn1), "-7971-05-31T10:00:00.000000Z")
    dtl <- as.POSIXct("2022-10-10 10:00:00", tz = "GMT") + as.difftime(10000 * 365 * 24, units = "hours")
    dtl1 <- as_datetimeoffset(dtl)
    expect_equal(format(dtl1), "+12016-02-19T10:00:00.000000Z")

    # POSIXlt
    dtn2 <- as_datetimeoffset(as.POSIXlt(dtn))
    expect_equal(format(dtn2), "-7971-05-31T10:00:00.000000Z")
    dtl2 <- as_datetimeoffset(as.POSIXlt(dtl))
    expect_equal(format(dtl2), "+12016-02-19T10:00:00.000000Z")

    dt <- as.POSIXlt(c("2022-10-10 10:00:00", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00.000000-04:00[America/New_York]", NA_character_))

    dt <- as.POSIXlt(c("2022-10-10 10:00:00.123456", NA_character_), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2022-10-10T10:00:00.123456-04:00[America/New_York]", NA_character_))
    dt <- as.POSIXlt(c("2019-01-01 01:00:00.1", "2019-01-01 01:00:00.3"), tz = "America/New_York")
    expect_equal(format(as_datetimeoffset(dt)),
                 c("2019-01-01T01:00:00.100000-05:00[America/New_York]",
                   "2019-01-01T01:00:00.300000-05:00[America/New_York]"))
    # potentially ambiguous time
    dt <- as.POSIXlt(as_datetimeoffset("2020-11-01T01:30:00.123456-04:00[America/New_York]"))
    expect_equal(format(as_datetimeoffset(dt)),
                 "2020-11-01T01:30:00.123456-04:00[America/New_York]")
    dt <- as.POSIXlt(as_datetimeoffset("2020-11-01T01:30:00.123456-05:00[America/New_York]"))
    expect_equal(format(as_datetimeoffset(dt)),
                 "2020-11-01T01:30:00.123456-05:00[America/New_York]")
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

    # year less than zero
    ymd <- clock::year_month_day(-73, 10, 10)
    dto <- as_datetimeoffset(ymd)
    expect_equal(format(dto), "-0073-10-10")
    nt <- clock::as_naive_time(ymd)
    dto <- as_datetimeoffset(nt)
    expect_equal(format(dto), "-0073-10-10")
    st <- clock::as_sys_time(ymd)
    dto <- as_datetimeoffset(st)
    expect_equal(format(dto), "-0073-10-10")

    # year more than 9999
    ymd <- clock::year_month_day(11234, 10, 10)
    dto <- as_datetimeoffset(ymd)
    expect_equal(format(dto), "+11234-10-10")
    nt <- clock::as_naive_time(ymd)
    dto <- as_datetimeoffset(nt)
    expect_equal(format(dto), "+11234-10-10")
    st <- clock::as_sys_time(ymd)
    dto <- as_datetimeoffset(st)
    expect_equal(format(dto), "+11234-10-10")

    # ambiguous times
    dts <- c("2020-11-01T01:30:00[America/New_York]", # ambiguous sys time
             "2020-11-01T01:30:00-05:00",
             "2020-11-01T01:30:00-04:00")
    dto <- as_datetimeoffset(dts)
    expect_error(clock::as_sys_time(dto))
    expect_equal(format(as_sys_time(dto, ambiguous = "NA")),
                 c(NA_character_, "2020-11-01T06:30:00", "2020-11-01T05:30:00"))
    expect_equal(format(as_sys_time(dto, ambiguous = "earliest")),
                 c("2020-11-01T05:30:00", "2020-11-01T06:30:00", "2020-11-01T05:30:00"))
    expect_equal(format(as_sys_time(dto, ambiguous = "latest")),
                 c("2020-11-01T06:30:00", "2020-11-01T06:30:00", "2020-11-01T05:30:00"))
    expect_error(clock::as_zoned_time(dto))
    skip_if_not("UTC" %in% OlsonNames())
    expect_equal(format(as_zoned_time(dto, "UTC", ambiguous = "NA")),
                 c(NA_character_,
                   "2020-11-01T06:30:00+00:00[UTC]",
                   "2020-11-01T05:30:00+00:00[UTC]"))
    expect_equal(format(as_zoned_time(dto, "UTC", ambiguous = "earliest")),
                 c("2020-11-01T05:30:00+00:00[UTC]",
                   "2020-11-01T06:30:00+00:00[UTC]",
                   "2020-11-01T05:30:00+00:00[UTC]"))
    expect_equal(format(as_zoned_time(dto, "UTC", ambiguous = "latest")),
                 c("2020-11-01T06:30:00+00:00[UTC]",
                   "2020-11-01T06:30:00+00:00[UTC]",
                   "2020-11-01T05:30:00+00:00[UTC]"))
})

test_that("as_datetimeoffset.nanotime()", {
    skip_if_not_installed("nanotime")
    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16.03Z"))
    expect_equal(format(dt), "2020-05-15T08:23:16.030000000Z")
    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"))
    expect_equal(get_nanosecond(dt), 0L)
    dt <- as_datetimeoffset(nanotime::as.nanotime(c(NA_integer_, 2000L)))
    expect_equal(format(dt), c(NA_character_, "1970-01-01T00:00:00.000002000Z"))
    expect_equal(is.na(dt), c(TRUE, FALSE))

    skip_if_not("America/New_York" %in% OlsonNames())

    dt <- as_datetimeoffset(nanotime::nanotime("2020-05-15T08:23:16Z"),
                            tz = c("GMT", "America/New_York"))
    expect_equal(format(dt),
                 c("2020-05-15T08:23:16.000000000Z",
                   "2020-05-15T04:23:16.000000000-04:00[America/New_York]"))
})

test_that("improved subsecond precision", {
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.0")),
                 "2020-02-02T10:10:10.0")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.1")),
                 "2020-02-02T10:10:10.1")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.12")),
                 "2020-02-02T10:10:10.12")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.123")),
                 "2020-02-02T10:10:10.123")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.1234")),
                 "2020-02-02T10:10:10.1234")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.12345")),
                 "2020-02-02T10:10:10.12345")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.123456")),
                 "2020-02-02T10:10:10.123456")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.1234567")),
                 "2020-02-02T10:10:10.1234567")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.12345678")),
                 "2020-02-02T10:10:10.12345678")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.123456789")),
                 "2020-02-02T10:10:10.123456789")

    # max nanotime precision
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.1234567890")),
                 "2020-02-02T10:10:10.123456789")
    expect_equal(format(as_datetimeoffset.character("2020-02-02T10:10:10.12345678901")),
                 "2020-02-02T10:10:10.123456789")

    # precision strings
    dts <- c("2020-02-02T10:10:10",
             "2020-02-02T10:10:10.1",
             "2020-02-02T10:10:10.12",
             "2020-02-02T10:10:10.123",
             "2020-02-02T10:10:10.1234",
             "2020-02-02T10:10:10.12345",
             "2020-02-02T10:10:10.123456",
             "2020-02-02T10:10:10.1234567",
             "2020-02-02T10:10:10.12345678",
             "2020-02-02T10:10:10.123456789")
    dto <- as_datetimeoffset.character(dts)
    expect_equal(format(dto), dts)
    expect_equal(datetime_precision(dto),
                 c("second", "decisecond", "centisecond", "millisecond",
                   "hundred microseconds", "ten microseconds", "microsecond",
                   "hundred nanoseconds", "ten nanoseconds", "nanosecond"))

    # keep precision of explicit zeroes
    dt <- as_datetimeoffset.character("2020-02-02T10:10:10.10")
    expect_equal(get_subsecond_digits(dt), 2L)
    expect_equal(datetime_precision(dt), "centisecond")
    expect_equal(format(dt), "2020-02-02T10:10:10.10")

    dt <- datetime_widen(dt, "ten nanoseconds")
    expect_equal(format(dt), "2020-02-02T10:10:10.10000000")
    dt <- datetime_narrow(dt, "hundred microseconds")
    expect_equal(format(dt), "2020-02-02T10:10:10.1000")

    dt <- as_datetimeoffset.character("2020-02-02T10:10:10.1000")
    expect_equal(get_subsecond_digits(dt), 4L)
    expect_equal(datetime_precision(dt), "hundred microseconds")
    expect_equal(format(dt), "2020-02-02T10:10:10.1000")
})
