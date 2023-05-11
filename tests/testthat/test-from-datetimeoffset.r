test_that("as.Date()", {
    expect_equal(as.Date(as_datetimeoffset(c("2020-03-23T10:10:10", "2020-03-23", ""))),
                 as.Date(c("2020-03-23", "2020-03-23", NA_character_)))
    expect_equal(clock::as_date(as_datetimeoffset(c("2020-03-23T10:10:10", "2020-03-23", ""))),
                 as.Date(c("2020-03-23", "2020-03-23", NA_character_)))
})

test_that("as.nanotime()", {
    skip_if_not_installed("nanotime")
    skip_if_not_installed("RcppCCTZ", "0.2.12") # fixes bug with `as.nanotime(NA_character_)`
    expect_equal(as.nanotime(as_datetimeoffset("2020-03-23Z", "2020-03-23")),
                 as.nanotime("2020-03-23T00:00:00Z", NA_character_))
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    expect_equal(as.nanotime(as_datetimeoffset("2020-03-23T04:04:04",
                             tz = c("America/Los_Angeles", "America/New_York"))),
                 as.nanotime(c("2020-03-23T11:04:04Z", "2020-03-23T08:04:04Z")))
    expect_equal(as.nanotime(as_datetimeoffset(c("2020-03-23T04:04:04Z", NA_character_))),
                 as.nanotime(c("2020-03-23T04:04:04Z", NA_character_)))
})

test_that("`as.parttime.datetimeoffset()` and `as_datetimeoffset.parttime()`", {
    skip_if_not_installed("lubridate")
    suppressPackageStartupMessages(skip_if_not_installed("parttime"))
    dto <- as_datetimeoffset("2020-02-04T01:01:05Z")
    pt <- parttime::as.parttime(dto)
    expect_equal(pt[, "year"], 2020)
    expect_equal(pt[, "month"], 2)
    expect_equal(pt[, "day"], 4)
    expect_equal(pt[, "hour"], 1)
    expect_equal(pt[, "min"], 1)
    expect_equal(pt[, "sec"], 5)
    expect_equal(pt[, "tzhour"], 0)

    dt <- as_datetimeoffset(pt)
    expect_equal(get_year(dt), 2020L)
    expect_equal(get_month(dt), 2L)
    expect_equal(get_day(dt), 4L)
    expect_equal(get_hour(dt), 1L)
    expect_equal(get_minute(dt), 1L)
    expect_equal(get_second(dt), 5L)
    expect_equal(get_nanosecond(dt), 0L)
    expect_equal(get_hour_offset(dt), 0L)
    expect_equal(get_minute_offset(dt), 0L)
    expect_equal(get_tz(dt), NA_character_)

    dto <- as_datetimeoffset("2020-02-04T01:01:05.1234-05:30")
    pt <- parttime::as.parttime(dto)
    expect_equal(pt[, "sec"], 5.1234)
    expect_equal(pt[, "tzhour"], -5.5)

    dt <- as_datetimeoffset(pt)
    expect_equal(get_second(dt), 5L)
    expect_equal(get_nanosecond(dt), 123400000)
    expect_equal(get_hour_offset(dt), -5L)
    expect_equal(get_minute_offset(dt), 30L)
    expect_equal(get_tz(dt), NA_character_)

    dto <- as_datetimeoffset("2020-02-04T01:01:05[America/Los_Angeles]")
    pt <- parttime::as.parttime(dto)
    expect_equal(pt[, "year"], 2020)
    expect_equal(pt[, "month"], 2)
    expect_equal(pt[, "day"], 4)
    expect_equal(pt[, "hour"], 1)
    expect_equal(pt[, "min"], 1)
    expect_equal(pt[, "sec"], 5)
    expect_equal(pt[, "tzhour"], -8)
})

test_that("as.POSIXct()", {
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(as.POSIXct(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(clock::as_date_time(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset(c("2020-06-15T10:10:10[America/Los_Angeles]", NA_character_))
    expect_equal(is.na(as.POSIXct(dt)), c(FALSE, TRUE))

    # ambiguous times
    dt <- as_datetimeoffset("2020-11-01T01:30:00-05:00[America/New_York]")
    expect_equal(format(dt), "2020-11-01T01:30:00-05:00[America/New_York]")
    expect_equal(format(as.POSIXct(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00")
    dt <- as_datetimeoffset("2020-11-01T01:30:00-04:00[America/New_York]")
    expect_equal(format(dt), "2020-11-01T01:30:00-04:00[America/New_York]")
    expect_equal(format(as.POSIXct(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00")

    # Preserve sub-seconds
    skip_if_not(getRversion() > '4.2.0')
    dt <- as_datetimeoffset("2019-01-01 01:00:00.123456[America/New_York]")
    expect_equal(format(as.POSIXct(dt), tz = "America/New_York", digits = 6L),  "2019-01-01 01:00:00.123456")
    dt <- as_datetimeoffset("2020-11-01T01:30:00.123456-04:00[America/New_York]")
    expect_equal(format(as.POSIXct(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00.123456")

    # Negative or large years
    dt <- datetimeoffset(-7971, 5, 31, 10, tz = "GMT")
    expect_equal(format(as.POSIXct(dt)), "-7971-05-31 10:00:00")

    dt <- datetimeoffset(12016, 2, 19, 10, tz = "GMT")
    expect_equal(format(as.POSIXct(dt)), "12016-02-19 10:00:00")
})

test_that("as.POSIXlt()", {
    expect_equal(format(as.POSIXlt("2020-03-23 04:04:04")),
                 format(as.POSIXlt(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset(c("2020-06-15T10:10:10[America/Los_Angeles]", NA_character_))
    expect_equal(is.na(as.POSIXlt(dt)), c(FALSE, TRUE))

    # ambiguous times
    dt <- as_datetimeoffset("2020-11-01T01:30:00-05:00[America/New_York]")
    expect_equal(format(as.POSIXlt(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00")
    dt <- as_datetimeoffset("2020-11-01T01:30:00-04:00[America/New_York]")
    expect_equal(format(as.POSIXlt(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00")

    # Preserve sub-seconds
    skip_if_not(getRversion() > '4.2.0')
    dt <- as_datetimeoffset("2019-01-01 01:00:00.123456[America/New_York]")
    expect_equal(format(as.POSIXlt(dt), tz = "America/New_York", digits = 6L),  "2019-01-01 01:00:00.123456")
    dt <- as_datetimeoffset("2020-11-01T01:30:00.123456-04:00[America/New_York]")
    expect_equal(format(as.POSIXlt(dt), tz = "America/New_York", digits = 6L),  "2020-11-01 01:30:00.123456")

    # Negative or large years
    dtn <- datetimeoffset(-7971, 5, 31, 10, tz = "GMT")
    expect_equal(format(as.POSIXlt(dtn)), "-7971-05-31 10:00:00")

    dtl <- datetimeoffset(12016, 2, 19, 10, 10, 10, 123456000L, tz = "GMT")
    expect_equal(format(as.POSIXlt(dtl), digits = 6L), "12016-02-19 10:10:10.123456")
})

test_that("clock classes", {
    dt <- as_datetimeoffset("2020-03-23T04:04:04Z")
    dtn <- datetimeoffset(-7971, 5, 31, 10, tz = "GMT")
    dtl <- datetimeoffset(12016, 2, 19, 10, tz = "GMT")

    ymd <- clock::as_year_month_day(dt)
    expect_equal(format(ymd), "2020-03-23T04:04:04")
    expect_equal(format(as_datetimeoffset(ymd)),
                 "2020-03-23T04:04:04")
    expect_equal(format(clock::as_year_month_day(dtn)), "-7971-05-31T10")
    expect_equal(format(clock::as_year_month_day(dtl)), "12016-02-19T10")
    ymd <- clock::year_month_day(c(1984L, NA_integer_))
    expect_equal(is.na(as_datetimeoffset(ymd)), c(FALSE, TRUE))
    expect_equal(format(as_datetimeoffset(ymd)), c("1984", NA_character_))

    ymw <- clock::as_year_month_weekday(dt)
    expect_equal(format(ymw), "2020-03-Mon[4]T04:04:04")
    expect_equal(format(as_datetimeoffset(ymw)),
                 "2020-03-23T04:04:04")

    ywd <- clock::as_iso_year_week_day(dt)
    expect_equal(format(ywd), "2020-W13-1T04:04:04")
    expect_equal(format(as_datetimeoffset(ywd)),
                 "2020-03-23T04:04:04")

    yqd <- clock::as_year_quarter_day(dt)
    expect_equal(format(yqd), "2020-Q1-83T04:04:04")
    expect_equal(format(as_datetimeoffset(yqd)),
                 "2020-03-23T04:04:04")

    yd <- clock::as_year_day(dt)
    expect_equal(format(yd), "2020-083T04:04:04")
    expect_equal(format(as_datetimeoffset(yd)),
                 "2020-03-23T04:04:04")

    nt <- clock::as_naive_time(dt)
    expect_equal(format(nt), "2020-03-23T04:04:04")
    expect_equal(format(as_datetimeoffset(nt)),
                 "2020-03-23T04:04:04")
    expect_equal(format(clock::as_naive_time(dtn)), "-7971-05-31T10")
    expect_equal(format(clock::as_naive_time(dtl)), "12016-02-19T10")

    dt <- as_datetimeoffset("2020-03-23T04:04:04.02Z")
    nt <- clock::as_naive_time(dt)
    expect_equal(format(nt), "2020-03-23T04:04:04.020")

    ymd <- clock::year_month_day(c(1984L, NA_integer_), 10L, 10L)
    nt <- clock::as_naive_time(ymd)
    expect_equal(is.na(as_datetimeoffset(nt)), c(FALSE, TRUE))
    expect_equal(format(as_datetimeoffset(nt)), c("1984-10-10", NA_character_))

    dt <- as_datetimeoffset("2020-03-01")
    expect_equal(format(clock::as_year_month_day(dt)),
                 "2020-03-01")
    expect_equal(format(clock::as_year_month_weekday(dt)),
                 "2020-03-Sun[1]")
    expect_equal(format(clock::as_iso_year_week_day(dt)),
                 "2020-W09-7")
    expect_equal(format(clock::as_year_quarter_day(dt)),
                 "2020-Q1-61")
    expect_equal(format(clock::as_year_day(dt)),
                 "2020-061")
    expect_equal(format(clock::as_weekday(dt)), "Sun")

    dt <- as_datetimeoffset("2020-03-23T04:04:04.123456789Z")
    ymd <- clock::as_year_month_day(dt)
    expect_equal(format(ymd), "2020-03-23T04:04:04.123456789")
    st <- clock::as_sys_time(dt)
    expect_equal(format(st), "2020-03-23T04:04:04.123456789")

    skip_if_not(all(c("America/Los_Angeles") %in% OlsonNames()))
    dts <- as_datetimeoffset(c("2000-01-02",
                               "2000-01-02T03",
                               "2000-01-02T03[America/Los_Angeles]",
                               "2000-01-02T03:04",
                               "2000-01-02T03:04-02",
                               "2000-01-02T03:04-02:00",
                               "2000-01-02T03:04:05",
                               "2000-01-02T03:04:05-02",
                               "2000-01-02T03:04:05-02:00",
                               "2000-01-02T03:04:05.006[America/Los_Angeles]",
                               "2000-01-02T03:04:05.006-02",
                               "2000-01-02T03:04:05.006-02:00",
                               "", "1984"))
    dts <- fill_tz(dts, "GMT")
    expect_equal(format(clock::as_sys_time(dts[1])),  "2000-01-02")
    expect_equal(format(clock::as_sys_time(dts[2])),  "2000-01-02T03")
    expect_equal(format(clock::as_sys_time(dts[3])),  "2000-01-02T11")
    expect_equal(format(clock::as_sys_time(dts[4])),  "2000-01-02T03:04")
    expect_equal(format(clock::as_sys_time(dts[5])),  "2000-01-02T05:04")
    expect_equal(format(clock::as_sys_time(dts[6])),  "2000-01-02T05:04")
    expect_equal(format(clock::as_sys_time(dts[7])),  "2000-01-02T03:04:05")
    expect_equal(format(clock::as_sys_time(dts[8])),  "2000-01-02T05:04:05")
    expect_equal(format(clock::as_sys_time(dts[9])),  "2000-01-02T05:04:05")
    expect_equal(format(clock::as_sys_time(dts[10])), "2000-01-02T11:04:05.006")
    expect_equal(format(clock::as_sys_time(dts[11])), "2000-01-02T05:04:05.006")
    expect_equal(format(clock::as_sys_time(dts[12])), "2000-01-02T05:04:05.006")
    expect_equal(is.na(clock::as_sys_time(dts[13:14])), c(TRUE, TRUE))

    st <- clock::as_sys_time(dts[3])
    expect_equal(format(as_datetimeoffset(st)), "2000-01-02T11Z")
    expect_equal(format(clock::as_sys_time(dtn)), "-7971-05-31T10")
    expect_equal(format(clock::as_sys_time(dtl)), "12016-02-19T10")

    expect_equal(format(clock::as_zoned_time(dts[3])),
                 "2000-01-02T03:00:00-08:00[America/Los_Angeles]")
    expect_equal(format(clock::as_zoned_time(dts[3], "GMT")),
                 "2000-01-02T11:00:00+00:00[GMT]")
    expect_true(is.na(clock::as_zoned_time(NA_datetimeoffset_, "GMT")))
    expect_equal(format(clock::as_zoned_time(dtn)), "-7971-05-31T10:00:00+00:00[GMT]")
    expect_equal(format(clock::as_zoned_time(dtl)), "12016-02-19T10:00:00+00:00[GMT]")

    dts <- as_datetimeoffset(c("2000-01-02T03:04:05.1234",
                               "2000-01-02T03:04:05.1234[America/Los_Angeles]",
                               "2000-01-02T03:04:05.1234-02:00"))
    st <- clock::as_sys_time(dts)
    expect_equal(format(st),
                 c(NA_character_, "2000-01-02T11:04:05.123400", "2000-01-02T05:04:05.123400"))
})
