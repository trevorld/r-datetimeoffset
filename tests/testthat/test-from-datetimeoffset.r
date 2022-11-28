test_that("as.Date()", {
    expect_equal(as.Date(as_datetimeoffset(c("2020-03-23T10:10:10", "2020-03-23", ""))),
                 as.Date(c("2020-03-23", "2020-03-23", NA_character_)))
    expect_equal(as_date(as_datetimeoffset(c("2020-03-23T10:10:10", "2020-03-23", ""))),
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

test_that("as.POSIXct()", {
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(as.POSIXct(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(as_date_time(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
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
})

test_that("clock classes", {
    dt <- as_datetimeoffset("2020-03-23T04:04:04Z")

    ymd <- as_year_month_day(dt)
    expect_equal(format(ymd), "2020-03-23T04:04:04")
    expect_equal(format(as_datetimeoffset(ymd)),
                 "2020-03-23T04:04:04")
    ymd <- clock::year_month_day(c(1984L, NA_integer_))
    expect_equal(is.na(as_datetimeoffset(ymd)), c(FALSE, TRUE))
    expect_equal(format(as_datetimeoffset(ymd)), c("1984", NA_character_))

    ymw <- as_year_month_weekday(dt)
    expect_equal(format(ymw), "2020-03-Mon[4]T04:04:04")
    expect_equal(format(as_datetimeoffset(ymw)),
                 "2020-03-23T04:04:04")

    ywd <- as_iso_year_week_day(dt)
    expect_equal(format(ywd), "2020-W13-1T04:04:04")
    expect_equal(format(as_datetimeoffset(ywd)),
                 "2020-03-23T04:04:04")

    yqd <- as_year_quarter_day(dt)
    expect_equal(format(yqd), "2020-Q1-83T04:04:04")
    expect_equal(format(as_datetimeoffset(yqd)),
                 "2020-03-23T04:04:04")

    yd <- as_year_day(dt)
    expect_equal(format(yd), "2020-083T04:04:04")
    expect_equal(format(as_datetimeoffset(yd)),
                 "2020-03-23T04:04:04")

    nt <- as_naive_time(dt)
    expect_equal(format(nt), "2020-03-23T04:04:04")
    expect_equal(format(as_datetimeoffset(nt)),
                 "2020-03-23T04:04:04")

    dt <- as_datetimeoffset("2020-03-23T04:04:04.02Z")
    nt <- as_naive_time(dt)
    expect_equal(format(nt), "2020-03-23T04:04:04.020")

    ymd <- clock::year_month_day(c(1984L, NA_integer_), 10L, 10L)
    nt <- as_naive_time(ymd)
    expect_equal(is.na(as_datetimeoffset(nt)), c(FALSE, TRUE))
    expect_equal(format(as_datetimeoffset(nt)), c("1984-10-10", NA_character_))

    dt <- as_datetimeoffset("2020-03-01")
    expect_equal(format(as_year_month_day(dt)),
                 "2020-03-01")
    expect_equal(format(as_year_month_weekday(dt)),
                 "2020-03-Sun[1]")
    expect_equal(format(as_iso_year_week_day(dt)),
                 "2020-W09-7")
    expect_equal(format(as_year_quarter_day(dt)),
                 "2020-Q1-61")
    expect_equal(format(as_year_day(dt)),
                 "2020-061")
    expect_equal(format(as_weekday(dt)), "Sun")

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

    expect_equal(format(as_zoned_time(dts[3])),
                 "2000-01-02T03:00:00-08:00[America/Los_Angeles]")
    expect_equal(format(as_zoned_time(dts[3], "GMT")),
                 "2000-01-02T11:00:00+00:00[GMT]")
    expect_true(is.na(as_zoned_time(NA_datetimeoffset_, "GMT")))

    dts <- as_datetimeoffset(c("2000-01-02T03:04:05.1234",
                               "2000-01-02T03:04:05.1234[America/Los_Angeles]",
                               "2000-01-02T03:04:05.1234-02:00"))
    st <- clock::as_sys_time(dts)
    expect_equal(format(st),
                 c(NA_character_, "2000-01-02T11:04:05.123400", "2000-01-02T05:04:05.123400"))
})
