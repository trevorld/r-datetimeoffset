test_that("getters", {
    dt <- as_datetimeoffset("1918-11-11T11:11:11")
    expect_equal(get_year(dt), 1918L)
    expect_equal(get_month(dt), 11L)
    expect_equal(get_day(dt), 11L)
    expect_equal(get_hour(dt), 11L)
    expect_equal(get_minute(dt), 11L)
    expect_equal(get_second(dt), 11L)
    expect_equal(get_nanosecond(dt), NA_integer_)
    expect_equal(get_hour_offset(dt), NA_integer_)
    expect_equal(get_minute_offset(dt), NA_integer_)
    expect_equal(get_tz(dt), NA_character_)

    skip_if_not("Europe/Paris" %in% OlsonNames())
    dts <- "1918-11-11T11:11:11+00:00[Europe/Paris]"
    dt <- as_datetimeoffset(dts)
    expect_equal(get_hour_offset(dt), 0L)
    expect_equal(get_minute_offset(dt), 0L)
    expect_equal(get_tz(dt), "Europe/Paris")
    expect_equal(get_tz(clock::zoned_time_now(Sys.timezone())), Sys.timezone())

    expect_equal(get_hour_offset(dts), 0L)
    expect_equal(get_minute_offset(dts), 0L)

    expect_equal(get_tz(Sys.Date()), "UTC")

    skip_if_not_installed("lubridate")
    expect_equal(lubridate::tz(dt), "Europe/Paris")
    expect_equal(lubridate::year(dt), 1918L)
    expect_equal(lubridate::month(dt), 11L)
    expect_equal(as.character(lubridate::month(dt, label = TRUE)),
                 "Nov")
    expect_equal(as.character(lubridate::month(dt, label = TRUE, abbr = FALSE)),
                 "November")
    expect_equal(lubridate::day(dt), 11L)
    expect_equal(lubridate::hour(dt), 11L)
    expect_equal(lubridate::minute(dt), 11L)
    expect_equal(lubridate::second(dt), 11L)
    expect_equal(lubridate::date(dt), as.Date(dt))
})

test_that("setters", {
    dt <- NA_datetimeoffset_
    dt <- set_year(dt, 1918L, na_set = TRUE)
    dt <- set_month(dt, 11L)
    expect_equal(get_day(set_day(dt, "last")), 30L)
    expect_equal(get_day(set_day(datetimeoffset(2001, NA_integer_), "last")),
                 NA_integer_)
    dt <- set_day(dt, 11L)
    dt <- set_hour(dt, 11L)
    dt <- set_minute(dt, 11L)
    dt <- set_second(dt, 11L)
    dt <- set_nanosecond(dt, NA_integer_)
    dt <- set_tz(dt, "Europe/Paris")
    dt <- set_hour_offset(dt, 0L)
    dt <- set_minute_offset(dt, 0L)
    expect_equal(format(dt), "1918-11-11T11:11:11+00:00[Europe/Paris]")

    dt <- set_nanosecond(dt, 123456789)
    dt <- set_subsecond_digits(dt, 4L)
    expect_equal(format(dt), "1918-11-11T11:11:11.1234+00:00[Europe/Paris]")

    expect_equal(get_subsecond(dt, 1L), 1L)
    expect_equal(get_subsecond(dt, 2L), 12L)
    expect_equal(get_subsecond(dt, 3L), 123L)
    expect_equal(get_subsecond(dt, 9L), 123456789L)

    expect_equal(get_subsecond_digits(Sys.time()), 6L)

    dt <- as_datetimeoffset("2020-01-01T10:10:10")
    dts <- datetime_widen(dt, precision = c("decisecond", "centisecond", "millisecond",
                                            "hundred microseconds", "ten microseconds", "microsecond",
                                            "hundred nanoseconds", "ten nanoseconds", "nanosecond"))
    expect_equal(format(dts),
                 c("2020-01-01T10:10:10.0",
                   "2020-01-01T10:10:10.00",
                   "2020-01-01T10:10:10.000",
                   "2020-01-01T10:10:10.0000",
                   "2020-01-01T10:10:10.00000",
                   "2020-01-01T10:10:10.000000",
                   "2020-01-01T10:10:10.0000000",
                   "2020-01-01T10:10:10.00000000",
                   "2020-01-01T10:10:10.000000000"))

    expect_equal(get_tz(Sys.time()), "")

    expect_error(set_hour_offset("Boo", 0L))
    expect_error(set_minute_offset("Boo", 0L))

    dt <- as_datetimeoffset("1918-11-11T11:11:11.123456789")
    expect_equal(get_millisecond(dt), 123L)
    expect_equal(get_microsecond(dt), 123456L)
    dt <- set_millisecond(dt, 123L)
    expect_equal(format(dt), "1918-11-11T11:11:11.123")
    dt <- set_microsecond(dt, 123456L)
    expect_equal(format(dt), "1918-11-11T11:11:11.123456")

    dt <- set_subsecond(dt, 1234, 4L)
    expect_equal(format(dt), "1918-11-11T11:11:11.1234")
})

test_that("setters for {clock}", {
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    ymd <- clock::year_month_day(2020, 10, 10, 10, 10, 10)
    nt <- clock::as_naive_time(ymd)
    zt <- clock::as_zoned_time(nt, "America/Los_Angeles")
    zt2 <- set_tz(zt, "America/New_York")
    expect_equal(format(zt2), "2020-10-10T10:10:10-04:00[America/New_York]")
})

test_that("setters for {lubridate}", {
    skip_if_not_installed("lubridate")
    dt <- set_tz(Sys.time(), "GMT")
    expect_equal(get_tz(dt), "GMT")
    expect_equal(get_hour_offset(dt), 0L)
    expect_equal(get_minute_offset(dt), 0L)

    dt <- datetimeoffset(2000L)
    lubridate::year(dt) <- 1918L
    lubridate::month(dt) <- 11L
    lubridate::day(dt) <- 11L
    lubridate::hour(dt) <- 11L
    lubridate::minute(dt) <- 11L
    lubridate::second(dt) <- 11L
    lubridate::date(dt) <- as.Date("1918-11-11")
    dt <- set_nanosecond(dt, NA_integer_)
    dt <- set_tz(dt, "Europe/Paris")
    dt <- set_hour_offset(dt, 0L)
    dt <- set_minute_offset(dt, 0L)
    expect_equal(format(dt), "1918-11-11T11:11:11+00:00[Europe/Paris]")

    skip_if_not_installed("lubridate", "1.8.0.9000") # generic `force_tz()`
    lubridate::tz(dt) <- "Europe/Paris"
    expect_equal(format(dt), "1918-11-11T11:11:11+00:00[Europe/Paris]")
})
