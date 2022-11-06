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

    expect_equal(get_tz(Sys.time()), "")

    expect_error(set_hour_offset("Boo", 0L))
    expect_error(set_minute_offset("Boo", 0L))
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
