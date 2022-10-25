test_that("getters", {
    dt <- as_datetimeoffset("1918-11-11T11:11:11")
    expect_equal(get_year(dt), 1918L)
    expect_equal(get_month(dt), 11L)
    expect_equal(get_day(dt), 11L)
    expect_equal(get_hour(dt), 11L)
    expect_equal(get_minute(dt), 11L)
    expect_equal(get_second(dt), 11L)
    expect_equal(get_nanosecond(dt), NA_integer_)
    expect_equal(get_zone(dt), NA_character_)
    expect_equal(get_hour_offset(dt), NA_integer_)
    expect_equal(get_minute_offset(dt), NA_integer_)

    skip_if_not("Europe/Paris" %in% OlsonNames())
    dts <- "1918-11-11T11:11:11+00:00[Europe/Paris]"
    dt <- as_datetimeoffset(dts)
    expect_equal(get_zone(dt), "Europe/Paris")
    expect_equal(get_hour_offset(dt), 0L)
    expect_equal(get_minute_offset(dt), 0L)

    expect_equal(get_hour_offset(dts), 0L)
    expect_equal(get_minute_offset(dts), 0L)

    expect_error(get_zone(dts))
})

test_that("setters", {
    dt <- NA_datetimeoffset_
    dt <- set_year(dt, 1918L)
    dt <- set_month(dt, 11L)
    dt <- set_day(dt, 11L)
    dt <- set_hour(dt, 11L)
    dt <- set_minute(dt, 11L)
    dt <- set_second(dt, 11L)
    dt <- set_nanosecond(dt, NA_integer_)
    dt <- set_zone(dt, "Europe/Paris")
    dt <- set_hour_offset(dt, 0L)
    dt <- set_minute_offset(dt, 0L)
    expect_equal(format(dt), "1918-11-11T11:11:11+00:00[Europe/Paris]")

    dt <- set_zone(Sys.time(), "GMT")
    expect_equal(get_zone(dt), "GMT")
    expect_equal(get_hour_offset(dt), 0L)
    expect_equal(get_minute_offset(dt), 0L)

    expect_equal(get_zone(Sys.time()), Sys.timezone())
    expect_error(set_hour_offset("Boo", 0L))
    expect_error(set_minute_offset("Boo", 0L))
})
