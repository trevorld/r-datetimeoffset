test_that("mode_tz()", {
    expect_equal(mode_tz(as_datetimeoffset(Sys.time())),
                 Sys.timezone())
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset("2020-01-01",
                            tz = c("America/Los_Angeles", "America/New_York"))
    expect_equal(mode_tz(dt), "America/Los_Angeles")

    dt <- as_datetimeoffset("2020-01-01",
                            tz = c("America/Los_Angeles", "America/New_York", NA_character_, NA_character_))
    expect_equal(mode_tz(dt), Sys.timezone())

    expect_equal(mode_tz(Sys.time()), Sys.timezone())
})

test_that("datetime_at_tz()", {
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset("2020-01-01T01:01[America/Los_Angeles]")
    expect_equal(format(datetime_at_tz(dt, "America/New_York")),
                 "2020-01-01T04:01-05:00[America/New_York]")
    expect_equal(format(datetime_at_tz(dt, c("America/Los_Angeles", "America/New_York"))),
                 c("2020-01-01T01:01-08:00[America/Los_Angeles]",
                   "2020-01-01T04:01-05:00[America/New_York]"))

    expect_equal(format(datetime_at_tz(as.POSIXct(dt), "America/New_York"), tz = "America/New_York"),
                 "2020-01-01 04:01:00")

    expect_equal(format(datetime_at_tz(as_zoned_time(dt), "America/New_York")),
                 "2020-01-01T04:01:00-05:00[America/New_York]")

    skip_if_not_installed("lubridate", "1.9.0")
    expect_equal(format(datetime_at_tz(as.Date(dt), "America/New_York")),
                 "2019-12-31 19:00:00")
    expect_equal(format(lubridate::with_tz(dt, "America/New_York")),
                 "2020-01-01T04:01-05:00[America/New_York]")

    dts <- as_datetimeoffset(c("2020-01-01T01:01[America/Los_Angeles]",
                               "2020-01-01T04:01[America/New_York]"))
    expect_equal(format(datetime_at_tz(dts, "GMT")),
                 c("2020-01-01T09:01Z", "2020-01-01T09:01Z"))
})

test_that("get_utc_offsets()", {
    dts <- as_datetimeoffset(c("2020-01-01T01:01",
                               "2020-01-01T01:01-07",
                               "2020-01-01T01:01-07:00"))
    expect_equal(get_utc_offsets(dts), c(NA_character_, "-07", "-07:00"))
    expect_equal(get_utc_offsets(dts, sep = ""), c(NA_character_, "-07", "-0700"))

    dts <- set_utc_offsets(dts, "-07:00")
    expect_equal(get_utc_offsets(dts), c("-07:00", "-07:00", "-07:00"))
    dts <- set_utc_offsets(dts, "+0800")
    expect_equal(get_utc_offsets(dts), c("+08:00", "+08:00", "+08:00"))
    dts <- set_utc_offsets(dts, "+00")
    expect_equal(get_utc_offsets(dts), c("+00", "+00", "+00"))
    dts <- set_utc_offsets(dts, NA_character_)
    expect_equal(get_utc_offsets(dts), c(NA_character_, NA_character_, NA_character_))

    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as.POSIXct("2020-01-01 10:10:10", tz = "America/Los_Angeles")
    expect_equal(get_utc_offsets(dt), "-08:00")
    expect_equal(get_utc_offsets(dt, sep = ""), "-0800")
})
