library("lubridate", exclude = c("date", "force_tz", "tz<-"))

test_that("date() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(date(dt), as.Date("2020-05-15"))
    date(dt) <- as.Date("2016-11-11")
    expect_equal(date(dt), as.Date("2016-11-11"))

    date(dt) <- "2018-10-10"
    expect_equal(date(dt), as.Date("2018-10-10"))
})

test_that("year() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(year(dt), 2020L)
    year(dt) <- 2016
    expect_equal(year(dt), 2016L)
})

test_that("month() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(month(dt), 5L)
    month(dt) <- 12
    expect_equal(month(dt), 12L)

    expect_equal(format(dt), "2020-12-15T08:23:16-07:00")
    month(dt) <- NA_integer_
    expect_equal(format(dt), "2020")
})

test_that("day() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(day(dt), 15L)
    day(dt) <- 12
    expect_equal(day(dt), 12L)
})

test_that("hour() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(hour(dt), 8L)
    hour(dt) <- 12
    expect_equal(hour(dt), 12L)
})

test_that("minute() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(minute(dt), 23L)
    minute(dt) <- 12
    expect_equal(minute(dt), 12L)
})

test_that("second() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(second(dt), 16L)
    second(dt) <- 12
    expect_equal(second(dt), 12L)
})

test_that("nanosecond() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16.003-07:00")
    expect_equal(nanosecond(dt), 3000000)
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    nanosecond(dt) <- 12
    expect_equal(nanosecond(dt), 12L)

    dt <- nanotime::as.nanotime("2020-05-15T08:23:16.003-07:00")
    expect_equal(nanosecond(dt), 3000000)
    expect_error(nanosecond(dt) <- 2000000)
    expect_type(hour_offset(dt), "integer")
    expect_type(minute_offset(dt), "integer")
})

test_that("tz() methods", {
    dt <- as_datetimeoffset("2020-05-15T08:23:16-07:00")
    expect_equal(tz(dt), NA_character_)
    tz(dt) <- "GMT"
    expect_equal(tz(dt), "GMT")
})

test_that("hour_offset() methods", {
    dt <- as_datetimeoffset("2020-04-08T20:12:16")
    hour_offset(dt) <- -7L
    expect_equal(hour_offset(dt), -7L)
    minute_offset(dt) <- 30L
    expect_equal(minute_offset(dt), 30L)
    dt <- lubridate::with_tz(Sys.time(), "GMT")
    expect_equal(hour_offset(dt), 0L)
    expect_equal(minute_offset(dt), 0L)

    dt <- as.POSIXlt(dt, "GMT")
    expect_equal(hour_offset(dt), 0L)
    expect_equal(minute_offset(dt), 0L)

    expect_error(hour_offset(dt) <- 0L)
    expect_error(minute_offset(dt) <- 0L)

})

test_that("force_tz() and with_tz()", {
    dt <- as_datetimeoffset("1918-11-11T11:11:11", tz = "GMT")
    if ("US/Pacific" %in% OlsonNames()) {
        expect_equal(format(force_tz(dt, "US/Pacific")),
                     "1918-11-11T11:11:11-08:00[US/Pacific]")
        expect_equal(format(with_tz(dt, "US/Pacific")),
                     "1918-11-11T03:11:11.0-08:00[US/Pacific]")
    }

    dt <- Sys.time()
    expect_equal(tz(with_tz(dt, Sys.timezone())), Sys.timezone())
    expect_equal(tz(force_tz(dt, Sys.timezone())), Sys.timezone())
})
