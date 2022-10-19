test_that("as.Date()", {
    expect_equal(as.Date(as_datetimeoffset("2020")),
                 as.Date("2020-01-01"))
    expect_equal(as.Date(as_datetimeoffset(""), year = 2020, month = 6, day = 15),
                 as.Date("2020-06-15"))
    expect_equal(as.Date(as_datetimeoffset("2020-03-23")),
                 as.Date("2020-03-23"))
    expect_equal(as_date(as_datetimeoffset("2020-03-23")),
                 as.Date("2020-03-23"))
})

test_that("as.nanotime()", {
    expect_equal(as.nanotime(as_datetimeoffset("2020-03-23T04:04:04Z")),
                 as.nanotime("2020-03-23T04:04:04Z"))
    expect_equal(as.nanotime(as_datetimeoffset("2020-03-23")),
                 as.nanotime("2020-03-23T00:00:00Z"))
})

test_that("as.POSIXct()", {
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(as.POSIXct(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
    expect_equal(format(as.POSIXct("2020-03-23 04:04:04")),
                 format(as_date_time(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
})

test_that("as.POSIXlt()", {
    expect_equal(format(as.POSIXlt("2020-03-23 04:04:04")),
                 format(as.POSIXlt(as_datetimeoffset("2020-03-23 04:04:04", tz=""))))
})

test_that("mode_tz()", {
    expect_equal(mode_tz(as_datetimeoffset(Sys.time())),
                 Sys.timezone())
    if (all(c("US/Pacific", "US/Eastern") %in% OlsonNames())) {
      dt <- as_datetimeoffset("2020-01-01",
                               tz = c("US/Pacific", "US/Eastern"))
      expect_equal(mode_tz(dt), "US/Pacific")

      dt <- as_datetimeoffset("2020-01-01",
                               tz = c("US/Pacific", "US/Eastern", NA_character_, NA_character_))
      expect_equal(mode_tz(dt), Sys.timezone())
    }
})
