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
