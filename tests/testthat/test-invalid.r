test_that("detect_invalid()", {
    dts <- c("2019-04-30T03:30:00", "2019-04-31T02:30:00")
    dts <- as_datetimeoffset(dts)
    expect_equal(clock::invalid_detect(dts), c(FALSE, TRUE))

    expect_true(clock::invalid_any(dts), TRUE)
    expect_equal(clock::invalid_count(dts), 1L)
    expect_equal(clock::invalid_remove(dts), dts[1])

    expect_equal(format(clock::invalid_resolve(dts)),
                 c("2019-04-30T03:30:00", NA_character_))
    expect_equal(format(clock::invalid_resolve(dts, invalid = "previous")),
                 c("2019-04-30T03:30:00", "2019-04-30T23:59:59"))
    expect_equal(format(clock::invalid_resolve(dts, invalid = "previous-day")),
                 c("2019-04-30T03:30:00", "2019-04-30T02:30:00"))

    # non-existent due to DST
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset("2020-03-08T02:59:59[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    expect_equal(clock::invalid_resolve(dt), NA_datetimeoffset_)
    expect_equal(clock::invalid_resolve(dt, nonexistent = "roll-forward"),
                 as_datetimeoffset("2020-03-08T03:00:00-04:00[America/New_York]"))
    expect_equal(clock::invalid_resolve(dt, nonexistent = "roll-backward"),
                 as_datetimeoffset("2020-03-08T01:59:59-05:00[America/New_York]"))

    dt <- as_datetimeoffset("2020-03-08T02:59:59-05[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59[America/New_York]")
    expect_false(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/Los_Angeles]")
    expect_false(clock::invalid_detect(dt))

    # invalid UTC offset
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08:30[America/Los_Angeles]")
    expect_true(clock::invalid_detect(dt))
    expect_equal(format(clock::invalid_resolve(dt)),
                 "2020-03-08T01:59:59-08:00[America/Los_Angeles]")

})
