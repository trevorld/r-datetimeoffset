test_that("detect_invalid()", {
    dts <- c("2019-04-30T03:30:00", "2019-04-31T02:30:00")
    dts <- as_datetimeoffset(dts)
    expect_equal(clock::invalid_detect(dts), c(FALSE, TRUE))

    expect_true(clock::invalid_any(dts), TRUE)
    expect_equal(clock::invalid_count(dts), 1L)
    expect_equal(clock::invalid_remove(dts), dts[1])

    # non-existent due to DST
    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    dt <- as_datetimeoffset("2020-03-08T02:59:59[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T02:59:59-05[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59[America/New_York]")
    expect_false(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/New_York]")
    expect_true(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08[America/Los_Angeles]")
    expect_false(clock::invalid_detect(dt))
    dt <- as_datetimeoffset("2020-03-08T01:59:59-08:30[America/Los_Angeles]")
    expect_true(clock::invalid_detect(dt))
})
