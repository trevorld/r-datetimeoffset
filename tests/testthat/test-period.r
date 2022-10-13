test_that("periods", {
    skip_if_not(all(c("America/Chicago", "America/Los_Angeles") %in% OlsonNames()))
    boundary <- as_datetimeoffset("2009-03-08 01:59:59",
                                  tz = c("America/Chicago", "America/Los_Angeles"))
    lp <- lubridate::days(1)
    expect_equal(format((lp + (boundary - lp)) + lp),
                 c("2009-03-09T01:59:59.0-05:00[America/Chicago]",
                   "2009-03-09T01:59:59.0-07:00[America/Los_Angeles]"))

    np <- nanotime::nanoperiod(day = 1)
    expect_equal(format((np + (boundary - np)) + np),
                 c("2009-03-09T01:59:59.0-05:00[America/Chicago]",
                   "2009-03-09T01:59:59.0-07:00[America/Los_Angeles]"))
})
