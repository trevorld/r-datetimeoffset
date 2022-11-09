test_that("base extractors functions", {
    dto <- as_datetimeoffset(c("1970-01-01", "2013-11-23"))
    expect_equal(weekdays(dto), c("Thursday", "Saturday"))
    expect_equal(months(dto, FALSE), c("January", "November"))
    expect_equal(months(dto, TRUE), c("Jan", "Nov"))
    expect_equal(quarters(dto), c("Q1", "Q4"))
    expect_equal(as.numeric(julian(dto)), c(0, 16032))
})

test_that("lubridate extractors functions", {
    skip_if_not_installed("lubridate")
    dto <- as_datetimeoffset(c("1970-01-01", "2013-11-23"))
    expect_equal(lubridate::isoyear(dto), c(1970, 2013))
    expect_equal(lubridate::epiyear(dto), c(1969, 2013))
    expect_equal(lubridate::quarter(dto), c(1L, 4L))
    expect_equal(lubridate::semester(dto), c(1L, 2L))
    expect_equal(lubridate::week(dto), c(1, 47))
    expect_equal(lubridate::isoweek(dto), c(1, 47))
    expect_equal(lubridate::epiweek(dto), c(53, 47))
    expect_equal(lubridate::wday(dto), c(5, 7))
    expect_equal(lubridate::qday(dto), c(1, 54))
    expect_equal(lubridate::yday(dto), c(1, 327))

    dto <- as_datetimeoffset("2013-11-23T11:00:00[America/Los_Angeles]")
    expect_equal(lubridate::am(dto), TRUE)
    expect_equal(lubridate::pm(dto), FALSE)
    expect_equal(as.numeric(lubridate::days_in_month(dto)), 30)
    expect_equal(lubridate::dst(dto), FALSE)
    expect_equal(lubridate::leap_year(dto), FALSE)

    dt <- as_datetimeoffset("1970-01-01")
    lubridate::wday(dt) <- 4
    expect_equal(lubridate::wday(dt), 4)
    lubridate::yday(dt) <- 5
    expect_equal(lubridate::yday(dt), 5)
    lubridate::qday(dt) <- 5
    expect_equal(lubridate::qday(dt), 5)
})
