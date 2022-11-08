test_that("base 'weekdays' functions", {
    dto <- as_datetimeoffset(c("1970-01-01", "2013-11-23"))
    expect_equal(weekdays(dto), c("Thursday", "Saturday"))
    expect_equal(months(dto, FALSE), c("January", "November"))
    expect_equal(months(dto, TRUE), c("Jan", "Nov"))
    expect_equal(quarters(dto), c("Q1", "Q4"))
    expect_equal(as.numeric(julian(dto)), c(0, 16032))
})
