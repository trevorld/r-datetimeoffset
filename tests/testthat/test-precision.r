test_that("precision", {
    library("clock", exclude = c("calendar_narrow", "calendar_precision", "calendar_widen"))
    dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
    expect_equal(calendar_precision(dts),
                 c("year", "day", "minute"))
    expect_equal(calendar_precision(dts, range = TRUE),
                 c("year", "minute"))

    dtn <- calendar_narrow(dts, "day")
    expect_equal(format(dtn), c("2020", "2020-04-10", "2020-04-10"))

    dtn <- calendar_narrow(dts, "year")
    expect_equal(format(dtn), c("2020", "2020", "2020"))

    ymd <- as_year_month_day(as_datetimeoffset("2020-04-10"))
    expect_equal(format(calendar_narrow(ymd, "year")), "2020")
    expect_equal(format(calendar_widen(ymd, "hour")), "2020-04-10T00")

    dtw <- calendar_widen(dts, "day")
    expect_equal(format(dtw), c("2020-01-01", "2020-04-10", "2020-04-10T10:10"))
})
