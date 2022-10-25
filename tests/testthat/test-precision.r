test_that("precision", {
    library("clock", exclude = c("calendar_narrow", "calendar_widen"))
    dts <- as_datetimeoffset(c("2020", "2020-04-10", "2020-04-10T10:10"))
    expect_equal(calendar_precision(dts),
                 c("year", "day", "minute"))
    dtn <- calendar_narrow(dts, "day")
    expect_equal(format(dtn), c("2020", "2020-04-10", "2020-04-10"))

    dtw <- calendar_widen(dts, "day")
    expect_equal(format(dtw), c("2020-01-01", "2020-04-10", "2020-04-10T10:10"))
})