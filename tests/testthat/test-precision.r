library("clock")

test_that("precision", {
    dts <- as_datetimeoffset(c("", "2020", "2020-04-10", "2020-04-10T10:10"))
    expect_equal(datetime_precision(dts),
                 c("missing", "year", "day", "minute"))
    expect_equal(datetime_precision(dts, range = TRUE),
                 c("missing", "minute"))

    dtn <- datetime_narrow(dts, "day")
    expect_equal(format(dtn), c(NA_character_, "2020", "2020-04-10", "2020-04-10"))

    dtn <- datetime_narrow(dts, "year")
    expect_equal(format(dtn), c(NA_character_, "2020", "2020", "2020"))

    ymd <- as_year_month_day(as_datetimeoffset("2020-04-10"))
    expect_equal(format(datetime_narrow(ymd, "year")), "2020")
    expect_equal(format(datetime_widen(ymd, "hour")), "2020-04-10T00")

    dtw <- datetime_widen(dts, "day")
    expect_equal(format(dtw), c(NA_character_, "2020-01-01", "2020-04-10", "2020-04-10T10:10"))
    dtw <- datetime_widen(dts, "day", na_set = TRUE)
    expect_equal(format(dtw), c("0000-01-01", "2020-01-01", "2020-04-10", "2020-04-10T10:10"))

    # vectorize precision
    dt <- as_datetimeoffset("2020-04-10T10:10:10.123456789")
    expect_equal(format(datetime_narrow(dt, c("missing", "year", "month", "day"))),
                 c(NA_character_, "2020", "2020-04", "2020-04-10"))

    dt <- NA_datetimeoffset_
    expect_equal(format(datetime_widen(dt, c("missing", "year", "month", "day"), na_set = TRUE)),
                 c(NA_character_, "0000", "0000-01", "0000-01-01"))

    # EDTF precisions
    dt <- as_datetimeoffset("2020-XX-04")
    expect_equal(datetime_precision(dt), "year")
    expect_equal(datetime_precision(dt, unspecified = TRUE), "day")
    expect_equal(format(datetime_narrow(dt, "missing")), NA_character_)

    # {clock} methods
    expect_equal(datetime_precision(clock::year_month_day(1918, 11, 11)), "day")
    expect_equal(datetime_precision(clock::sys_time_now()), "nanosecond")
    expect_equal(datetime_precision(clock::zoned_time_now(Sys.timezone())), "nanosecond")

    ymd <- year_month_day(1918, 11, 11, 11)
    expect_equal(format(datetime_narrow(ymd, "month")), "1918-11")
    expect_equal(format(datetime_narrow(ymd, "second")), "1918-11-11T11")
    expect_equal(format(datetime_widen(ymd, "second")), "1918-11-11T11:00:00")
    expect_equal(format(datetime_widen(ymd, "month")), "1918-11-11T11")

    nt <- as_naive_time(ymd)
    expect_equal(format(datetime_narrow(nt, "day")), "1918-11-11")
    expect_equal(format(datetime_narrow(nt, "second")), "1918-11-11T11")
    expect_equal(format(datetime_widen(nt, "second")), "1918-11-11T11:00:00")
    expect_equal(format(datetime_widen(nt, "month")), "1918-11-11T11")

    skip_if_not_installed("nanotime")
    expect_equal(datetime_precision(as.nanotime(Sys.time())), "nanosecond")
})
