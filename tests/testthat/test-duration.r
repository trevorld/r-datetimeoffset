test_that("durations", {
    skip_if_not("America/Chicago" %in% OlsonNames())

    boundary <- as_datetimeoffset("2009-03-08 01:59:59", tz = "America/Chicago")
    ld <- lubridate::ddays(1)
    expect_equal(format((ld + (boundary - ld)) + ld),
                 "2009-03-09T02:59:59.0-05:00[America/Chicago]")

    expect_equal(format((8L + (boundary - 8L)) + 8L),
                 "2009-03-08T01:59:59.000000008-06:00[America/Chicago]")

    expect_error(boundary + "foobar")
    expect_error(boundary + boundary)
    expect_error(boundary * 2)
    expect_error(2 * boundary)

    nd <- nanotime::nanoduration(hour = 24, minute = 0, second = 0, nanosecond = 0)
    expect_equal(format((nd + (boundary - nd)) + nd),
                 "2009-03-09T02:59:59.0-05:00[America/Chicago]")

    expect_equal(boundary - boundary, nanotime::nanoduration(0, 0, 0, 0))

    dd <- as.difftime(1, units = "days")
    dt <- vec_arith("+", vec_arith("+", dd,
                                   vec_arith("-", boundary, dd)),
                    dd)
    expect_equal(format(dt), "2009-03-09T02:59:59.0-05:00[America/Chicago]")
    expect_error(vec_arith("*", boundary, dd))
    expect_error(vec_arith("-", dd, boundary))
})
