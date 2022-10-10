test_that("format.datetime_offset()", {
    expect_equal(format(datetime_offset(2020)),
                 "2020")
    expect_equal(format(datetime_offset(2020, 5)),
                 "2020-05")
    expect_equal(format(datetime_offset(2020, 5, 15)),
                 "2020-05-15")
    expect_equal(format(datetime_offset(2020, 5, 15, 8)),
                 "2020-05-15T08")
    expect_equal(format(datetime_offset(2020, 5, 15, 8, 23)),
                 "2020-05-15T08:23")
    expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16)),
                 "2020-05-15T08:23:16")
    if ("US/Pacific" %in% OlsonNames())
        expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16, tz = "US/Pacific")),
                     "2020-05-15T08:23:16-07:00")
    if ("US/Eastern" %in% OlsonNames())
        expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16, tz = "US/Eastern")),
                     "2020-05-15T08:23:16-04:00")
    expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16, tz = "GMT")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16, -7)),
                 "2020-05-15T08:23:16-07")
    expect_equal(format(datetime_offset(2020, 5, 15, 8, 23, 16, -7, 0)),
                 "2020-05-15T08:23:16-07:00")
})
