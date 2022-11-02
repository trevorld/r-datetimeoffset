test_that("format.datetimeoffset()", {
    expect_length(NA_datetimeoffset_, 1L)
    expect_length(datetimeoffset(2020), 1L)
    expect_length(datetimeoffset(), 0L)
    expect_equal(format(datetimeoffset(2020)),
                 "2020")
    expect_equal(format(datetimeoffset(2020, 5)),
                 "2020-05")
    expect_equal(format(datetimeoffset(2020, 5, 15)),
                 "2020-05-15")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8)),
                 "2020-05-15T08")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23)),
                 "2020-05-15T08:23")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16)),
                 "2020-05-15T08:23:16")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "GMT")),
                 "2020-05-15T08:23:16Z")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16,
                                        hour_offset = -7)),
                 "2020-05-15T08:23:16-07")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16,
                                       hour_offset = -7, minute_offset = 0)),
                 "2020-05-15T08:23:16-07:00")

    dt <- datetimeoffset(2020, 5, 15, 8, 23, 16, 200)
    expect_true(is_datetimeoffset(dt))
    expect_equal(format(dt), "2020-05-15T08:23:16.0000002")
    expect_equal(vec_ptype_abbr(dt), "dto")
    expect_equal(vec_ptype_full(dt), "datetimeoffset")
    expect_false(is.na(dt))

    expect_true(is.na(NA_datetimeoffset_))

    skip_if_not(all(c("US/Pacific", "US/Eastern") %in% OlsonNames()))
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "US/Pacific")),
                 "2020-05-15T08:23:16-07:00[US/Pacific]")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "US/Eastern")),
                 "2020-05-15T08:23:16-04:00[US/Eastern]")
})
