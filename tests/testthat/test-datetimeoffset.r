test_that("enforce limits on ranges", {
    expect_equal(get_year(datetimeoffset(-50)),  -50L)
    expect_equal(get_year(datetimeoffset(99999)),  99999L)
    expect_error(get_month(datetimeoffset(1900, 0)))
    expect_error(get_month(datetimeoffset(1900, -5)))
    expect_error(get_month(datetimeoffset(1900, 13)))
    expect_error(get_month(datetimeoffset(1900, 10, 0)))
    expect_error(get_month(datetimeoffset(1900, 10, -05)))
    expect_error(get_month(datetimeoffset(1900, 10, 32)))
})

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
    expect_equal(is.na(datetimeoffset(c(1980, NA, NA), c(NA, NA, 10))),
                 c(FALSE, TRUE, FALSE))
    expect_true(datetimeoffset(1980, 10, 10) == datetimeoffset(1980, 10, 10))
    expect_true(datetimeoffset(1980) < datetimeoffset(1981))
    expect_true(datetimeoffset(1980) < datetimeoffset(NA_integer_))
    expect_true(datetimeoffset(1980, 10) < datetimeoffset(1981, 10))
    expect_true(datetimeoffset(1980, 10) < datetimeoffset(1980, 11))

    skip_if_not(all(c("America/Los_Angeles", "America/New_York") %in% OlsonNames()))
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "America/Los_Angeles")),
                 "2020-05-15T08:23:16-07:00[America/Los_Angeles]")
    expect_equal(format(datetimeoffset(2020, 5, 15, 8, 23, 16, tz = "America/New_York")),
                 "2020-05-15T08:23:16-04:00[America/New_York]")

    dton <- datetimeoffset_now()
    expect_true(is_datetimeoffset(dton))
    expect_equal(clean_tz(get_tz(dton)), Sys.timezone())
})
