test_that("as.Date()", {
    expect_equal(as.Date(as_datetime_offset("2020")),
                 as.Date("2020-01-01"))
    expect_equal(as.Date(as_datetime_offset("2020"), 6, 15),
                 as.Date("2020-06-15"))
    expect_equal(as.Date(as_datetime_offset("2020-03-23")),
                 as.Date("2020-03-23"))
})
