ls <- "2005-12-31 23:59:60"
lsz <- paste0(ls, "Z")

test_that("leap seconds work", {
    dt <- as_datetimeoffset.character(lsz)
    expect_equal(format(dt), "2005-12-31T23:59:60Z")

    expect_false(clock::invalid_detect(dt))
})

test_that("convert to/from POSIXt classes", {
    skip_on_cran()
    skip_if_not(all(c("America/Los_Angeles", "UTC") %in% OlsonNames()))

    # from POSIXt classes
    dt_ct <- as.POSIXct(ls, tz = "UTC")
    dt <- as_datetimeoffset(dt_ct)
    expect_equal(format(dt), "2006-01-01T00:00:00.000000Z")

    dt_lt <- as.POSIXlt(ls, tz = "UTC")
    dt <- as_datetimeoffset(dt_lt)
    expect_equal(format(dt), "2005-12-31T23:59:60.000000Z")

    dt_lt <- as.POSIXlt(ls, tz = "America/Los_Angeles")
    dt <- as_datetimeoffset(dt_lt)
    expect_equal(format(dt), "2005-12-31T23:59:60.000000-08:00[America/Los_Angeles]")

    # to POSIXt classes
    dt <- as_datetimeoffset.character(lsz)
    dt_lt <- as.POSIXlt(dt)
    expect_equal(format(dt_lt, format = "%F %T"),
                 "2005-12-31 23:59:60")

    dt <- as_datetimeoffset.character(ls, tz = "America/Los_Angeles")
    dt_lt <- as.POSIXlt(dt, tz = "UTC")
    expect_equal(format(dt_lt, format = "%F %T"),
                 "2006-01-01 08:00:00")

    dt <- as_datetimeoffset.character(paste0(ls, "-08:00"))
    dt_lt <- as.POSIXlt(dt, tz = "UTC")
    expect_equal(format(dt_lt, format = "%F %T"),
                 "2006-01-01 08:00:00")

    dt <- as_datetimeoffset.character(lsz)
    dt_ct <- as.POSIXct(dt)
    expect_equal(format(dt_ct, format = "%F %T"),
                 "2006-01-01 00:00:00")
})

test_that("convert to {nanotime}", {
    skip_if_not_installed("nanotime")
    dt <- as_datetimeoffset.character(lsz)
    nt <- nanotime::as.nanotime(dt)
    expect_equal(format(nt), "2006-01-01T00:00:00+00:00")
})

test_that("convert to {clock}", {
    dt <- as_datetimeoffset.character(lsz)

    # since `{clock}` doesn't support leap seconds we use the next second
    # like `as.nanotime()` and `as.POSIXct()`
    ymd <- as_year_month_day.datetimeoffset(dt)
    expect_equal(format(ymd), "2006-01-01T00:00:00")

    nt <- as_naive_time.datetimeoffset(dt)
    expect_equal(format(nt), "2006-01-01T00:00:00")

    st <- as_sys_time(dt)
    expect_equal(format(st), "2006-01-01T00:00:00")

    zt <- as_zoned_time.datetimeoffset(dt)
    expect_equal(format(zt), "2006-01-01T00:00:00+00:00[GMT]")
})
