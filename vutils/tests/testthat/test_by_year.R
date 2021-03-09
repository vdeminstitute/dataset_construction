test_that("by_year", {
    # takes as input a grouped data.frame by country plus a date vetor and a function
    # output is a list of data.frames each one row for applied function.

    # First test that obs are not added with -01-01
    df <- data.frame(x = 1:3)
    v <- as.Date(c("1900-01-01", "1901-01-01", "1902-01-01"))
    out <- by_year(df, v, function(...) list(...))

    for (i in seq_along(out)) {
        g <- out[[i]]
        expect_identical(df[i,, drop = F], g[[1]])
        expect_identical(v[i], g[[2]])
    }

    # general test; na_rm = T
    df <- data.frame(value = c(1, 2, 3),
                     value2 = c(NA, 2, 3))
    v <- as.Date(c("1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 1.917808, value2 = 2.002994, year = 1900))

    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # general test; na_rm = F
    df <- data.frame(value = c(1, 2, 3),
                     value2 = c(NA, 2, 3))
    v <- as.Date(c("1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 1.917808, value2 = 2.002994, year = 1900))

    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # more years; there is a gap so the NA should not be filled!
    df <- data.frame(value = c(2, 1, 2, 3),
                     value2 = c(1, NA, 2, 3))
    v <- as.Date(c("1898-02-23", "1900-01-03", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 2, value2 = 1, year = 1898),
               data.frame(value = 1.922865, value2 = 2.002994, year = 1900))
    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # more years; fill gap from previous year, but NOT if there is already a row at -01-01
    df <- data.frame(value = c(2, 1, 2, 3),
                     value2 = c(1, NA, 2, 3))
    v <- as.Date(c("1899-12-31", "1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 2, value2 = 1, year = 1899),
               data.frame(value = 1.917808, value2 = 2.002994, year = 1900))

    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # more years; check same case as before but previous date != -12-31
    df <- data.frame(value = c(2, 1, 2, 3),
                     value2 = c(1, NA, 2, 3))
    v <- as.Date(c("1899-12-12", "1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 2, value2 = 1, year = 1899),
               data.frame(value = 1.917808, value2 = 2.002994, year = 1900))

    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # no dates at -01-01; should insert value from previous year and create -01-01
    df <- data.frame(value = c(1, 2, 3),
                     value2 = c(1, 2, 3))
    v <- as.Date(c("1899-02-23", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 1, value2 = 1, year = 1899),
               data.frame(value = 1.917808, value2 = 1.917808, year = 1900))
    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # different length between object and date vector should return error.
    df <- data.frame(value = c(1, 2),
                     value2 = c(NA, 2))
    v <- as.Date(c("1900-01-01", "1900-02-01", "1900-12-31"))
    expect_error(by_year(df, v, day_mean))

    # empty data.frame returns empty list; I might be okay with that, since the output
    # type is list anyway
    df <- data.frame()
    v <- vector('raw')
    expect_equal(by_year(df, v, day_mean), list())

    # test sorting of data.frame
    df <- data.frame(value = c(2, 1, 3),
                     value2 = c(2, NA, 3))
    v <- as.Date(c("1900-02-01" ,"1900-01-01", "1900-12-31"))
    out <- list(data.frame(value = 1.917808, value2 = 2.002994, year = 1900))
    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)

    # what if data.frame has a character column -> weird value
    df <- data.frame(v = c("1900-01-01", "1900-02-01", "1900-12-31"),
                     value = c(1, 2, 3),
                     value2 = c(NA, 2, 3), stringsAsFactors = F)
    v <- as.Date(c("1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 1.917808, value2 = 2.002994, year = 1900))
    expect_error(by_year(df, v, day_mean))

    # what if data.frame has a character column -> error
    df <- data.frame(v = c("hello", "good", "bye"),
                     value = c(1, 2, 3),
                     value2 = c(NA, 2, 3), stringsAsFactors = F)
    v <- as.Date(c("1900-01-01", "1900-02-01", "1900-12-31"))
    out <- list(data.frame(value = 1.917808, value2 = 2.002994, year = 1900))
    expect_error(by_year(df, v, day_mean))

    # NAs in the datevector -> want error
    df <- data.frame(value = c(1, 2, 3),
                     value2 = c(3, 2, 3))
    v <- as.Date(c(NA, "1900-02-01", "1900-12-31"))
    expect_error(by_year(df, v, day_mean))

    # Check if a year before 1900 is added
    df <- data.frame(value = c(1, 2, 3, 4, 5),
                     value2 = c(1, 2, 3, 4, 5), stringsAsFactors = F)
    v <- as.Date(c("1847-12-31", "1848-01-20", "1848-12-20", "1848-12-31", "1849-12-31"))
    out <- list(data.frame(value = 1, value2 = 1, year = 1847),
               data.frame(value = 1.983607, value2 = 1.983607, year = 1848),
               data.frame(value = 5, value2 = 5, year = 1849))
    expect_equal(by_year(df, v, day_mean), out, tolerance = 0.0001)
})
