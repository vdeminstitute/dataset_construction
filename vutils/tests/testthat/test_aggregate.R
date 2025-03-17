test_that("collect_max", {
    expect_equal(collect_max(NaN), NaN)
    expect_equal(collect_max(c(NA, 1, NA)), 1)
    expect_equal(collect_max(c(NA, NA, NA)), NA)
    expect_equal(collect_max(c(NaN, NA, NA)), NaN)
    expect_equal(collect_max(c(NA, NA, NA)), NA)
    expect_equal(collect_max(c(NA, NaN, NA)), NA_real_)
    expect_equal(collect_max(NA), NA)
    expect_equal(collect_max(pi), pi)
    expect_equal(collect_max(c("1", " ", "3")), "3")
    expect_equal(collect_max(c("1", "", "3")), "3")
})

test_that("collect_last", {
    expect_equal(collect_last(NaN), NaN)
    expect_equal(collect_last(c(NA, 1, NA)), 1)
    expect_equal(collect_last(c(NA, NA, NA)), NA)
    expect_equal(collect_last(c(NaN, NA, NA)), NaN)
    expect_equal(collect_last(c(NA, NA, NaN)), NA_real_)
    expect_equal(collect_last(c(NA, NaN, NA)), NA_real_)
    expect_equal(collect_last(NA), NA)
    expect_equal(collect_last(pi), pi)
    expect_equal(collect_last(c(1, 2, 4)), 4)
    expect_equal(collect_last(c("", "3", "4")), "4")
    expect_equal(collect_last(c("1", "2", "")), "2")
    expect_equal(collect_last(c("", " ", "")), " ")
    expect_equal(collect_last(c("", "", "")), NA_character_)
})

test_that("collect_mean", {
    expect_equal(collect_mean(NaN), NaN)
    expect_equal(collect_mean(c(NA, 1, NA)), 1)
    expect_equal(collect_mean(c(NA, NA, NA)), NA)
    expect_equal(collect_mean(c(NaN, NA, NA)), NaN)
    expect_equal(collect_mean(c(NA, NA, NA)), NA)
    expect_equal(collect_mean(c(NA, NaN, NA)), NA_real_)
    expect_equal(collect_mean(NA), NA)
    expect_equal(collect_mean(pi), pi)
    expect_equal(collect_mean(c(1, 2, 3)), 2)
    expect_equal(collect_mean(c(NA, 2, 3)), 2.5)
    expect_error(collect_mean(c("1", " ", "3")))
})

test_that("by_split.data.frame", {
    x <- data.frame(idx = c(1, 1, 2, 2), x = 1:4)

    y <- data.frame(idx = c(1, 1, 2, 2), x = 1:4,
                   row.names = c("1", "2", "3", "4"))
    out <- by_split(x, x$idx, identity)
    expect_identical(out, y)

    y <- data.frame(x = c(1.5, 3.5))
    out <- by_split(x[, "x", drop = F], x$idx, function(df) {
        aggregate(x ~ ., data = df, FUN = mean)
    })
    expect_identical(out, y)

    # Fail to pass in a function
    expect_error(by_split(x, x$idx, 1))

    # Error from mc_assert
    expect_error(by_split(x, x$idx, function(df) "a" / 1))

    # Returning not a data frame will error
    expect_error(by_split(x, x$idx, function(df) T))
})

test_that("by_split.matrix", {
    x <- matrix(1:6, 3, 2, dimnames = list(letters[1:3], letters[4:5]))
    idx <- c(1, 1, 2)

    y <- matrix(1:6, 3, 2, dimnames = list(letters[1:3], letters[4:5]))
    out <- by_split(x, idx, identity)
    expect_identical(out, y)

    # Rownames will be stripped by colMeans
    y <- matrix(c(1.5, 3, 4.5, 6), 2, 2, dimnames = list(NULL, letters[4:5]))
    out <- by_split(x, idx, colMeans)
    expect_identical(out, y)

    # Error from mc_assert
    expect_error(by_split(x, idx, function(sub.ma) "a" / 1))

    # When not returning a matrix, R will coerce it. We don't force an
    # error since we want vectors to be coerced into matrices.
    out <- by_split(x, idx, function(sub.ma) "a")
    expect_identical(out, matrix(c("a", "a"), 2, 1))
})


test_that("cy.day_mean.data.frame", {
    expect_warning(cy.day_mean(data.frame(), historical_date))
    expect_equal(suppressWarnings(cy.day_mean(data.frame(), historical_date)),
                 data.frame())

    df1 <- data.frame(x = 1:3, historical_date = as.Date(c("1900-01-01", "1901-03-03", "1901-11-20")))
    df2 <- data.frame(x = 1:3, historical_date = as.Date(c("1900-01-01", "1901-03-03", "1901-11-20")))

    # We have separate unit tests for by_year and day_mean
    y <- by_year(df1[, "x", drop = F], df1$historical_date, day_mean) %>% do.call(rbind, .)

    expect_equal(cy.day_mean(df1, historical_date), y)
    expect_identical(df1, df2)

    # Note, `testthat` breaks on NSE and subsetting, so evaluate
    # cy.day_mean outside of `expect_equal`
    out <- cy.day_mean(df1[, "x", drop = F], df1$historical_date)
    expect_equal(out, y)

    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                      "1902-12-31", "1903-05-04", "1903-11-22",
                      "1903-12-10", "1904-01-01", "1904-02-02",
                      "1904-03-03"))

    df <- data.frame(x = 1:10,
                    y = 11:20,
                    historical_date = dates,
                    country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                        "MEX", "MEX", "SWE", "SWE",
                                        "SWE", "USA"),
                    stringsAsFactors = F)

    y <- data.frame(x = c(1, 2, 2.912329, 4.832877, 9.743169),
                     y = c(11, 12, 12.91233, 14.83288, 19.74317),
                     year = c(1900, 1901, 1902, 1903, 1904))

    expect_equal(cy.day_mean(df[, c("x", "y")], dates),
                 y, 1.5e-7)

    y <- data.frame(x = c(1, 2, 2.912329, 5.165289, 7, 8.912568, 10),
                   y = c(11, 12, 12.91233, 15.16529, 17, 18.91257, 20),
                   year = c(1900, 1901, 1902, 1903, 1903, 1904, 1904),
                   country_text_id = c("AFG", "AFG", "AFG", "MEX", "SWE", "SWE", "USA"),
                   stringsAsFactors = F)

    out <- cy.day_mean(df, historical_date, country_text_id)

    expect_equal(row.names(out), as.character(1:nrow(out)))
    expect_equal(out, y, 1.5e-7)

    dates <- df$historical_date
    countries <- df$country_text_id

    out <- cy.day_mean(df[, c("x", "y")], dates, countries)
    colnames(y)[4] <- "countries"
    expect_equal(out, y, 1.5e-7)

    # Multiple CPU cores
    colnames(y)[4] <- "country_text_id"
    expect_equal(cy.day_mean(df, historical_date, country_text_id, mc.cores = 2),
                 y, 1.5e-7)

    # Unordered
    set.seed(123)
    idx <- sample(1:nrow(df), nrow(df))

    expect_equal(cy.day_mean(df[idx, ], historical_date, country_text_id), y, 1.5e-7)

    # What happens when we have an NA
    df[df$country_text_id == "MEX" & df$historical_date == "1903-05-04", "x"] <- NA
    y[y$country_text_id == "MEX" & y$year == 1903, "x"] <- 6

    expect_equal(cy.day_mean(df, historical_date, country_text_id), y, 1.5e-7)

    # Error missing argument vector
    expect_error(cy.day_mean(df))
    expect_error(cy.day_mean(df, hist))
    expect_error(cy.day_mean(df, historical_date, country))
    expect_error(cy.day_mean(df, by = country_text_id))
    expect_error(cy.day_mean(df, 1:10))
    expect_error(cy.day_mean(data.frame(1:3), as.Date("1900-01-01")))
    expect_error(cy.day_mean(data.frame(1:2),
                             as.Date(c("1900-01-01", "1901-01-01")),
                             "USA"))

    # Data.frame already has a year column
    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                      "1902-12-31", "1903-05-04", "1903-11-22",
                      "1903-12-10", "1904-01-01", "1904-02-02",
                      "1904-03-03"))

    # Note the year vector is incorrect!
    df <- data.frame(x = 1:10,
                    y = 11:20,
                    historical_date = dates,
                    year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903,
                             1904, 1904, 1905),
                    country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                        "MEX", "MEX", "SWE", "SWE",
                                        "SWE", "USA"),
                    stringsAsFactors = F)

    y <- data.frame(x = c(1, 2, 2.912329, 4.832877, 9.743169),
                    y = c(11, 12, 12.91233, 14.83288, 19.74317),
                    year = c(1900, 1901, 1902, 1903, 1904))

    expect_equal(cy.day_mean(df[, c("x", "y")], dates),
                 y, 1.5e-7)

    # Dates are years should throw an error
    dates <- to_year(as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                       "1902-12-31", "1903-05-04", "1903-11-22",
                       "1903-12-10", "1904-01-01", "1904-02-02",
                       "1904-03-03")))

    # Note the year vector is incorrect!
    df <- data.frame(x = 1:10,
                     y = 11:20,
                     historical_date = dates,
                     year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903,
                              1904, 1904, 1905),
                     country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                         "MEX", "MEX", "SWE", "SWE",
                                         "SWE", "USA"),
                     stringsAsFactors = F)

    expect_error(cy.day_mean(df[, c("x", "y")], dates))

    # Next test
    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                      "1902-12-31", "1903-05-04", "1903-11-22",
                      "1903-12-10", "1904-01-01", "1904-02-02",
                      "1904-03-03"))

    df <- data.frame(code = 1:10,
                    y = 11:20,
                    historical_date = dates,
                    year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903,
                             1904, 1904, 1905),
                    country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                        "MEX", "MEX", "SWE", "SWE",
                                        "SWE", "USA"),
                    stringsAsFactors = F)

    y <- data.frame(code = c(1, 2, 2.912329, 4.832877, 9.743169),
                    y = c(11, 12, 12.91233, 14.83288, 19.74317),
                    year = c(1900, 1901, 1902, 1903, 1904))

    expect_equal(cy.day_mean(df[, c("code", "y")], dates),
                 y, 1.5e-7)


    # Next test
    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                       "1902-12-31", "1903-05-04", "1903-11-22",
                       "1903-12-10", "1904-01-01", "1904-02-02",
                       "1904-03-03"))

    df <- data.frame(x = 1:10,
                     y = 11:20,
                     historical_date = dates,
                     #year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903,
                    #          1904, 1904, 1905),
                     country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                         "MEX", "MEX", "SWE", "SWE",
                                         "SWE", "USA"),
                     stringsAsFactors = F)

    y <- data.frame(x = c(1, 2, 2.912329, 4.832877, 9.743169),
                    y = c(11, 12, 12.91233, 14.83288, 19.74317),
                    year = c(1900, 1901, 1902, 1903, 1904))

    expect_equal(cy.day_mean(df[, c("x", "y", "historical_date")],
                             historical_date),
                 y,
                 1.5e-7)
    expect_error(cy.day_mean(df[, c("x", "y", "historical_date",
                                    "country_text_id")],
                             historical_date))


    # Next test
    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                       "1902-12-31", "1903-05-04", "1903-11-22",
                       "1903-12-10", "1904-01-01", "1904-02-02",
                       "1904-03-03"))

    df <- data.frame(x = 1:10,
                    y = 11:20,
                    historical_date = dates,
                    year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903,
                            1904, 1904, 1905),
                    country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                        "MEX", "MEX", "SWE", "SWE",
                                        "SWE", "USA"),
                    stringsAsFactors = F)

    y <- data.frame(x = c(1, 2, 2.912329, 4.832877, 9.743169),
                    y = c(11, 12, 12.91233, 14.83288, 19.74317),
                    year = c(1900, 1901, 1902, 1903, 1904))

    # I want an error if there is already a year column! (we could also drop it.)
    expect_equal(cy.day_mean(df[, c("x", "y", "historical_date",
                                    "year")],
                            historical_date),
                 y,
                 1.5e-7)


    # Next test (for the year bug)
    dates <- as.Date(c("1900-01-01", "1901-12-31", "1902-02-03",
                        "1902-12-31", "1903-05-04", "1903-11-22",
                        "1903-12-10", "1904-01-01", "1904-02-02",
                        "1904-03-03"))

    df <- data.frame(x = 1:10,
                    y = 11:20,
                    historical_date = dates,
                    year = c(1900, 1901, 1902, 1902, 1903, 1903, 1903, 1904,
                             1904, 1904),
                    country_text_id = c("AFG", "AFG", "AFG", "AFG",
                                         "MEX", "MEX", "SWE", "SWE",
                                        "SWE", "USA"),
                    stringsAsFactors = F)

    y <- data.frame(x = c(1, 2, 2.912329, 4.832877, 9.743169),
                    y = c(11, 12, 12.91233, 14.83288, 19.74317),
                    year = c(1900, 1901, 1902, 1903, 1904))

    expect_equal(cy.day_mean(df[, c("x", "y")], dates),
                              y, 1.5e-7)

    y <- data.frame(x = c(1, 2, 2.912329, 5.165289, 7, 8.912568, 10),
                    y = c(11, 12, 12.91233, 15.16529, 17, 18.91257, 20),
                    year = c(1900, 1901, 1902, 1903, 1903, 1904, 1904),
                    country_text_id = c("AFG", "AFG", "AFG", "MEX", "SWE", "SWE", "USA"),
                    stringsAsFactors = F)

    expect_equal(cy.day_mean(df, historical_date, country_text_id),
                              y, 1.5e-7)

	df <- data.frame(
		x = 1:3,
		historical_date = as.Date(c("1900-01-01", "1901-11-30", "1902-02-03")))
	cy.day_mean(df, historical_date)


	df <- data.frame(
		x = 1:3,
		historical_date = as.Date(c("1900-01-01", "1901-12-31", "1902-02-03")))
	cy.day_mean(df, historical_date)
	# 1901 has a score of 2! -12-31 as only observation in the year gets treated
	# differently!

	# Gap year
	df <- data.frame(
		x = c(1, 2, 2),
		historical_date = as.Date(c("1900-06-01", "1902-01-31", "1902-02-03")))
	cy.day_mean(df, historical_date)


	
})
