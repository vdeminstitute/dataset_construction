Rdf_mean <- function(df, weights, year, na.rm = T) {
    out <- lapply(df, function(v) {
        weighted.mean(v, weights, na.rm = na.rm)
    }) %>% as.data.frame

    out$year <- year
    out
}

test_that("day_mean.data.frame", {
    # Let's first start with predefined weights before using R to autocalculate everything
    df <- data.frame(x = c(1, 2, 3), y = c(1L, 2L, 3L), z = c(1, NA, 3))
    dates <- as.Date(c("1900-01-01", "1900-03-15", "1900-10-20"))

    expect_equal(day_mean(df, dates), data.frame(x = 2, y = 2, z = 2, year = 1900))

    df <- data.frame(x = c(2, 2, 3))
    dates <- as.Date(c("1900-01-02", "1900-05-04", "1900-10-11"))

    expect_equal(day_mean(df, dates),
                 data.frame(x = (122 * 2 + 160 * 2 + 82 * 3) / sum(c(122, 160, 82)), year = 1900))

    # Let's try testing columns with all NAs and option na.rm
    df <- data.frame(x = rep(NA_real_, 4), y = 1:4, z = c(NA, NA, 3, NA))
    dates <- as.Date(c("1900-01-01", "1900-04-01", "1900-05-22", "1900-12-31"))
    weights <- c(as.numeric(diff(dates)), 1)

    expect_equal(day_mean(df, dates), Rdf_mean(df, weights, 1900))
    expect_equal(day_mean(df, dates, na_rm = F),
                 Rdf_mean(df, weights, 1900, na.rm = F))

    df <- data.frame(x = 1)
    dates <- as.Date(c("1900-01-01", "1900-06-02"))

    expect_error(day_mean(df, dates))
    expect_equal(day_mean(data.frame(), dates[-c(1, 2)]), data.frame())
    expect_equal(day_mean(df, dates[1]), data.frame(x = 1, year = 1900))

    df <- data.frame(x = c(1.5, 4, 3.2))
    dates <- as.Date(c("1989-02-03", "1904-05-01", "1906-01-01"))
    weights <- c(as.numeric(diff(dates)), as.Date("1906-12-31") - tail(dates, 1) + 1)

    expect_equal(day_mean(df, dates), Rdf_mean(df, weights, 1989))

    dates[1] <- NA
    expect_equal(day_mean(df, dates), data.frame(x = NA_real_, year = NA_real_))

    # Lastly, try passing in a non-integer/numeric column
    df <- data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = F)
    dates <- as.Date(c("1900-01-01", "1900-12-31"))

    expect_error(day_mean(df, dates))

    # H/w, we'll trigger an early return and no error if only one row
    expect_equal(day_mean(data.frame(x = "a"), as.Date("1900-01-01")),
                 data.frame(x = "a", year = 1900))
})

Rmatrix_mean <- function(m, weights, year, na.rm = T) {
    v <- apply(m, 2, function(v) {
        weighted.mean(v, weights, na.rm = na.rm)
    })

    matrix(v, 1, 3, dimnames = list(1900, colnames(m)))
}


test_that("day_mean.matrix", {
    m <- matrix(1:3, 3, 1)
    dates <- as.Date(c("1900-01-01", "1900-03-15", "1900-10-20"))

    expect_equal(day_mean(m, dates), matrix(2, 1, 1, dimnames = list(1900, NULL)))

    m <- matrix(1L:9L, 3, 3)
    dates <- as.Date(c("1900-01-22", "1900-01-23", "1900-01-24"))
    weights <- c(as.numeric(diff(dates)), as.Date("1900-12-31") - tail(dates, 1) + 1)

    expect_equal(day_mean(m, dates), Rmatrix_mean(m, weights, 1900))

    m[1, 1] <- NA
    colnames(m) <- c("a", "b", "c")

    expect_equal(day_mean(m, dates), Rmatrix_mean(m, weights, 1900))
    expect_equal(day_mean(m, dates, na_rm = F),
                 Rmatrix_mean(m, weights, 1900, na.rm = F))

    m <- matrix(letters[1:9], 3, 3)
    expect_error(day_mean(m, dates))

    m <- matrix(1:2, 2, 1)
    dates <- as.Date("1900-01-01")

    expect_error(day_mean(m, dates))
    expect_equal(day_mean(m[1,, drop = F], dates),
                 matrix(1, 1, 1, dimnames = list(1900, NULL)))
    expect_error(day_mean(matrix(), dates[-1])) # Empty matrix has one row & column
    expect_equal(day_mean(m[-c(1, 2), -1], dates[-1]), m[-c(1, 2), -1])
})

test_that("day_mean prior 1900", {
    x <- data.frame(x = 1:3)
    dates <- as.Date(c("1889-01-01", "1889-03-04", "1889-12-31"))

    expect_equal(day_mean(x, dates), data.frame(x = 1.832877, year = 1889), 1e-6)
})
