library(vbase)
library(testthat)

testthat::test_that("test organiseRows", {

    df <- data.frame(
        a = c(1, 2, 3),
        b = rev(c(4, 5, 6)),
        c = rnorm(3))

    a <- organiseRows(df, a)
    expect_equal(a, df)

    b <- organiseRows(df, b)
    expect_equal(b, df[order(df$b), ])

    c <- organiseRows(df, c)
    expect_equal(c, df[order(df$c), ])

    a <- organiseRows(df, a, decreasing = TRUE)
    expect_equal(a, df[order(df$a, decreasing = TRUE), ])


    c <- organiseRows(df, c, decreasing = TRUE)
    expect_equal(c, df[order(df$c, decreasing = TRUE), ])

})

testthat::test_that("test organiseRows for structure of input", {

    df <- data.frame(
        a = c(1, 2, 3),
        b = rev(c(4, 5, 6)),
        c = rnorm(3))

    expect_error(organiseRows(as.list(df), a))
    expect_error(organiseRows(df, as.list(df)))
    expect_error(organiseRows(df, df$a))
    expect_error(organiseRows(df, as.matrix(df)))

})

testthat::test_that("test organiseRows for duplicates", {

    df <- data.frame(
        a = rep(1:3, each = 2),
        b = seq(1, 6)
        )

    expect_equal(organiseRows(df, a), df)
    expect_equal(organiseRows(df, b), df[order(df$b), ])

    badecs <- organiseRows(df, b, a, decreasing = TRUE)
    expect_equal(badecs, df[order(df$b, df$a, decreasing = TRUE), ])
    bassce <- organiseRows(df, b, a, decreasing = FALSE)
    expect_equal(bassce, df[order(df$b, df$a, decreasing = FALSE), ])

    abdecr <- organiseRows(df, a, b)
    expect_equal(abdecr, df[order(df$a, df$b), ])
    
    abasce <- organiseRows(df, a, b, decreasing = TRUE)
    expect_equal(abasce, df[order(df$a, df$b, decreasing = TRUE), ])

})

# test with NA
testthat::test_that("test NA", {

    df <- data.frame(
        a = c(1, 2, 3),
        b = c(4, NA, 6),
        c = rnorm(3))

    a <- organiseRows(df, a, naLast = TRUE)
    expect_equal(a, df)

    bT <- organiseRows(df, b, naLast = TRUE)
    expect_equal(bT, df[order(df$b, na.last = TRUE), ])

    bF <- organiseRows(df, b, naLast = FALSE)
    expect_equal(bF, df[order(df$b, na.last = FALSE), ])

    df$b <- rep(NA, nrow(df))

    bT <- organiseRows(df, b, naLast = TRUE)
    expect_equal(bT, df)


})

# throw errors
testthat::test_that("test errors", {
    df <- data.frame(
        a = c(1, 2, 3),
        b = c(4, NA, 6),
        c = rnorm(3))

    expect_error(organiseRows(df, A))

    A <- c("a", "b", "c")
    expect_error(organiseRows(df, A))

    expect_error(organiseRows(df, B))
    
})