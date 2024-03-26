###
# interpolate
test_that("interpolate default method", {
    x <- c(NA, NA, 3)
    y <- rep(3, 3)
    out <- interpolate(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    x <- c(2, NA, 3)
    y <- c(2, 2, 3)
    expect_equal(interpolate(x), y)

    x <- c(NA, 2, 3)
    y <- c(NA, 2, 3)
    expect_equal(interpolate(x), y)

    x <- c(NA, "a", NA)
    y <- c(NA, "a", "a")
    expect_equal(interpolate(x), y)

    x <- rep(NA, 3)
    y <- rep(NA, 3)
    expect_equal(interpolate(x), y)

    x <- 1:3
    y <- 1:3
    expect_equal(interpolate(x), y)

    x <- list(1, NA, 2)
    y <- list(1, NA, 2)
    expect_equal(interpolate(x), y)
})

test_that("interpolate a matrix", {
    x <- matrix(c(1, NA, 2, NA, NA, 3, rep(NA, 3), NA, 2, 3), 3, 4)
    rownames(x) <- c("1920-01-01", "1920-06-10", "1920-12-31")
    y <- matrix(c(1, 1, 2, 3, 3, 3, rep(NA, 3), NA, 2, 3), 3, 4)
    rownames(y) <- c("1920-01-01", "1920-06-10", "1920-12-31")
    out <- interpolate(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    # Ensure that rownames are preserved
    x <- matrix(c(NA, 1), 2, 1, dimnames = list(letters[1:2], NULL))
    y <- matrix(c(1, 1), 2, 1, dimnames = list(letters[1:2], NULL))
    expect_equal(interpolate(x), y)

    x <- matrix(1, 1, 1)
    y <- matrix(1, 1, 1)
    expect_equal(interpolate(x), y)
})

test_that("interpolate a data frame", {
    x <- data.frame(x = c(1, NA, 2), y = c(NA, NA, T))
    y <- data.frame(x = c(1, 1, 2), y = rep(T, 3))
    out <- interpolate(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    x <- data.frame(x = complex(1, 2))
    y <- data.frame(x = complex(1, 2))
    expect_equal(interpolate(x), y)
})

###
# locf
test_that("locf vectors/lists", {
    x <- c(1, 2, NA, NA, 1)
    y <- c(1, 2, 2, 2, 1)

    out <- locf(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    x <- c(NA, 2, 3, NA, NA)
    y <- c(NA, 2, 3, 3, 3)
    expect_equal(locf(x), y)

    x <- c(NA, "a", NA, "b", NA)
    y <- c(NA, "a", "a", "b", "b")
    expect_equal(locf(x), y)

    # Lists should be unaffected
    x <- list(1, NA)
    y <- list(1, NA)
    expect_equal(locf(x), y)
})

test_that("locf data frame", {
    x <- data.frame(a = c(1L, NA, NA, 2L), b = c(NA, "a", NA, NA), stringsAsFactors = F)
    y <- data.frame(a = c(1L, 1L, 1L, 2L), b = c(NA, "a", "a", "a"), stringsAsFactors = F)
    out <- locf(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    x <- data.frame(a = complex(1))
    y <- data.frame(a = complex(1))
    expect_equal(locf(x), y)
})

test_that("locf matrix", {
    x <- matrix(c(1, NA, NA, NA, 2, NA), 3, 2)
    y <- matrix(c(1, 1, 1, NA, 2, 2), 3, 2)
    out <- locf(x)

    expect_equal(out, y)

    expect_false(identical(out, x))

    # Ensure that rownames are preserved
    x <- matrix(c("a", "b", NA), 3, 1, dimnames = list(letters[1:3], NULL))
    y <- matrix(c("a", "b", "b"), 3, 1, dimnames = list(letters[1:3], NULL))
    expect_equal(locf(x), y)

    x <- matrix(1, 1, 1)
    y <- matrix(1, 1, 1)
    expect_equal(locf(x), y)
})
