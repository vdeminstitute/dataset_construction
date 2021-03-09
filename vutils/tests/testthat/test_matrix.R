test_that("add_empty_cols", {
    a <- matrix(data = c(c(1, 2, 3), c(2, 3, 4)),
               ncol = 2,
               dimnames = list(c("row1", "row2", "row3"), c("a", "b")))
    b <- matrix(data = c(c(1, 2, 3), c(2, 3, 4), c(NA, NA, NA), c(NA, NA, NA)),
               ncol = 4,
               dimnames = list(c("row1", "row2", "row3"), letters[1:4]))

    expect_equal(add_empty_cols(x = a, missing = c("c", "d")), b)
})

test_that("full_rbind", {

    x <- matrix(data = c(c(1, 2, 3), c(2, 3, 4)),
               ncol = 2,
               dimnames = list(c("row1", "row2", "row3"), c("a", "b")))

    y <- matrix(data = c(c(1, 2, 2), c(2, 2, 4), c(4,5,6)),
               ncol = 3,
               dimnames = list(c("row1", "row2", "row3"), letters[1:3]))

    z <- matrix(data = c(c(1, 2, 3, 1, 2, 2), c(2, 3, 4, 2, 2, 4), c(NA, NA, NA, 4, 5, 6)),
               ncol = 3,
               dimnames = list(c("row1", "row2", "row3", "row1", "row2", "row3"), letters[1:3]))

    expect_equal(full_rbind(x, y), z)

    x <- matrix(1:9, 3, 3, dimnames = list(letters[1:3], letters[4:6]))
    y <- matrix(1:3, 1, 3, dimnames = list("xx", letters[4:6]))
    z <- matrix(c(c(1, 2, 3, 1), c(4, 5, 6, 2), c(7, 8, 9, 3)), 4, 3,
               dimnames = list(c(letters[1:3], "xx"), letters[4:6]))

    expect_equal(full_rbind(x, y), z)
})

test_that("add_to_matrix", {
    m <- matrix(1:9, 3, 3)
    expect_equal(add_to_matrix(m, 1), matrix(2:10, 3, 3))

    b <- matrix(c(T, rep(F, 7), T), 3, 3)
    expect_equal(add_to_matrix(m, NA, by = b), matrix(c(NA, 2:8, NA), 3, 3))
})

test_that("add_empty_rows", {
    x <- matrix(1:3, 1, 3, dimnames = list("a", c("1", "2", "3")))
    y <- matrix(c(1:3, rep(NA, 9)), 4, 3,
               dimnames = list(letters[1:4], c("1", "2", "3")), byrow = T)
    expect_equal(add_empty_rows(x, c("d", "b", "c")), y)

    x <- matrix(1, 1, 1)
    y <- matrix(c(1, NA), 2, 1, dimnames = list(c("", "a"), NULL))
    expect_equal(add_empty_rows(x, "a"), y)

    x <- matrix(1:3, 3, 1)
    expect_equal(add_empty_rows(x, character()), x)

    x <- matrix(letters[1:3], 3, 1, dimnames = list(letters[1:3], NULL))
    y <- matrix(c(letters[1:3], NA), 4, 1, dimnames = list(letters[1:4], NULL))
    expect_equal(add_empty_rows(x, "d"), y)
})
