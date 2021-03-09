test_that("offsett_diff", {
    m <- data.frame(a = 1,
                   b = c(rep(1, 5), rep(2, 5)),
                   c = c(rep(2, 3), NA, rep(2, 6)),
                   d = c(rep(NA, 5), rep(1, 5)),
                   e = c(rep(NA, 5), rep(2, 5)))
    m <- as.matrix(m)

    b <- data.frame(a = F,
                   b = F,
                   c = F,
                   d = c(rep(F, 5), rep(T, 5)),
                   e = c(rep(F, 5), rep(T, 5)))
    b <- as.matrix(b)

    w <- matrix(1, 10, 5)

    expect_equal(offset_diff(m, b, w), 1 / 6)
    expect_equal(offset_diff(m, b, w, min = 6), 0)
    expect_equal(offset_diff(m, matrix(F, 10, 5), w), 0)
    expect_error(offset_diff(m, b, w[, 1:3]))
})

test_that("weighted.rowMeans", {
    m <- matrix(c(1:3, NA, 4, NA), 2, 3)
    b <- matrix(c(rep(1, 3), rep(2, 3)), 2, 3)

    expect_equal(weighted.rowMeans(m, b), c(3, 2))
})

test_that("to_seq", {
    expect_equal(to_seq(c("A", "B", "C")), c(-1.5, 0, 1.5))
    expect_equal(to_seq(c("C", "B", "A")), c(1.5, 0, -1.5))
    expect_equal(to_seq(c("A", "B", "A", "C")), c(-1.5, 0, -1.5, 1.5))
    expect_equal(to_seq(c("A", "B", "C", "D")), c(-1.5, -0.5, 0.5, 1.5))
})
