test_that("osp for ordinal vars", {
    # static
    z <- matrix(c(0,1,6,5,8,7,3,4,6), 3, 3)
    mu <- matrix(1:9, 3, 3)

    # hand calculated output, rounded to three decimals
    very_handy <- data.frame(a = c(.159, .159, 1.500), b = c(1.864, 2.499, 1.864), c = c(1.136, 1.136, 1.500)) %>% data.matrix()
    dimnames(very_handy) <- NULL

    expect_equal(round(osp(z, mu), 3), very_handy)

    # non static
    set.seed(123)
    # numer of rows/draws
    N <- 5

    # country_dates and generated point estimates
    a <- rnorm(N, mean = 1)
    b <- rnorm(N, mean = 2)
    c <- rnorm(N, mean = 3)
    d <- rnorm(N, mean = 4)
    e <- rnorm(N, mean = 5)

    z <- data.frame(a, b, c, d, e) %>% data.matrix()

    # mu thresholds for 4 cathogories, 3 thresholds
    f <- rnorm(N, mean = 2)
    g <- rnorm(N, mean = 3)
    h <- rnorm(N, mean = 4)

    mu <- data.frame(f, g, h) %>% data.matrix()

    # formula
    out_0 <- pnorm(mu[,1] - z)
    out_1 <- pnorm(mu[,2] - z) - pnorm(mu[,1] - z)
    out_2 <- pnorm(mu[,3] - z) - pnorm(mu[,2] - z)
    out_3 <- 1 - pnorm(mu[,3] - z)

    osp_out <- 0 * out_0 + 1 * out_1 + 2 * out_2 + 3 * out_3

    expect_equal(osp(z, mu), osp_out)


    # test that colnames are preserved in z
    z_0 <- data.frame(a, b, c, d, e) %>% data.matrix()
    z_t <- data.frame(a, b, c, d, e) %>% data.matrix()
    invisible(osp(z_t, mu))

    expect_equal(colnames(z_t), colnames(z_0))

    # test that input is not modified
    mu_0 <- data.frame(f, g, h) %>% data.matrix()
    mu_t <- data.frame(f, g, h) %>% data.matrix()
    invisible(osp(z_t, mu_t))

    expect_equal(z_t, z_0)
})

test_that("osp for binary vars", {
    z <- matrix(9:1, 3, 3)
    g <- matrix(3:1, 3, 1)

    out <- matrix(c(1, 1, 1, 1, 1, .99999, 1, .99996, .97725), 3, 3)

    expect_equal(osp(z, g), out, tolerance = 1e-5)

    set.seed(123)
    z <- matrix(rnorm(18), 6, 3)
    g <- matrix(rnorm(6), 6, 1)
    out <- osp(z, g)

    expect_equal(out, pnorm(z + as.vector(g)))
    expect_false(identical(out, z))

    z <- matrix(rnorm(18), 6, 3, dimnames = list(NULL, c("a", "b", "c")))
    g <- matrix(rnorm(6), 6, 1)

    out <- osp(z, g)

    expect_equal(colnames(out), colnames(z))
})
