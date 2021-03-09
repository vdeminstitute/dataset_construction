# ord() tests

test_that("ord for ordinals", {
    
    # static test
    z <- matrix(c(0,1,6,5,8,7,3,4,6), 3, 3)
    mu <- matrix(1:9, 3, 3)

    # hand calculated output, rounded to three decimals
    very_handy <- data.frame(a = c(0, 0, 1), b = c(2, 2, 2), c = c(1, 1, 1)) %>% data.matrix()
    dimnames(very_handy) <- NULL

    expect_equal(ord(z, mu), very_handy)
    expect_equal(dim(ord(z, mu)), dim(z))
    
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
    
    # same dimensions
    expect_equal(dim(ord(z, mu)), dim(z))
    
    # formula out
    out_ord <- ifelse(z <= mu[,1], 0, ifelse(mu[,1] < z & z <= mu[,2], 1,
                                    ifelse(mu[,2] < z & z <= mu[,3], 2, 
                                    ifelse(z > mu[,3], 3, NA))))
    
    expect_equal(ord(z, mu), out_ord)
    
    # test that colnames are preserved in z
    z_0 <- data.frame(a, b, c, d, e) %>% data.matrix()
    z_t <- data.frame(a, b, c, d, e) %>% data.matrix()
    invisible(ord(z_t, mu))
    
    expect_equal(colnames(z_t), colnames(z_0))  
    
    # test that input is not modified
    mu_0 <- data.frame(f, g, h) %>% data.matrix()
    mu_t <- data.frame(f, g, h) %>% data.matrix()
    invisible(ord(z_t, mu_t))
    
    expect_equal(z_t, z_0)
})

test_that("ord for binary vars", {
    z <- matrix(-4:4, 3, 3) / 10
    g <- as.matrix(1:3) / 20

    out <- matrix(c(0, 0, 0, 0, 1, 1, 1, 1, 1), 3, 3)
    expect_equal(ord(z, g), out)

    set.seed(123)
    z <- matrix(rnorm(18), 6, 3)
    g <- matrix(rnorm(6), 6, 1)

    out <- ord(z, g)
    R_out <- pnorm(z + as.vector(g)) > .5
    storage.mode(R_out) <- "integer"

    expect_equal(out, R_out)
    expect_false(identical(out, z))

    # Finally, let's test that colnames are also preserved for binary
    # vars
    z <- matrix(rnorm(18), 6, 3, dimnames = list(NULL, c("a", "b", "c")))
    g <- matrix(rnorm(6), 6, 1)

    out <- ord(z, g)

    expect_equal(colnames(out), colnames(z))
})


