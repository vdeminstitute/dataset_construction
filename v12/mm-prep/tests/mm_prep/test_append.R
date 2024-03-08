library(testthat)

    # test data
    d2 <- c(1, NA, 3, 1, NA, 2, 2, 3, 3, 0, NA, NA)
    d1 <- {set.seed(42); sample(c(TRUE, FALSE), 12, replace = TRUE)}
    m <- matrix(d1, nrow = 4, byrow = TRUE, dimnames = 
        list(paste0("RUS ", 2010:2013, "-12-31"), NULL))
    m2 <- matrix(d2, nrow = 4, byrow = TRUE, dimnames = 
        list(paste0("RUS ", 2010:2013, "-12-31"), NULL))
    lat_m <- matrix({set.seed(42); sample(as.logical(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)), 12, replace = TRUE)}, nrow = 4)
    is_lat <- as.logical(rowSums(lat_m))

    # output is a matrix
    test_that("output class and everything",{
        # output is a matrix
        expect_equal(is.matrix(append_vignettes(m, is_lat)), is.matrix(matrix()))
        expect_equal(is.matrix(append_vignettes(m2, is_lat, lat_m)), is.matrix(matrix()))
        # input condition is a matrix
        expect_equal(is.matrix(append_vignettes(m, as.matrix(is_lat))), is.matrix(matrix()))
        expect_equal(is.matrix(append_vignettes(m2, as.matrix(is_lat), lat_m)), is.matrix(matrix()))
        # output for the function has nrow(input_matrix) + n cases where condition is TRUE
        expect_equal(nrow(append_vignettes(m, is_lat)), {nrow(m) + length(is_lat[is_lat == TRUE])})
        expect_equal(nrow(append_vignettes(m2, is_lat, lat_m)), {nrow(m) + length(is_lat[is_lat == TRUE])})
        # check the correspondence of output piece by piece
        # 2 args
        repl <- matrix(rep(FALSE, 6), nrow = 2, ncol = 3, dimnames = list(paste0("A_RUS ", c(2010, 2012), "-12-31"), NULL))
        out <- rbind(m, repl)

        expect_equal(append_vignettes(m, is_lat), out)
        # when all values of the condition is FALSE, we will get input matrix
        # 2 args
        expect_equal(append_vignettes(m, rep(FALSE, nrow(m))), m)
        # check the correspondence of output piece by piece
        # 3 args
        repl <- matrix(c(1, NA, 3, 2, 3, 3), byrow = TRUE, nrow = 2,
            dimnames = list(paste0("A_RUS ", c(2010, 2012), "-12-31"), NULL))
        mt <- m2
        mt[c(1,3),] <- matrix(c(NA, NA, NA, NA, 3, NA), nrow = 2, byrow = TRUE)
        out <- rbind(mt, repl)

        expect_equal(append_vignettes(m2, is_lat, lat_m), out)
        # when all values of the condition is FALSE, we will get input matrix
        # 3 args
        is_lat <- rep(FALSE, nrow(m2))
        expect_equal(append_vignettes(m2, is_lat, lat_m), m2)
    })

    d2 <- c(1, NA, 3, 1, NA, 2, 2, 3, 3, 0, NA, NA)
    d1 <- {set.seed(42); sample(c(TRUE, FALSE), 12, replace = TRUE)}
    m <- matrix(d1, nrow = 4, byrow = TRUE, dimnames = 
        list(paste0("RUS ", 2010:2013, "-12-31"), NULL))
    lat_m <- matrix({set.seed(42); sample(as.logical(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)), 12, replace = TRUE)}, nrow = 4)
    is_lat <- as.logical(rowSums(lat_m))

    # First of all test arguments of the function
    test_that("test classes of function arguments",{
        expect_error(is.matrix(append_vignettes(as.data.frame(m), is_lat)))
        expect_error(is.matrix(append_vignettes(m, list(is_lat))))
        # input m with 3 args should be numeric
        expect_error(append_vignettes(m, is_lat, lat_m))
        expect_error(append_vignettes(m2, is_lat, as.numeric(lat_m)))

        m[1,1] <- NA
        expect_error(append_vignettes(m, is_lat))

        m[1,1] <- 3
        expect_error(append_vignettes(m, is_lat))

        # the input matrix should have rownames
        m <- matrix(d1, nrow = 4, byrow = TRUE, dimnames = 
        list(paste0("RUS ", 2010:2013, "-12-31"), NULL))
        rownames(m) <- NULL
        expect_error(append_vignettes(m, is_lat))

        rownames(m2) <- NULL
        expect_error(append_vignettes(m, is_lat, lat_m))

        expect_error(append_vignettes(matrix(nrow = 0, ncol = 0), is_lat))
        expect_error(append_vignettes(matrix(nrow = 0, ncol = 0), is_lat, lat_m))
    })