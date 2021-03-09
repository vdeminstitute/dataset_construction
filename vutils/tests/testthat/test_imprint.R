test_that("imprint", {
    # Testcase 1 #
    wdata <- as.matrix(data.frame(code_1 = c(1,1,-1,-1),
                        code_2 = c(-1,-1,2,-1)))
    historical = c(T,F)

    wdata_output <- as.matrix(data.frame(code_1 = c(T,T,F,F),
                               code_2 = c(F,F,F,F)))

    expect_equal(imprint(wdata, historical), wdata_output)

    # Testcase 2 #
    wdata <- as.matrix(data.frame(code_1 = c(1,1,-1,-1),
                        code_2 = c(-1,-1,2,-1),
                        code_3 = c(2,-1,-1,3)))
    historical = c(T,F,T)

    wdata_output <- as.matrix(data.frame(code_1 = c(T,T,F,F),
                               code_2 = c(F,F,F,F),
                               code_3 = c(T,F,F,T)))

    expect_equal(imprint(wdata, historical), wdata_output)

    # Testcase 3: error #
    wdata <- as.matrix(data.frame(code_1 = c(1,1,-1,-1),
                        code_2 = c(-1,-1,2,-1),
                        code_3 = c(2,-1,-1,3)))
    historical = c(T,F)

    expect_error(imprint(wdata, historical))
})
