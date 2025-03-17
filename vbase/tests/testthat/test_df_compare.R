
library(vbase)
library(testthat)

test_that("Test df_compare for structure of input", {
    
    dat <- data.frame(
        a = as.integer(c(1,2,3)),
        b = as.integer(c(4,5,6)),
        c = as.numeric(c(7.77777, 8.88888, 9.99999)))
    
    expect_error(df_compare(dat, as.list(dat))) 
    expect_error(df_compare(dat, dat$a )) 
    expect_error(df_compare(dat, as.matrix(dat)))
    expect_error(df_compare(as.list(dat), dat))
})

test_that("Test df_compare for duplicates", {

    dfa <- data.frame(
        a = as.integer(c(1, NA, 2)),
        b = as.integer(c(NA, NA, 3)))
    dfb <- data.frame(
        a = as.integer(c(NA, NA, 2)),
        b = as.integer(c(NA, NA, 4)))
    dfc <- data.frame(
        a = as.integer(c(1, NA, 2)),
        b = as.integer(c(NA, NA, 3)))
    dfd <- data.frame(
        a = as.integer(c(3, 3, 2)),
        b = as.integer(c(3, 3, 4)))
    dfe <- data.frame(
        a = as.integer(c(1, NA, 2)),
        b = as.integer(c(NA, NA, 3)))
    dff <- data.frame(
        a = as.integer(c(3, 3, 2)),
        b = as.integer(c(NA, NA, 4)))
    
    expect_error(df_compare(dfa, dfb))
    expect_error(df_compare(dfc, dfd))
    expect_error(df_compare(dfe, dff))

})

test_that("Test df_compare against using two disjoint data.frames", {

    dfa <- data.frame(a = as.integer(c(1,3)), b = as.integer(c(4,5)))
    dfnota <- dfa
    colnames(dfnota) <- c("c", "d")

    expect_error(df_compare(dfa, dfnota, keep_cols = TRUE))
    expect_error(df_compare(dfa, dfnota, keep_cols = FALSE))

})


test_that("Test df_compare against different length of doubles", {
    
    dat1 <- data.frame(
        a = c(1.111, 2.222, 3.333),
        b = c(1,2,3))
    dat2 <- data.frame(
        a = c(1.111111114521142156, 2.2222224521142156, 3.3333334521142156),
        b = c(1,2,3))
    
    dfc_out_len <- dim(df_compare(dfNew = dat1, dfOld = dat2))

    expect_equal(dfc_out_len[1], 0)
    expect_equal(dfc_out_len[2], 0)

})

test_that("Test df_compare with POSIXct", {
    df <- data.frame(
        a = rnorm(10),
        time = as.POSIXct(
            x = Sys.time() - 1*sample(1:100, 10, replace = FALSE)*60,
            tz = Sys.timezone(location = TRUE),
            origin = "1970-01-01" )
            )
    
    df_old <- df[-c(1,10), ]
    df_old[5, "a"] <- df_old[5, "a"] * 10
    df_old[6, "time"] <- as.POSIXct(as.numeric(df_old[5, "time"]) - 10000, tz = Sys.timezone(location = TRUE), origin = "1970-01-01")
    
    dfout <- df_compare(df, df_old, round_numeric = TRUE)
    
    expect_true(length(dfout[dfout$change == "-", "change" ]) == 2)
    expect_true(length(dfout[dfout$change == "+", "change" ]) == 4)
    
})
