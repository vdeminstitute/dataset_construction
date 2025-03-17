
library(vbase)
library(testthat)

test_that("Test round_df for structure of input", {

    dat <- data.frame(
        a = as.integer(c(1,2,3)),
        b = as.integer(c(4,5,6)),
        c = as.numeric(c(7.77777, 8.88888, 9.99999)))
    
    expect_error(round_df(as.list(dat), n = 3 )) 
    expect_error(round_df(dat$c, n = 3)) 
    expect_error(round_df(as.matrix(dat), n = 3))
})

test_that("Test round_df for row NA", {

    df <- data.frame(
        nmr = as.numeric(log(10)),
        time = as.POSIXct(Sys.time(), tz = Sys.timezone(location = TRUE), origin = "1970-01-01"),
        char = LETTERS[5] ) 

    df <- rbind.data.frame(df, data.frame(nmr = NA, time = NA, char = NA))
    rd_df <- round_df(df, n = 3)

    expect_true(all(is.na(rd_df[nrow(rd_df), ])))

})

test_that("Test for rounding of POSIXct", {

    df <- data.frame(
        nmr = as.numeric(log(10)),
        time = as.POSIXct(Sys.time(), tz = Sys.timezone(location = TRUE), origin = "1970-01-01"),
        char = LETTERS[5])
                        
    dfr <- round_df(df = df, n = 3)

    expect_true(nchar(as.character(as.numeric(format(df$time, "%OS")))) > 2 )
    expect_true(nchar(as.character(as.numeric(format(dfr$time, "%OS")))) <= 2 )

})


