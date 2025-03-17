library(vbase)
library(testthat)

testthat::test_that("antiMerge", {
    
    df1 <- data.frame(
        id = c(1, 2, 3, 4, 5),
        name = c("a", "b", "c", "d", "e"),
        stringsAsFactors = FALSE
    )

    df2 <- data.frame(
        id = c(1, 2, 3, 4, 5),
        name = c("a", "c", "e", "g", "h"),
        stringsAsFactors = FALSE
    )

    df11 <- antiMerge(df1, df1)
    expect_equal(nrow(df11), 0)

    df12 <- antiMerge(df1, df2, by = "name")
    expect_equal(df12, df1[c(2,4), ])

})