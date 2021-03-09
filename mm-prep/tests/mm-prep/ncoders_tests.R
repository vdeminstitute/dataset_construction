library(testthat)

test_that("country-date", {
    m <- matrix(c(1, 2, NA, NA, 3, NA, 5, NA), nrow = 2)
    rownames(m) <- c("USA 1900-06-06", "USA 1900-12-31")

    out <- list(
        cd = data.frame(
            country_text_id = "USA", 
            historical_date = as.Date(c("1900-06-06", "1900-12-31")), 
            v2test_nr = c(3, 1)),
        cy = data.frame(
            country_text_id = "USA", 
            year = 1900, 
            v2test_nr = 3))
    expect_equal(main(m, "v2test"), out)
})

test_that("country-year", {
    m <- matrix(c(1, 2, NA, NA, 3, NA, 5, NA), nrow = 2)
    rownames(m) <- c("USA 1900-06-06", "USA 1901-12-31")
    out <- list(
        cd = data.frame(
            country_text_id = "USA", 
            historical_date = as.Date(c("1900-06-06", "1901-12-31")), 
            v2test_nr = c(3, 1)),
        cy = data.frame(
            country_text_id = c("USA", "USA"),
            year = c(1900, 1901), 
            v2test_nr = c(3, 1)))
    expect_equal(main(m, "v2test"), out)
})
