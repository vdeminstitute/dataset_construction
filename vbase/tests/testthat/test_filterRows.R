library(vbase)
library(testthat)

# Unit tests for filterRows
testthat::test_that("test filterRows", {
    # Create a test data frame
    df <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = rev(c(1, 2, 3, 4, 5)),
        z = c(1, 2, 3, 4, 5)
    )

    # Test that the function works
    expect_equal(filterRows(df, x > 3), df[df$x > 3, ])
    expect_equal(filterRows(df, x > 3, y > 3), df[df$x > 3 & df$y > 3, ])

    expect_equal(filterRows(df, x == 2, y > 3, combine = "all"), df[df$x == 2 & df$y > 3, ])
    expect_equal(filterRows(df, x == 2, y > 3, combine = "any"), df[df$x == 2 | df$y > 3, ])



})

# Unit tests for filterRows with vectors as symbols
testthat::test_that("test filterRows", {
    # Create a test data frame
    df <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = rev(c(1, 2, 3, 4, 5)),
        v = letters[1:5]
    )

    a = 2; b = 3; c = c("b", "c")

    expect_equal(filterRows(df, x == a), df[df$x == a, , drop = FALSE])   
    expect_equal(filterRows(df, x > a), df[df$x > a, , drop = FALSE])   
    expect_equal(filterRows(df, x >= a, x <= b), df[df$x >= a & df$x <= b, , drop = FALSE])   
    expect_equal(filterRows(df, x >= a, x <= b, combine = "any"), df[df$x >= a | df$x <= b, , drop = FALSE])

    expect_equal(filterRows(df, v %in% c), df[df$v %in% c, , drop = FALSE])
    expect_equal(filterRows(df, !v %in% c), df[!df$v %in% c, , drop = FALSE])

    expect_equal(filterRows(df, x == a, v %in% c), df[df$x == a & df$v %in% c, , drop = FALSE])
    expect_equal(filterRows(df, x == a, v %in% c, combine = "any"), df[df$x == a | df$v %in% c, , drop = FALSE])

    expect_equal(filterRows(df, x == a, v %in% c, combine = "all"), df[df$x == a & df$v %in% c, , drop = FALSE])
    expect_equal(filterRows(df, x == a, v %in% c, combine = "any"), df[df$x == a | df$v %in% c, , drop = FALSE])

    expect_equal(filterRows(df, x == mean(x)), df[df$x == mean(df$x), , drop = FALSE])
    expect_equal(filterRows(df, x == max(x), v %in% c), df[df$x == max(df$x) & df$v %in% c, , drop = FALSE])

})

# unit tests for filterRows with NA
testthat::test_that("filterRows with NA", {

    df <- data.frame(
        x = c(NA, 2, 3, 4, 5),
        y = rev(c(1, 2, NA, 4, 5)),
        v = c(letters[1:4], NA)
    )

    expect_equal(filterRows(df, x == 2, includeNA = FALSE), df[df$x == 2 & !is.na(df$x), , drop = FALSE])
    expect_equal(filterRows(df, x == 2, includeNA = TRUE), df[1:2, , drop = FALSE])

    expect_equal(filterRows(df, x == 2, v == "d", combine = "any"), df[df$x == 2 & !is.na(df$x) | df$v == "d" & !is.na(df$v), , drop = FALSE])
    expect_equal(filterRows(df, x == 2, v == "b", combine = "all", includeNA = TRUE), df[df$x == 2 & !is.na(df$x) | df$v == "b" & !is.na(df$v), , drop = FALSE])

})

# unit tests for filterRows with grepl
testthat::test_that("filterRows with NA", {

    df <- data.frame(
        x = c("parrot", "tort", "fox"),
        y = rev(toupper(c("parrot", "tort", "fox")))
    )

    expect_equal(filterRows(df, grepl("par", x)), df[grepl("par", df$x), , drop = FALSE])
    expect_equal(filterRows(df, grepl("par", x), grepl("PAR", y)), df[grepl("par", df$x) & grepl("PAR", df$y), , drop = FALSE])

})