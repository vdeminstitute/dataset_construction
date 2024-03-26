test_that("rm_newline", {
    v <- c("test\n\n\rme\r\nhard")
    expect_equal(rm_newline(v), "test   me  hard")
    
    v <- NA
    expect_equal(rm_newline(v), as.character(NA))
    
    v <- "no"
    expect_equal(rm_newline(v), v)
    
    v <- ""
    expect_equal(rm_newline(v), v)
    
    v <- 123
    expect_equal(rm_newline(v), "123")
})

test_that("comma_swap", {
    expect_equal(comma_swap(c("Korea, North", "Korea,South", "Sweden")),
                 c("North Korea", "South Korea", "Sweden"))

    expect_error(comma_swap("Too, Many, Commas"))
    expect_error(comma_swap(NA))
})

test_that("ordinalize", {
    v <- seq(0, 1, .1)
    out <- c(0, 0, 0, .25, .25, .5, .5, .75, .75, 1, 1)
    expect_equal(ordinalize(v, categories = 5), out)
    expect_equal(ord_5C(v), out)

    out <- c(0, 0, 0, 1/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1)
    expect_equal(ordinalize(v, categories = 4), out)
    expect_equal(ord_4C(v), out)

    out <- c(0, 0, 0, 0, .5, .5, .5, 1, 1, 1, 1)
    expect_equal(ordinalize(v, categories = 3), out)
    
    v <- seq(0, 2, .2)
    out <- c(0, 0, 0, .25, .25, .5, .5, .75, .75, 1, 1)
    expect_equal(ordinalize(v, categories = 5, max = 2), out)
    expect_equal(ord_5C(v, max = 2), out)

    v <- 1:5
    out <- c(0, 0, 0, .25, .25)
    
    expect_equal(ord_5C(v, min = 1, max = 5, vec = c(0, 3, 5)), out)

    expect_error(ord_5C(v, max = 1))
    expect_error(ord_5C(v, min = 2))

    expect_error(ordinalize(1:3))
    expect_error(ordinalize(letters[1:3]))
})

test_that("ord_3C", {
    v <- seq(0, 1, .1)
    out <- c(0, 0, 0, .5, .5, .5, 1, 1, 1, 1, 1)

    expect_equal(ord_3C(v), out)

    out <- c(0, 0, 0, 0, .5, .5, .5, 1, 1, 1, 1)
    expect_equal(ord_3C(v, vec = c(0, 1/3, 2/3, 1)), out)

    expect_error(ord_3C(v, max = .5))
    expect_error(ord_3C(v, min = "a"))
})

test_that("all_identical", {
    expect_equal(all_identical(c(1, 2, 3), c(1, 2, 3)), TRUE)
    expect_equal(all_identical(c(1, 2, 3), c("1", "2", "3")), FALSE)
    expect_equal(all_identical(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3)), TRUE)
    expect_equal(all_identical(c("", NA, "a"), c("", "", NA)), FALSE)
    expect_equal(all_identical(list(a = 5, b = 6), list(a = 5, b = 6), 
        list(a = 6, b = 5)), FALSE)
    expect_error(all_identical())
    expect_equal(all_identical(TRUE), TRUE)
    expect_equal(all_identical(c(NA, NA)), TRUE)
    expect_equal(all_identical(c(1, 2, 3), c(1, 2, 3, NA)), FALSE) 
})

test_that("s_union", {
    expect_equal(s_union(c(1, 2, 3, 4, 5), c(1, 2, 3, 8), c(1, 2, 3, 5, 6)), 
        c(1, 2, 3, 4, 5, 8, 6))
    expect_equal(s_union(c(1, 2, 3, 4, 5), c(NA, NA, NaN), c("", NA)), 
        c("1", "2", "3", "4", "5", NA, "NaN", ""))
    expect_error(s_union())
    expect_equal(s_union(NA), NA)
    expect_equal(s_union(list(a = 5, b = 6)), list(a = 5, b = 6))
    
    expect_equal(s_union(list(a = 5, b = 6), list(a = 5, c = 6, d = 7)), 
                list(5, 6, 7))
    expect_equal(s_union(c(a = 5, b = 6), c(c = 5, d = 6)), c(5, 6))

    expect_equal(s_union(c(1, 2), list(1, 2)), list(1, 2))
    expect_equal(s_union(c(1, 2), list(1, 2)), list(1, 2))
    expect_equal(s_union(c("1", "2", "3"), c(1, 2, 3)), c("1", "2", "3"))
})