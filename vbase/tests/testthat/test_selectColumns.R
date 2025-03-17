library(vbase)
library(testthat)

testthat::test_that("simple symbol", {

    tobj <- selectColumns(mtcars, mpg)    
    testthat::expect_equal(tobj, mtcars[, "mpg", drop = FALSE])
    testthat::expect_equal(ncol(tobj), 1)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("simple character", {

    tobj <- selectColumns(mtcars, "mpg")    
    testthat::expect_equal(tobj, mtcars[, "mpg", drop = FALSE])
    testthat::expect_equal(ncol(tobj), 1)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("simple symbol but keep all", {

    tobj <- selectColumns(mtcars, mpg, keep_all = TRUE)
    correct_order <- union("mpg", names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("simple char but keep all", {

    tobj <- selectColumns(mtcars, "mpg", keep_all = TRUE)
    correct_order <- union("mpg", names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})


testthat::test_that("simple character", 
{
    tobj <- selectColumns(mtcars, "mpg")
    testthat::expect_equal(tobj, mtcars[, "mpg", drop = FALSE])
    testthat::expect_equal(ncol(tobj), 1)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several symbol", {

    tobj <- selectColumns(mtcars, mpg, cyl)
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several char", {

    tobj <- selectColumns(mtcars, "mpg", "cyl")
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several symbol but keep all", {

    tobj <- selectColumns(mtcars, mpg, qsec, keep_all = TRUE)
    correct_order <- union(c("mpg", "qsec"), names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several char but keep all", {

    tobj <- selectColumns(mtcars, "mpg", "qsec", keep_all = TRUE)
    correct_order <- union(c("mpg", "qsec"), names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("c(symbols)", {

    tobj <- selectColumns(mtcars, c(mpg, cyl))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("c(char)", {

    tobj <- selectColumns(mtcars, c("mpg", "cyl"))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("union", {

    aa <- "mpg"; bb <- "cyl"
    tobj <- selectColumns(mtcars, union(aa, bb))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("intersection", {

    aa <- "mpg"; bb <- c("cyl", "mpg")
    tobj <- selectColumns(mtcars, intersect(aa, bb))
    testthat::expect_equal(tobj, mtcars[, c("mpg"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 1)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("intersection", {

    aa <- "mpg"; bb <- c("cyl")
    testthat::expect_error(selectColumns(mtcars, intersect(aa, bb)))

})

testthat::test_that("setdiff", {

    aa <- "mpg"; bb <- c("cyl", "hp")
    tobj <- selectColumns(mtcars, setdiff(aa, bb))
    testthat::expect_equal(tobj, mtcars[, c("mpg"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 1)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("several char", {

    tobj <- selectColumns(mtcars, "mpg", "cyl")
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several symbol but keep all", {

    tobj <- selectColumns(mtcars, mpg, qsec, keep_all = TRUE)
    correct_order <- union(c("mpg", "qsec"), names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("several char but keep all", {

    tobj <- selectColumns(mtcars, "mpg", "qsec", keep_all = TRUE)
    correct_order <- union(c("mpg", "qsec"), names(mtcars))
    testthat::expect_equal(tobj, mtcars[, correct_order, drop = FALSE])
    testthat::expect_equal(ncol(tobj), length(correct_order))
    testthat::expect_equal(nrow(tobj), nrow(mtcars))
    
})

testthat::test_that("c(symbols)", {

    tobj <- selectColumns(mtcars, c(mpg, cyl))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("c(char)", {

    tobj <- selectColumns(mtcars, c("mpg", "cyl"))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(ncol(tobj), 2)
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

# Errors
testthat::test_that("errors", {

    testthat::expect_error(selectColumns(blah, blahCol))
    testthat::expect_error(selectColumns(mtcars, blahCol))
    testthat::expect_error(selectColumns(df, blahCol))
    testthat::expect_error(selectColumns(mtcars, mpgg))
    testthat::expect_error(selectColumns(mtcars))
    testthat::expect_error(selectColumns(mtcars, mtcars)) # ??
    testthat::expect_error(selectColumns(mtcars, NULL))

})

testthat::test_that("errors for non-implemented choices", {

    testthat::expect_error(selectColumns(mtcars, list(mpg)))
    testthat::expect_error(selectColumns(mtcars, 1:2))
    testthat::expect_error(selectColumns(mtcars, matrix(mpg)))
    testthat::expect_error(selectColumns(mtcars, names(mtcars) == "mpg"))

})

# selectors
testthat::test_that("pickAll", {

    tobj <- selectColumns(mtcars, pickAll(mpg, cyl))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAll", {

    tobj <- selectColumns(mtcars, pickAll(c(mpg, cyl)))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAll", {

    tobj <- selectColumns(mtcars, pickAll("mpg", "cyl"))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAll", {

    tobj <- selectColumns(mtcars, pickAll(c("mpg", "cyl")))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAll", {

    testthat::expect_error(selectColumns(mtcars, pickAll(mpg, CYL)))
    testthat::expect_error(selectColumns(mtcars, pickAll()))
    testthat::expect_error(selectColumns(mtcars, pickAll(NA, mpg)))
    testthat::expect_error(selectColumns(mtcars, pickAll(NULL)))
    testthat::expect_error(selectColumns(mtcars, pickAll(1)))
    testthat::expect_error(selectColumns(mtcars, pickAll(MPG, CYL)))
    testthat::expect_error(selectColumns(mtcars, pickAll("MPG", "CYL")))
    testthat::expect_error(selectColumns(mtcars, pickAll("mpg", "CYL")))
    testthat::expect_error(selectColumns(mtcars, pickAll("mpg", "MPG")))
    testthat::expect_error(selectColumns(mtcars, pickAll("mpg", NA)))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(mpg, cyl))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(c(mpg, cyl)))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny("mpg", "cyl"))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(c("mpg", "cyl")))
    testthat::expect_equal(tobj, mtcars[, c("mpg", "cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(mpg, cylinders))
    testthat::expect_equal(tobj, mtcars[, c("mpg"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(c(mpg, cylinders)))
    testthat::expect_equal(tobj, mtcars[, c("mpg"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny("Mpg", "cyl"))
    testthat::expect_equal(tobj, mtcars[, c("cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    tobj <- selectColumns(mtcars, pickAny(c("Mpg", "cyl")))
    testthat::expect_equal(tobj, mtcars[, c("cyl"), drop = FALSE])
    testthat::expect_equal(nrow(tobj), nrow(mtcars))

})

testthat::test_that("pickAny", {

    testthat::expect_error(selectColumns(mtcars, pickAny(c("Mpg", "Cyl"))))

})

# Inside other functions

testthat::test_that("inside f", {

    f <- function(df) {
        out <- selectColumns(df, cyl, hp)
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("cyl", "hp")])

    f <- function(df) {
        out <- selectColumns(df, "cyl", "hp")
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("cyl", "hp")])

    f <- function(df) {
        out <- selectColumns(df, c("mpg", "hp", "cyl"))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        out <- selectColumns(df, c(mpg, hp, cyl))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        out <- selectColumns(df, pickAll(mpg, hp, cyl))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        out <- selectColumns(df, pickAll("mpg", "hp", "cyl"))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        out <- selectColumns(df, pickAll(c("mpg", "hp", "cyl")))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])


    f <- function(df) {
        vec <- c("mpg", "hp", "cyl")
        out <- selectColumns(df, pickAny(vec))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        vec <- c("mpg", "hp", "cyl", "LOL")
        out <- selectColumns(df, pickAny(vec))
        return(out)
    }    
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        vec <- c("mpg", "hp", "cyl")
        out <- selectColumns(df, pickAll(vec))
        return(out)
    }
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

    f <- function(df) {
        vec <- c("mpg", "hp", "cyl")
        out <- selectColumns(df, pickAny(vec))
        return(out)
    }    
    testthat::expect_equal(f(mtcars), mtcars[, c("mpg", "hp", "cyl")])

})



