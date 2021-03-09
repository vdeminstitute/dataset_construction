# colMedians() tests
test_that("colMedians return correct value, exclude NA", {
    expect_equal(colMedians(matrix(1:1, 1, 1)), 1)
    expect_equal(colMedians(matrix(1:4, 2, 2)), c(1.5, 3.5))
    expect_equal(colMedians(matrix(c(NA, 3, 5, 7), 2, 2)), c(NA, 6))
})

test_that("colMedians return error if not given correct class or type", {
    expect_error(colMedians(data.frame()))
    expect_error(colMedians(data.frame()), "no applicable method")
    expect_error(colMedians(as.list(1:3)), "no applicable method")
    expect_error(colMedians(matrix(c(1, 3, 5, "7"), 2, 2), "Not compatible with requested type"))
})

test_that("colMedians use R type coercion", {
    expect_equal(colMedians(matrix(c(1, 3, 5, as.factor(7)), 2, 2)), c(2, 3)) # all factors coerced to 1
    expect_equal(colMedians(matrix(c(1, 3, 5, as.logical(7)), 2, 2)), c(2, 3)) # all logical != 0 coerced to 1
})


# colSDs() tests
test_that("colSDs return correct value and exclude NA", {
    expect_equal(colSDs(matrix(1:1, 1, 1)), NaN)
    expect_equal(colSDs(matrix(1:9, 3, 3)), c(1, 1, 1))
    expect_equal(colSDs(matrix(c(1:8, NA), 3, 3)), c(1, 1, NA))
})

test_that("colSDs return error if not given correct class or type", {
    expect_error(colSDs(data.frame()))
    expect_error(colSDs(data.frame()), "no applicable method")
    expect_error(colSDs(as.list(1:3)), "no applicable method")
    expect_error(colSDs(matrix(c(1:8, "9"), 3, 3)), "Not compatible with requested type")
})

test_that("colSDs use R type coercion", {
    expect_equal(colSDs(matrix(c(1:8, as.factor(9)), 3, 3)), c(1, 1, sd(c(7, 8, 1)))) #all factors coerced to 1
    expect_equal(colSDs(matrix(c(1:8, as.logical(9)), 3, 3)), c(1, 1, sd(c(7, 8, 1)))) #all logical != 0 coerced to 1
    expect_equal(colSDs(matrix(c(1:8, as.logical(0)), 3, 3)), c(1, 1, sd(c(7, 8, 0))))
})

# rowMedians tests
test_that("rowMedians return correct value, exclude NA", {
    expect_equal(rowMedians(matrix(1:1, 1, 1)), 1)
    expect_equal(rowMedians(matrix(1:4, 2, 2)), c(2, 3))
    expect_equal(rowMedians(matrix(c(NA, 3, 5, 7), 2, 2)), c(NA, 5))

})

test_that("rowMedians return error if not given correct class or type", {
    expect_error(rowMedians(data.frame()))
    expect_error(rowMedians(data.frame()), "no applicable method")
    expect_error(rowMedians(as.list(1:3)), "no applicable method")
    expect_error(rowMedians(matrix(c(1, 3, 5, "7"), 2, 2), "Not compatible with requested type"))
})

test_that("rowMedians use R type coercion", {
    expect_equal(rowMedians(matrix(c(1, 3, 5, as.factor(7)), 2, 2)), c(3, 2)) # all factors coerced to 1
    expect_equal(rowMedians(matrix(c(1, 3, 5, as.logical(7)), 2, 2)), c(3, 2)) # all logical != 0 coerced to 1
    expect_equal(rowMedians(matrix(c(1:8, as.logical(0)), 3, 3)), c(4, 5, median(c(3, 6, 0))))
})


# rowSDs() tests
test_that("rowSDs return correct value and exclude NA", {
    expect_equal(rowSDs(matrix(1:1, 1, 1)), NaN)
    expect_equal(rowSDs(matrix(1:9, 3, 3)), c(3, 3, 3))
    expect_equal(rowSDs(matrix(c(1:8, NA), 3, 3)), c(3, 3, NA))
})

test_that("rowSDs return error if not given correct class or type", {
    expect_error(rowSDs(data.frame()))
    expect_error(rowSDs(data.frame()), "no applicable method")
    expect_error(rowSDs(as.list(1:3)), "no applicable method")
    expect_error(rowSDs(matrix(c(1:8, "9"), 3, 3)), "Not compatible with requested type")
})

test_that("rowSDs use R type coercion", {
    expect_equal(rowSDs(matrix(c(1:8, as.factor(9)), 3, 3)), c(3, 3, sd(c(3, 6, 1)))) # all factors coerced to 1
    expect_equal(rowSDs(matrix(c(1:8, as.logical(9)), 3, 3)), c(3, 3, sd(c(3, 6, 1)))) # all logical != 0 coerced to 1
    expect_equal(rowSDs(matrix(c(1:8, as.logical(0)), 3, 3)), c(3, 3, sd(c(3, 6, 0))))
})

 # dist_summary tests
 test_that("dist_summary", {

        # define utable
         utable <- data.frame(
            country_text_id = rep("AFG", 4),
            year = 1890:1893,
            gap_idx = rep(1, 4),
            stringsAsFactors = FALSE
         )

#     # expected behaviour
     input <- matrix(1:15, 5, 3)
     res <- dist_summary(input, utable = utable)
     expect_equal(dim(res), c(3, 7))

     colnames(input) <- c("AFG 1890-01-01", "AFG 1891-12-31", "AFG 1892-12-31")
     res <- dist_summary(input, utable = utable)
     expect_equal(rownames(res), as.character(seq(1:ncol(input))))

     # expanding correctly? (expanded.names)
     input <- matrix(1:15, 5, 3)
     colnames(input) <- c("AFG 1890-01-01", "AFG 1891-12-31", "AFG 1892-12-31")
     exp_names <- c("AFG 1890-01-01", "AFG 1890-12-31", "AFG 1891-12-31",
                    "AFG 1892-12-31", "AFG 1893-12-31")
     res <- dist_summary(input, expanded.names = exp_names, utable = utable)
     expect_equal(rownames(res), as.character(1:length(exp_names)))
     expect_equal(res$historical_date, as.Date(substr(exp_names, 5, 14)))

     # no column names when expanding
     input <- matrix(1:15, 5, 3)
     exp_names <- c("AFG 1890-01-01", "AFG 1890-12-31", "AFG 1891-12-31",
                    "AFG 1892-12-31", "AFG 1893-12-31")
     expect_error(dist_summary(input, expanded.names = exp_names, utable = utable))

     # dropping vignettes correctly (drop.vignettes)
     input <- matrix(1:15, 5, 3)
     colnames(input) <- c("A_pilot_1_v1", "AFG 1891-12-31", "AFG 1892-12-31")
     res <- dist_summary(input, utable = utable)
     expect_equal(nrow(res), 2)

     # entire column of NA (ie missing country-date)
     input <- matrix(c(1:3, rep(NA, 3), 4:6), 3, 3)
     output <- matrix(c(2, 2, 1, 1, 3, 1, 3,
                       rep(NA, 7),
                       5, 5, 1, 4, 6, 4, 6), 3, 7, byrow = T,
                     dimnames = list(NULL, c("mean", "median", "sd", "codelow68",
                                             "codehigh68", "codelow95", "codehigh95"))) %>%
         as.data.frame
     expect_identical(dist_summary(input, utable = utable), output)

     # test with single NA
     input <- matrix(c(1:8, NA), 3, 3)
     expect_error(dist_summary(input, utable = utable))

     # test with single NaN
     input <- matrix(c(1:8, NaN), 3, 3)
     expect_error(dist_summary(input, utable = utable))

     # expected errors
     expect_error(dist_summary(as.matrix(), utable = utable))
 })
