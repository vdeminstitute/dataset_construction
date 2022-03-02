library(testthat)

#
# Tests for identify_reduction_groups
# --------------------------------------------------------------------------
m <- matrix(c(2, 2, 2, 2, 2, 2, 2, 2), ncol = 2, byrow = TRUE)
rnms <- c("FRA 1995-12-31", "FRA 1996-12-31", "RUS 1994-01-01", "RUS 1996-12-31")
rownames(m) <- rnms

testthat::test_that("expected dim",{
    testthat::expect_equal(nrow(m), 4)})

testthat::test_that("expected output",{
    # gap for RUS 1995, so we have 3 groups
    testthat::expect_equal(identify_reduction_groups(m), c(1, 1, 2, 3))
    m2 <- matrix(c(2, 3), ncol = 2)
    rownames(m2) <- c("FRA 1997-12-31")
    m <- rbind(m, m2)
    m <- m[order(rownames(m)),]
    testthat::expect_equal(identify_reduction_groups(m), c(1, 1, 2, 3, 4))
    m[3, 2] <- 2
    testthat::expect_equal(identify_reduction_groups(m), c(1, 1, 1, 2, 3))
    })


testthat::test_that("expected type",{
    testthat::expect_equal(class(identify_reduction_groups(m)), "numeric")
    testthat::expect_equal(is.vector(identify_reduction_groups(m)), TRUE)
    })



#
# Tests for interpolate_within_groups
# --------------------------------------------------------------------------
m <- matrix(c(NA, 1, 2, 2, NA, 2, 3, 3), ncol = 2, byrow = TRUE)
out <- matrix(c(2, 1, 2, 2, 3, 2, 3, 3), ncol = 2, byrow = TRUE)
rnms <- c("1994-01-01", "1994-12-31", "1995-12-31", "1996-12-31")
rownames(m) <- rnms
rownames(out) <- rnms

testthat::test_that("expected dim", {
    # vutils needs to be loaded
    testthat::expect_equal(nrow(interpolate_within_groups(m, c(1, 1, 2, 2))), 4)
    testthat::expect_equal(ncol(interpolate_within_groups(m, c(1, 1, 2, 2))), 2)
    testthat::expect_equal(sum(is.na(interpolate_within_groups(m, c(1, 1, 2, 2)))), 0)
    })
testthat::test_that("expected output", {
    testthat::expect_equal(interpolate_within_groups(m, c(1, 1, 2, 2)), out)
    testthat::expect_equal(interpolate_within_groups(m, c(1, 1, 1, 1)), out)
    out[1, 1] <- NA 
    out[3, 1] <- 2
    testthat::expect_equal(interpolate_within_groups(m, c(1, 2, 2, 3)), out)
    })
testthat::test_that("expected type", {
    testthat::expect_equal(class(interpolate_within_groups(m, c(1, 2, 2, 3)))[1], "matrix")
    })



#
# Tests for reduce_groups
# --------------------------------------------------------------------------
m <- matrix(c(1, 1, 2, 3, 1, 1, 3, 3), ncol = 2, byrow = FALSE)

testthat::test_that("expected dim", {
    testthat::expect_equal(nrow(reduce_groups(m, c(1, 1, 2, 3))), 3)
    testthat::expect_equal(ncol(reduce_groups(m, c(1, 1, 2, 3))), 2)
    })
testthat::test_that("expected output", {
    testthat::expect_equal(reduce_groups(m, c(1, 1, 1, 2)), matrix(c(1, 1, 3, 3), ncol = 2, byrow = TRUE))
    testthat::expect_equal(reduce_groups(m, c(0, 0, 0, 1)), matrix(c(1, 1, 3, 3), ncol = 2, byrow = TRUE))
    testthat::expect_equal(reduce_groups(m, c(-1, -1, -1, 0)), matrix(c(1, 1, 3, 3), ncol = 2, byrow = TRUE))
    testthat::expect_equal(reduce_groups(m, c(1, 2, 2, 3)), matrix(c(1, 1, 1, 1, 3, 3), ncol = 2, byrow = TRUE))
    testthat::expect_equal(reduce_groups(m, c(1, 2, 2, 2)), matrix(c(1, 1, 1, 1), ncol = 2))
    testthat::expect_error(reduce_groups(m, c(1, 2, 2, 2, 3)))
    })
testthat::test_that("expected type", {
    testthat::expect_equal(class(reduce_groups(m, c(1, 1, 1, 1))), class(matrix(c(1, 1), ncol = 2)))
    })


#
# Tests: date_to_weights_within_year
# --------------------------------------------------------------------------
vect <- as.Date(c("1960-01-01", "1960-01-31",
    "1960-06-01", "1960-12-31"), format = "%Y-%m-%d")

testthat::test_that("expected dim", {
    testthat::expect_equal(length(date_to_weights_within_year(vect[1])), length(vect[1]))
    testthat::expect_equal(length(date_to_weights_within_year(vect)), length(vect))
    })
testthat::test_that("expected output", {
    testthat::expect_true(
        identical(date_to_weights_within_year(vect), date_to_weights_within_year(sort(vect, decreasing = TRUE)))
        )
    testthat::expect_true(all({date_to_weights_within_year(vect) < 1}))
    testthat::expect_lte(sum(date_to_weights_within_year(vect)), 1)
    testthat::expect_lte(sum(date_to_weights_within_year(vect[1])), 1)
    testthat::expect_lte(sum(date_to_weights_within_year(vect[2:4])), 1)
    })
testthat::test_that("expected type", {
    testthat::expect_vector(date_to_weights_within_year(vect))
    testthat::expect_true(is.numeric(date_to_weights_within_year(vect)))
    })


#
# Tests: date_to_weights_across_years
# --------------------------------------------------------------------------
vect <- as.Date(c("1959-04-02", "1959-12-31", "1960-01-01",
    "1960-05-23", "1960-12-31"), format = "%Y-%m-%d")

testthat::test_that("expected dim", {
    testthat::expect_equal(length(date_to_weights_across_years(vect)), length(vect))
    testthat::expect_equal(length(date_to_weights_across_years(as.Date("1970-12-31", format = "%Y-%m-%d"))),
        length(as.Date("1970-12-31", format = "%Y-%m-%d")))
    })
testthat::test_that("expected output", {
    testthat::expect_true(
        identical(date_to_weights_across_years(vect), date_to_weights_across_years(sort(vect, decreasing = TRUE)))
        )
    testthat::expect_lte(sum(date_to_weights_across_years(vect)), 1)
    testthat::expect_lte(sum(date_to_weights_across_years(vect[1])), 1)
    testthat::expect_lte(sum(date_to_weights_across_years(vect[2:4])), 1)
    })
testthat::test_that("expected type", {
    testthat::expect_error(date_to_weights_across_years(as.character(vect)))
    testthat::expect_vector(date_to_weights_across_years(vect))
    testthat::expect_true(is.numeric(date_to_weights_across_years(vect)))
    })

#
# Tests: reduce confidences
# --------------------------------------------------------------------------
vect <- as.Date(c("1959-04-02", "1959-12-31", "1960-01-01",
    "1960-05-23", "1960-12-31"), format = "%Y-%m-%d")
m <- matrix(c(90, 100, 95, 100, 100, 100, 100, 55, 80, 90), nrow = 5,
    dimnames = list(paste("RUS", vect), NULL))
group_id <- c(1, 1, 1, 2, 2)
m2 <- matrix(c(NA, NA, NA, 100, NA, 100, NA, 55, 80, 90), nrow = 5,
    dimnames = list(paste("RUS", vect), NULL))

reduce_confidences(m, group_id)
m
reduce_confidences(m2, group_id)
m2

testthat::test_that("expected dim", {
    testthat::expect_equal(nrow(reduce_confidences(m, group_id)), length(unique(group_id)))
    testthat::expect_equal(nrow(reduce_confidences(m2, group_id)), length(unique(group_id)))
    testthat::expect_equal(ncol(reduce_confidences(m, group_id)), ncol(m))
    testthat::expect_equal(ncol(reduce_confidences(m2, group_id)), ncol(m2))
    })
testthat::test_that("expected type", {
    testthat::expect_error(reduce_confidences(as.character(m)))
    testthat::expect_true(is.matrix(reduce_confidences(m, group_id)))
    testthat::expect_true(all(sapply(reduce_confidences(m, group_id), is.numeric)))
    testthat::expect_error(reduce_confidences(as.character(m2)))
    testthat::expect_true(is.matrix(reduce_confidences(m2, group_id)))
    testthat::expect_true(all(sapply(reduce_confidences(m2, group_id), is.numeric)))
    })
testthat::test_that("expected output", {
    testthat::expect_equal(floor(reduce_confidences(m[group_id == 1,], group_id[group_id == 1])),
        {apply(m[group_id == 1, ], 2, function(col) weighted.mean(col, c(0.49, 0.01, 0.5))) %>% 
                    floor %>%
                    matrix(ncol = 2,
                        dimnames = list(rownames(m[group_id == 1, ])[1], NULL))}
        )   
    # watch out for NA_real_ and not logical NA
    testthat::expect_equal(setNames(reduce_confidences(m2, group_id)[1, 1], NULL), NA_real_)
    })