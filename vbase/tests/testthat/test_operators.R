

library(vbase)
library(testthat)

test_that("Test %>>% for cleaning functions", {
	# Objects required for testing
	fu <- function(obj) return(obj)

	# Expected errors
	expect_error(data.frame(clean = "hello", dirt = "dirty") %>>% fu)
	expect_error(list(clean = "hello", dirt = list("you")) %>>% fu)
	expect_error(list(clean = "hello", dirt = list(a = list(a = 3), b = 3)) %>>% fu)
	expect_error(list(clean = "hello", dirt = list(list(a = 3))) %>>% fu)
	expect_error(NA %>>% fu)
	expect_error(NULL %>>% fu)
	expect_error(list() %>>% fu)
	expect_error(list(clean = "hello") %>>% fu)
	expect_error(list(dirt = "hello") %>>% fu)
	expect_error(list(clean = "hello", dirt = "hello") %>>% fu)
	expect_error(list(clean = "hello", dirty = list(fu2 = "you")) %>>% fu)

	# Expected behavior
	expect_equal(list(clean = "hello", dirt = list(fu2 = "you")) %>>% fu, "hello")
})
