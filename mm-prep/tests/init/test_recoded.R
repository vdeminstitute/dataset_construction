library(testthat)

#
# Test append_info
# --------------------------------------------------------------------------
df <- data.frame(question_id = c(1311, 1311, 1311),
	code = c(4, 3, 1))

qtable <- data.frame(question_id = c(1311, 1313),
	recoded = c(TRUE, TRUE),
	name = c("v2help", "v2help_rec"))


test_that("append_info", {
	expect_error(append_info(data.frame(), qtable))
	expect_error(append_info(df, data.frame()))
	expect_equal(ncol(append_info(df, qtable)), ncol(df) + 2)
	expect_equal(names(append_info(df, qtable)), c("question_id", "code", 
		"recoded", "name"))
	expect_equal(class(append_info(df, qtable)), "data.frame")
	})

#
# Test create_recoded
# --------------------------------------------------------------------------
df <- append_info(df, qtable)

test_that("create_recoded", {
	expect_error(create_recoded(data.frame(), qtable))
	expect_error(create_recoded(df, data.frame()))
	expect_equal(class(create_recoded(df, qtable)), "data.frame")
	expect_equal(create_recoded(df, qtable), transform(df, question_id = rep(1313, 3),
		code = c(2, 2, 1), name = rep("v2help_rec", 3)))
	expect_equal(nrow(create_recoded(df, qtable)), nrow(df))
	expect_error(create_recoded(transform(df, code = c(6, 3, 1)), qtable)$code)
	})	