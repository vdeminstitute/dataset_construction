library(testthat)

#
# Test list_responses
# --------------------------------------------------------------------------
qtable <- data.frame(
	cb_responses = "0 1 2 3 4"
	)

test_that("list_responses", {
	expect_error(list_responses(bind_rows(qtable, qtable)))
	expect_error(list_responses(data.frame(cb_responses = NA_character_)))
	expect_error(list_responses(list(cb_responses = "0 1 2 3 4 5")))
	expect_error(list_responses(data.frame(cb_responses = " ")))
	expect_equal(list_responses(qtable), "0 1 2 3 4")
	expect_equal(length(list_responses(qtable)), length(qtable$cb_responses))
	})

#
# Test filter_dirty
# --------------------------------------------------------------------------
df <- data.frame(id = seq(1, 3, 1), code = c(0, 2, 10))

test_that("filter_dirty", {
	expect_equal(filter_dirty(df, qtable$cb_responses), 3)
	expect_equal(length(filter_dirty(df, qtable$cb_responses)), 1)
	expect_equal(class(filter_dirty(df, qtable$cb_responses)), "numeric")
	expect_error(filter_dirty(data.frame(), qtable$cb_responses))
	expect_error(filter_dirty(list(id = seq(1, 3, 1),
		code = c(0, 2, 10)), qtable$cb_responses))
	})