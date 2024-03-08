library(testthat)

#
# Test append_qtable
# --------------------------------------------------------------------------
qtable <- data.frame(question_id = c(1311, 9999, 9998),
	name = c("v2help", "v2help_1", "v2help_3"),
	class = c("C", "C", "C"),
	question_type = c("S", "S", "S"),
	cb_responses = c("0 1 2 3 4 5", "0 1", "0 1"),
	choice_values = c("0 1 2 3 4 5", "0 1 2 3 4 5", "0 1 2 3 4 5")
	)

df <- data.frame(id = c(111, 112),
	question_id = c(1311, 1311),
	country_id = c(11, 11),
	coder_id = c(1111, 1111),
	historical_date = as.Date(c("2020-12-31", "2020-12-31"), format = "%Y-%m-%d"),
	code = c(NA_real_, NA_real_),
	text_answer = c("1,2,3", NA_character_),
	confidence = c(100, 100),
	year = c(2020, 2020),
	data_type = c("normal", "normal")
	)

test_that("append_qtable", {
	expect_error(append_qtable(data.frame(), qtable))
	expect_error(append_qtable(df, cbind(qtable[, -4], question_type = c("M", "M", "M"))))
	expect_equal(ncol(append_qtable(df, qtable)), ncol(df) + 3)
	expect_equal(names(append_qtable(df, qtable)), c("id", "question_id",
		"country_id", "coder_id", "historical_date", "code", "text_answer", "confidence",
		"year", "data_type", "name", "class", "question_type"))
	expect_equal(nrow(append_qtable(df, qtable)), nrow(df))
	})

#
# Test calc_choices
# --------------------------------------------------------------------------
df <- append_qtable(df, qtable)

test_that("calc_choices", {
	expect_error(calc_choices(data.frame(), qtable))
	expect_error(calc_choices(df, data.frame()))
	expect_equal(ncol(calc_choices(df, qtable)), 4)
	expect_equal(nrow(calc_choices(df, qtable)), 1)
	expect_equal(calc_choices(df, qtable), data.frame(
		name = "v2help", ms_choices = "1,2,3",
		cb_responses = "0 1 2 3 4 5",
		choice_values = "0 1 2 3 4 5"))
	})

#
# Test calc_ms_expanded
# --------------------------------------------------------------------------
choices_df <- calc_choices(df, qtable)

test_that("calc_ms_expanded", {
	expect_error(calc_ms_expanded(data.frame(), qtable))
	expect_error(calc_ms_expanded(df, data.frame()))
	expect_equal(ncol(calc_ms_expanded(df, choices_df)), ncol(df) + ncol(choices_df) - 1)
	expect_equal(nrow(calc_ms_expanded(df, choices_df)), 12)
	expect_equal(names(calc_ms_expanded(df, qtable)), c("id", "question_id.x",
		"country_id", "coder_id", "historical_date", "text_answer", "confidence",
		"year", "data_type", "name", "class.x", "question_type.x", "question_id.y",
		"class.y", "question_type.y", "cb_responses", "choice_values",
		"question_id", "code", "class", "question_type"))
	expect_equal(ncol(calc_ms_expanded(cbind(df[,-4], coder = c(1111, 1111)), qtable)), 21)
	})

#
# Test dirty_names
# --------------------------------------------------------------------------
test_that("dirty_names", {
	expect_error(dirty_names(data.frame()))
	expect_error(dirty_names(data.frame(
		name = c("v2help_1", "v2help_3", "v2help"),
		cb_responses = c("0 1 2 3 4 5", "0 1 2 3 4 5", "0 1 2 3 4 5")
		)))
	expect_equal(dirty_names(data.frame(
		name = c("v2help_1", "v2help_3", "v2help_8"),
		cb_responses = c("0 1 2 3 4 5", "0 1 2 3 4 5", "0 1 2 3 4 5")
		)), "v2help_8")
	})

#
# Test remove_dirty
# --------------------------------------------------------------------------
test_that("remove_dirty", {
	expect_error(remove_dirty(data.frame(), "v2help_8", qtable))
	expect_error(remove_dirty(df, "v2help_8", data.frame()))
	expect_error(remove_dirty(df, "v2help_8", qtable[-1,]))	
	expect_equal(remove_dirty(df, "", qtable), df)		
	expect_equal(remove_dirty(df, "v2help_8", qtable), df)
	})

#
# Test append_ms_expanded
# --------------------------------------------------------------------------
ms_expanded <- calc_ms_expanded(df, choices_df)
test_that("append_ms_expanded", {
	# n col, n rows, no question_id in df
	expect_error(append_ms_expanded(data.frame(), ms_expanded, qtable))
	expect_error(append_ms_expanded(df, data.frame(), qtable))
	expect_error(append_ms_expanded(df, ms_expanded, data.frame()))
	expect_error(append_ms_expanded(cbind(df[, -2], question_id = c(1311, NA_real_)), 
		ms_expanded, qtable))
	expect_equal(ncol(append_ms_expanded(cbind(df[, -2], question_id = c(1311, NA_real_)), 
			ms_expanded[grepl("1|3", ms_expanded$name),], qtable)), 16)
	expect_equal(nrow(append_ms_expanded(cbind(df[, -2], question_id = c(1311, NA_real_)), 
			ms_expanded[grepl("1|3", ms_expanded$name),], qtable)), nrow(df) * 2)
	})