library(testthat)

#
# Test dichotomize_var
# --------------------------------------------------------------------------
qtable <- data.frame(question_id = c(1311, 9999, 9998),
	name = c("v2help", "v2help_1", "v2help_3"),
	class = c("C", "C", "C"),
	question_type = c("M", "M", "M"))

df <- data.frame(id = c(111, 112),
	question_id = c(1311, 1311),
	country_id = c(11, 11),
	coder_id = c(1111, 1111),
	historical_date = as.Date(c("2020-12-31", "2020-12-31"), format = "%Y-%m-%d"),
	code = c(3, 1),
	text_answer = c(NA_character_, NA_character_),
	confidence = c(100, 100),
	year = c(2020, 2020),
	data_type = c("normal", "normal")
	)


test_that("dichotomize_var", {
	expect_error(dichotomize_var(data.frame(), qtable, "v2help"))
	expect_equal(ncol(dichotomize_var(df, qtable, "v2help")), ncol(df) + 3)
	expect_equal(names(dichotomize_var(df, qtable, "v2help")), c("id", "question_id",
		"country_id", "coder_id", "historical_date", "code", "text_answer", "confidence",
		"year", "data_type", "name", "class", "question_type"))
	expect_equal(nrow(dichotomize_var(df, qtable, "v2help")), nrow(df))
	})

#
# Test category_vector
# --------------------------------------------------------------------------
test_that("category_vector", {
	qtable$cb_responses <- ""
	expect_error(category_vector(qtable))
	expect_error(category_vector(qtable, "v3help"))
	expect_error(category_vector(qtable, "v2help"))
	qtable$cb_responses <- "0 1 2 3 4 5"
	expect_equal(category_vector(qtable, "v2help"), c(0, 1, 2, 3, 4, 5))
	expect_equal(length(category_vector(qtable, "v2help")), 6)
	})

#
# Test prep_dich
# --------------------------------------------------------------------------
df_dich <- dichotomize_var(df, qtable, "v2help")
qtable$cb_responses <- "0 1 2 3 4 5"
test_that("prep_dich", {
	expect_error(prep_dich(c(0, 1, 2, 3), df_dich, qtable))
	expect_error(prep_dich(c(1, 3), df_dich))
	expect_error(prep_dich(category = c(1, 3), qtable = qtable))
	expect_error(prep_dich(list(1, 3), df_dich, qtable))
	expect_error(prep_dich(list(3, 1), df_dich, qtable))

	out <- df_dich; out$question_id <- c(9999, 9998);
	out$name <- c("v2help_1", "v2help_3"); out$code <- c(1, 1)
	out$id <- c(112, 111)
	expect_equal(prep_dich(c(1, 3), df_dich, qtable), out)
	})

#
# Test prep_out
# --------------------------------------------------------------------------
df_prep <- prep_dich(c(1, 3), df_dich, qtable)
col_names <- colnames(df)

# this, actually, could replace the function being tested and become a one-liner
out <- split(select(df_prep, one_of(col_names)), as.factor(df_prep$name))

test_that("prep_out", {
	expect_equal(prep_out(df_prep, col_names), out)
	expect_equal(length(prep_out(df_prep, col_names)), length(out))
	expect_error(prep_out(df_prep, ""))
	expect_error(prep_out(df_prep[, -11], col_names))
	})