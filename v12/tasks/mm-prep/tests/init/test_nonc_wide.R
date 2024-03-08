library(testthat)

#
# Test delete_column
# --------------------------------------------------------------------------
df <- data.frame(question_id = c(1311),
	country_id = 11,
	historical_date = as.Date("2020-12-31", format = "%Y-%m-%d"),
	year = 2020)

qtable <- data.frame(question_id = 1311,
	code_col = TRUE,
	text_col = FALSE)


test_that("delete_column", {
	expect_error(delete_column(data.frame(), qtable, "v2help"))
	expect_error(delete_column(df, data.frame(), "v2help"))
	expect_error(delete_column(transform(df, code = c(1)), data.frame(), "v2help"))
	expect_error(delete_column(transform(df, text_answer = c("hello")), data.frame(), "v2help"))
	expect_error(delete_column(transform(df, text_answer = c("hello")), qtable, "help"), "grepl")

	expect_equal(delete_column(transform(df, code = c(1)), qtable, "v2help"),
		data.frame(question_id = c(1311),
	country_id = 11,
	historical_date = as.Date("2020-12-31", format = "%Y-%m-%d"),
	year = 2020, code_col = TRUE, text_col = FALSE, v2help = 1))

	expect_equal(delete_column(transform(df, text_answer = c("hello")),
		transform(qtable, code_col = FALSE, text_col = TRUE), "v2help"),
		data.frame(question_id = c(1311),
	country_id = 11,
	historical_date = as.Date("2020-12-31", format = "%Y-%m-%d"),
	year = 2020, code_col = FALSE, text_col = TRUE, v2help = "hello"))
	})

#
# Test subset_cols
# --------------------------------------------------------------------------
df <- delete_column(transform(df, code = c(1)), qtable, "v2help")

test_that("subset_cols", {
	expect_error(subset_cols(data.frame(), "v2help"))
	expect_error(subset_cols(df, ""))
	expect_error(subset_cols(df, "help"))
	expect_equal(subset_cols(df, "v2help"),
		df[,c("country_id", "historical_date", "year", "v2help")])
	})