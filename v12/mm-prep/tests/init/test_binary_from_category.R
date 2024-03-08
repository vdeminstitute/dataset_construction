library(testthat)

#
# Test append_df
# --------------------------------------------------------------------------
df <- data.frame(question_id = 1311,
	code = 3)

qtable <- data.frame(question_id = c(1311, 1344),
	name = c("v2help", "v2helpbin"),
	recode_category_to_binary = c(5, 5))


test_that("append_df", {
	expect_error(append_df(data.frame(), qtable))
	expect_error(append_df(df, data.frame()))
	expect_equal(ncol(append_df(df, qtable)), ncol(df) + 2)
	expect_equal(nrow(append_df(df, qtable)), nrow(df))
	expect_equal(append_df(df, qtable), left_join(df, qtable, "question_id"))
	expect_error(append_df(df, transform(qtable, question_id = 1322)))
	})

#
# Test binary_version
# --------------------------------------------------------------------------
df <- append_df(df, qtable)

test_that("binary_version", {
	expect_error(binary_version(data.frame(), qtable), "code")
	expect_error(binary_version(df, data.frame()), "Missing")
	expect_equal(binary_version(df, qtable),
		transform(df, question_id = 1344, code = 1, name = "v2helpbin"))
	expect_equal(binary_version(transform(df, code = 5), qtable),
		transform(df, question_id = 1344, code = 0, name = "v2helpbin"))
	expect_equal(names(binary_version(df, qtable)),
		c("question_id", "code", "name", "recode_category_to_binary"))
	expect_equal(ncol(binary_version(df, qtable)), ncol(df))
	expect_equal(nrow(binary_version(df, qtable)), nrow(df))
	expect_equal(class(binary_version(df, qtable)), "data.frame")
	})

#
# Test binary_transform
# --------------------------------------------------------------------------
test_that("binary_transform", {
	expect_error(binary_transform(data.frame()))
	expect_equal(binary_transform(df), df[,-4])
	expect_equal(binary_transform(transform(df, code = 5)),
		data.frame(question_id = numeric(),
			code = logical(),
			name = character()))
	expect_equal(binary_transform(transform(df, name = "v2mecenefi")),
		transform(df, code = 2, name = "v2mecenefi")[,-4])
	expect_equal(binary_transform(transform(df, name = "v2lgdsadlo")),
		transform(df, code = 2, name = "v2lgdsadlo")[,-4])
	expect_error(binary_transform(df[,-4]), "filter")
	expect_equal(ncol(binary_transform(df)), ncol(df) - 1)
	expect_equal(nrow(binary_transform(df)), nrow(df))
	expect_equal(nrow(binary_transform(transform(df, code = 5))), 0)
	})