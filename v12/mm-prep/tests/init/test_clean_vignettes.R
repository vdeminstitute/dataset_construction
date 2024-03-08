library(testthat)

#
# Test clean_by_vtable
# --------------------------------------------------------------------------
vtable <- data.frame(question_id = 1411,
	parent_name = "v2clacfree",
	hist_merged = TRUE,
	vignette_id = 3856,
	vignette_name = "A_v2clacfree_01_01__1",
	threshold = 1,
	type = "contemporary",
	id = "A_v2clacfree_01_01__1",
	merge_with_cont = NA_character_,
	hist_merged_id = NA_real_)

vdata_df <- data.frame(id = 1, question_id = 3856, country_id = 4,
	coder_id = 2930, historical_date = as.Date(NA), code = 3, text_answer = NA_character_,
	confidence = NA_real_, data_type = "normal", year = NA_real_)

test_that("clean_by_vtable returns expected output format", {
	expect_equal(nrow(clean_by_vtable(vdata_df, vtable)), 1)
	expect_equal(ncol(clean_by_vtable(vdata_df, vtable)), 15)
	expect_equal(class(clean_by_vtable(vdata_df, vtable)), "data.frame")
	})

test_that("clean_by_vtable returns expected output data", {
	expect_equal(clean_by_vtable(vdata_df, vtable)$code, 3)
	expect_equal(all(sapply(suppressMessages(clean_by_vtable(vdata_df, vtable)), is.na)), FALSE)
	expect_equal(suppressMessages(clean_by_vtable(bind_rows(vdata_df, vdata_df), vtable)),
		suppressMessages(clean_by_vtable(vdata_df, vtable)))
	})

test_that("clean_by_vtable fails with NA or empty input vtable_df data", {
	expect_error(clean_by_vtable(data.frame(), vtable))
	expect_error(clean_by_vtable(data.frame(
		id = NA_integer_,
		question_id = NA_real_,
		country_id = NA_real_,
		coder_id = NA_real_,
		historical_date = NA_real_,
		code = NA_real_,
		text_answer = NA_character_,
		confidence = NA_real_,
		data_type = NA_character_,
		year = NA_real_), vtable))
	})

test_that("clean_by_vtable fails with NA or emty input vtable data", {
	expect_error(clean_by_vtable(vdata_df, data.frame()))
	expect_error(clean_by_vtable(vdata_df, data.frame(
		question_id = NA_real_,
		parent_name = NA_character_,
		hist_merged = NA_real_,
		vignette_id = NA_real_,
		vignette_name = NA_character_,
		threshold = NA_real_,
		type = NA_character_,
		id = NA_character_,
		merge_with_cont = NA_character_,
		hist_merged_id = NA_real_
		)))
	})

#
# Test clean_missing
# --------------------------------------------------------------------------
df <- clean_by_vtable(vdata_df, vtable)
df2 <- bind_rows(df, df)
df2$code[2] <- NA_real_
test_that("clean_missing doesn't have missing values", {
	expect_error(clean_missing(data.frame()))
	expect_equal(clean_missing(df), df)
	expect_equal(nrow(clean_missing(df2)), 1)
	expect_equal(ncol(clean_missing(df)), ncol(df))
	})

#
# Test change_codes
# --------------------------------------------------------------------------

baseline_vars <- list("v2mecenefi" = 0, "v2lgdsadlo" = 0, "v2elffelr" = 5)

test_that("change_codes error behaviour", {
	expect_error(change_codes(df, vtable))
	})

test_that("change_codes returns data format", {
	expect_equal(ncol(change_codes(df, vtable, baseline_vars)), 6)
	expect_equal(nrow(change_codes(df, vtable, baseline_vars)), nrow(df))
	})

#
# Test clean_by_qtable
# --------------------------------------------------------------------------
qtable <- data.frame(question_id = c(1411, 1304),
	cb_responses = c("0 1 2 3 4", "0 1 2 3 4 5"),
	k = c(5, 3))

test_that("clean_by_qtable", {
	expect_equal(ncol(clean_by_qtable(df, qtable, vtable, baseline_vars)), 16)
	expect_equal(nrow(clean_by_qtable(df, qtable, vtable, baseline_vars)), 1)
	})


#
# Test final prep
# --------------------------------------------------------------------------
test_that("final_prep", {
	expect_equal(ncol(final_prep(df, qtable, vtable)), 6)
	expect_equal(nrow(final_prep(df, qtable, vtable)), nrow(df))
	})