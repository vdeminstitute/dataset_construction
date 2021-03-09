library(testthat)

#
# prep_df
# --------------------------------------------------------------------------
df_ll <- list(
	cy = data.frame(
		country_text_id = c("A", "A", "B"),
		year = c(1234, 2345, 3456),
		col1 = c(1, 2, 3),
		col2 = c(1, NA, 2),
		col3 = c(NA, NA, 2)),
	cd = data.frame(country_text_id = c("A", "A", "B"),
		historical_date = c("1234-12-31", "2345-12-31", "3456-12-31"),
		col1 = c(1, 2, 3),
		col2 = c(1, NA, 2),
		col3 = c(NA, NA, 2))
	)

country <- data.frame(
	country_id = c(11, 12, 13),
	country_text_id = c("A", "B", "C")
	)


test_that("prep_df", {
	expect_equal(names(prep_df(df_ll, country, "cy")), c("country_text_id", "year", "col1", "col2", "col3", "country_id", "historical_date"))
	expect_equal(names(prep_df(df_ll, country, "cd")), c("country_text_id", "historical_date", "col1", "col2", "col3", "country_id", "year"))
	})

#
# prep_df_coders
# --------------------------------------------------------------------------
df_coders <- data.frame(
	country_id = c(11, 12, 13),
	year = c(1234, 2345, 3456),
	col1_bool = c(TRUE, FALSE, TRUE),
	col2_bool = c(TRUE, FALSE, TRUE),
	col3_bool = c(TRUE, FALSE, FALSE)
	)


test_that("prep_df_coders", {
	expect_equal(prep_df_coders(data.frame(), 3, "v2help"), data.frame())
	expect_equal(prep_df_coders(df_coders, NA, "v2help"), data.frame(country_id = numeric(),
		year = numeric()))
	out <- df_coders[c(TRUE, FALSE, TRUE),1:2]
	rownames(out) <- NULL
	expect_equal(prep_df_coders(df_coders, 1, "v2help"), out)
	expect_equal(prep_df_coders(df_coders, 1, ""), out)
	expect_equal(prep_df_coders(df_coders, 3, "v2help"), out)
	})

#
# final_prep
# --------------------------------------------------------------------------
df <- prep_df(df_ll, country, "cy")
df_coders <- prep_df_coders(df_coders, 3, 'v2help')

test_that("final_prep", {
	expect_equal(final_prep(df, data.frame()), df)
	out <- df[-1,]
	rownames(out) <- NULL
	expect_equal(final_prep(df, df_coders), out)
	})