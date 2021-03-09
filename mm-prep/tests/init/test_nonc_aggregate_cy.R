library(testthat)

#
# Test merge_ref_tables
# --------------------------------------------------------------------------
df <- data.frame(country_id = c(11, 11),
	country_text_id = c("RUS", "RUS"),
	country_name = c("Russia", "Russia"),
	historical_date = as.Date(c("2020-01-30", "2020-12-31"), format = "%Y-%m-%d"),
	year = c(2020, 2020),
	v2ddspmor = c(0, 3)
	)

country <- data.frame(country_id = 11,
	country_text_id = "RUS",
	country_name = "Russia")

test_that("merge_ref_tables", {
	expect_equal(merge_ref_tables(df[,-1], country), df[, c(3,4,5,6,1)])
	expect_equal(merge_ref_tables(df[,-3]), df[, -2:-3])
	expect_equal(merge_ref_tables(df[, c(-1, -3)], country), df[, c(4, 5, 6, 1)])
	expect_equal(nrow(merge_ref_tables(df, country)), nrow(df[, -2]))
	})

#
# Test find_aggregation_method
# --------------------------------------------------------------------------
qtable <- data.frame(name = c("v2clacfree", "v2ddspmor"),
	date_specific = c(NA, NA),
	question_type = c("P", "R"),
	index_type = c(NA, "interval")
	)

test_that("find_aggregation_method", {
	expect_error(find_aggregation_method(data.frame(), "v2clacfree"))
	expect_error(find_aggregation_method(qtable, NA))
	expect_error(find_aggregation_method(qtable, "help"))
	# the following two lines exibit dubious function behaviour (ain't much to do about it)
	expect_equal(find_aggregation_method(qtable, "v2help"), "last")
	expect_equal(find_aggregation_method(qtable, "v3help"), "last")
	expect_equal(find_aggregation_method(qtable, "v2clacfree"), "max")
	expect_equal(find_aggregation_method(qtable, "v2ddspmor"), "ratio")
	})

#
# Test aggr_ratio
# --------------------------------------------------------------------------
# for me, the subfunction of cy.day_mean isn't working properly, so I'm skipping testing this function

#
# Test aggr_max
# --------------------------------------------------------------------------
test_that("aggr_max", {
	expect_equal(aggr_max(df), data.frame(country_id = 11,
		year = 2020,
		country_text_id = "RUS",
		country_name = "Russia",
		v2ddspmor = 3))
	expect_equal(class(aggr_max(df)), "data.frame")
	expect_equal(nrow(aggr_max(df)), 1)
	expect_equal(ncol(aggr_max(df)), ncol(df) - 1)
	expect_error(aggr_max(data.frame()))
	})

#
# Test aggr_last
# --------------------------------------------------------------------------
test_that("aggr_last", {
	expect_equal(aggr_last(df), data.frame(country_id = 11,
		year = 2020,
		country_text_id = "RUS",
		country_name = "Russia",
		v2ddspmor = 3))
	expect_equal(class(aggr_last(df)), "data.frame")
	expect_equal(nrow(aggr_last(df)), 1)
	expect_equal(ncol(aggr_last(df)), ncol(df) - 1)
	expect_error(aggr_last(data.frame()))
	})