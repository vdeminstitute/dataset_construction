library(testthat)

#
# Test merge_reference_tables
# --------------------------------------------------------------------------
df <- data.frame(
	id = c(1, 2, 3),
	question_id = c(1113, 1213, 1114),
	country_id = c(11, 12, 23),
	coder_id = c(1, 1, 2),
	historical_date = as.Date(c("1992-12-31", "1994-12-31", "1901-12-31"), "%Y-%m-%d"),
	code = c(2, 0, 4),
	confidence = c(50, 30, 90),
	year = c(1992, 1994, 1901)
	)

qtable <- data.frame(
	question_id = c(1113, 1213, 1114),
	name = c("v2help", "v2helop", "v3helpo"),
	class = c("C", "B", "C"),
	hist_outside_coding = c(FALSE, FALSE, TRUE),
	cont_outside_coding = c(TRUE, FALSE, FALSE),
	question_type = c("M", "M", "S"),
	choice_values = c("0 1 2 3 4 5", "0 1 2 3 4", "0 1 2 3")
	)

country <- data.frame(
	country_id = c(11, 12, 23, 24),
	name = c("Pepe", "Gege", "Gugu", "Bebe"),
	codingstart_hist = c(1883, 1900, 1834, 1845)
	)

country_unit <- data.frame(
	country_id = c(11, 12, 23, 24),
	year = c(1992, 1994, 1993, 2014),
	cu_id = c(1, 2, 3, 4),
	project = c("contemporary-only", "contemporary-only", "historical-only", "contemporary-only"),
	gap_idx = c(2, 3, 4, 5),
	historical_date = c(1, 2, 3, 4) # it doesn't matter what class this column has
	)

test_that("merge_reference_tables", {
	expect_error(merge_reference_tables(data.frame(), qtable, country))
	expect_error(merge_reference_tables(df, data.frame(), coutnry))
	expect_error(merge_reference_tables(df, qtable, data.frame()))

	expect_equal(ncol(merge_reference_tables(df, qtable, country)),
		ncol(df) + 4 + 2)
	expect_equal(nrow(merge_reference_tables(df, qtable, country)), nrow(df))
	expect_equal(class(merge_reference_tables(df, qtable, country)), "data.frame")
	expect_equal(names(merge_reference_tables(df, qtable, country)),
		c(names(df), "name", "class", "hist_outside_coding", "cont_outside_coding",
			"country_name", "codingstart_hist"))
	expect_equal(
		merge_reference_tables(
			transform(df, country_id = c(NA, 12, 13)), qtable, country
			)$country_name, c(NA, "Gege", NA)
		)

	expect_equal(
		merge_reference_tables(
			transform(df, question_id = c(1112, 1111, 1114)), qtable, country
			)$name, c(NA, NA, "v3helpo")
		)
	})

#
# Test remove_historical_na
# --------------------------------------------------------------------------

test_that("remove_historical_na", {
	expect_error(remove_historical_na(data.frame, "v2help"))

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")

	expect_error(remove_historical_na(df, ""))
	expect_error(remove_historical_na(df, "v2help"))

	dirtylist <<- list()
	expect_error(remove_historical_na(df))

	expect_equal(remove_historical_na(df, ""), df)
	expect_equal(
		nrow(remove_historical_na(
					# our df has several questions, so let's not focus on the name
					transform(df, historical_date = c(NA, NA, NA)), ""
					)), 0
		)

	expect_equal(
		remove_historical_na(
			transform(df, historical_date = as.Date(c(NA, "1994-12-31", "1901-12-31"), "%Y-%m-%d")), ""
			)$historical_date, as.Date(c("1994-12-31", "1901-12-31"), "%Y-%m-%d")
		)

	})

#
# Test merge_country_unit
# --------------------------------------------------------------------------
df <- merge_reference_tables(df, qtable, country) %>%
	remove_historical_na(., "")

test_that("merge_country_unit", {
	expect_error(merge_country_unit(data.frame(), country_unit))
	expect_error(merge_country_unit(df, data.frame()))

	expect_equal(ncol(merge_country_unit(df, country_unit)), ncol(df) + 3)
	expect_equal(nrow(merge_country_unit(df, country_unit)), nrow(df))
	expect_equal(merge_country_unit(df, country_unit)$project, c("contemporary-only", "contemporary-only", NA))
	expect_equal(names(merge_country_unit(df, country_unit)),
		c(names(df), "cu_id", "project", "gap_idx"))
	expect_equal(class(merge_country_unit(df, country_unit)), "data.frame")
	})

#
# Test transform_historical
# --------------------------------------------------------------------------
df <- merge_country_unit(df, country_unit)

test_that("transform_historical", {
	expect_error(transform_historical(data.frame()))
	expect_error(
		transform_historical(
			transform(df, name = c("v3help", "v3yelp", "v3helpo"),
				codingstart_hist = rep(NA, 3))
			)
		)

	expect_equal(transform_historical(df), df)
	expect_equal(
		transform_historical(
			transform(df, year = c(1992, 1994, 1833), class = c("C", "B", "B"))
			)$project, c("contemporary-only", "contemporary-only", "pre-historical")
		)
	expect_equal(
		transform_historical(
			transform(df, year = c(1992, 1994, 1834), class = c("C", "B", "B"))
			)$project, c("contemporary-only", "contemporary-only", NA) 
		)
	expect_equal(class(transform_historical(df)), "data.frame")
	})

#
# Test clean_by_country_unit
# --------------------------------------------------------------------------

test_that("clean_by_country_unit", {
	expect_error(clean_by_country_unit(data.frame(), ""))
	expect_error(clean_by_country_unit(df), "VARNAME")
	expect_error(
		clean_by_country_unit(
			transform(df, country_name = c(NA, "Gege", "Pepe")), ""
			), "is not TRUE"
		)
	expect_equal(nrow(clean_by_country_unit(df, "")), nrow(df) - 1)
	expect_equal(ncol(clean_by_country_unit(df, "")), ncol(df))

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")

	expect_error(clean_by_country_unit(df, ""), "dirtylist")

	dirtylist <<- list()

	expect_equal(
		nrow(clean_by_country_unit(
					transform(df, cont_outside_coding = c(TRUE, TRUE, FALSE),
						project = c("contemporary-only", NA, NA)), ""
					)), nrow(df) - 2
		) # first & last cleaning remove rows 2-3

	expect_equal(
		clean_by_country_unit(
					transform(df, cont_outside_coding = c(TRUE, TRUE, FALSE),
						project = c("contemporary-only", NA, NA)), ""
					)$name, "v2help" 
		) # first & last cleaning remove rows 2-3

	expect_equal(
		nrow(
			clean_by_country_unit(
					transform(df, hist_outside_coding = rep(FALSE, 3)), ""
					)
			), nrow(df) - 1
	) # second cleaning removes row 3

	expect_equal(
		clean_by_country_unit(
					transform(df, hist_outside_coding = rep(FALSE, 3)), ""
					)$name, c("v2help", "v2helop")
	) # second cleaning removes row 3

	expect_equal(class(clean_by_country_unit(df, "")), "data.frame")
	})

#
# Test clean_duplicates
# --------------------------------------------------------------------------
df$project[3] <- "overlap"

test_that("clean_duplicates", {
	# hit duplicates in id_cols (A/C vars)
	expect_error(clean_duplicates(data.frame()))
	expect_error(clean_duplicates(data.frame()), "")

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")

	expect_error(clean_duplicates(df, ""), "dirtylist")

	dirtylist <<- list()

	expect_equal(clean_duplicates(df, ""), df)
	expect_equal(class(clean_duplicates(df, "")), "data.frame")
	expect_equal(nrow(clean_duplicates(df, "")), nrow(df))
	expect_equal(ncol(clean_duplicates(df, "")), ncol(df))

	expect_error(
		clean_duplicates(transform(df, project = c(NA, 1, 2)), "")
		)
	expect_error(clean_duplicates(transform(df, id = c(1, 1, 2)), ""))
	expect_error(clean_duplicates(transform(df, class = c("I", "C", "A**")), ""))

	expect_error(
		clean_duplicates(transform(df, class = c("B", "B", "C"),
			question_id = c(1113, 1113, 1114),
			country_id = c(11, 11, 12),
			historical_date = as.Date(c("1994-12-31", "1994-12-31", "1992-12-31"), "%Y-%m-%d")), "")
		)

	expect_equal(
		clean_duplicates(transform(df, class = c("B", "B", "B"),
			question_id = c(1113, 1113, 1114),
			country_id = c(11, 11, 12),
			historical_date = as.Date(c("1994-12-31", "1994-12-31", "1992-12-31"), "%Y-%m-%d")), "")$id, c(2, 3)
		)

	expect_equal(
		clean_duplicates(transform(df, class = c("C", "C", "C"),
			question_id = c(1113, 1113, 1114),
			country_id = c(11, 11, 12),
			historical_date = as.Date(c("1994-12-31", "1994-12-31", "1992-12-31"), "%Y-%m-%d")), "")$id, c(2, 3)
		)

	})

#
# Test percent_cleaning
# --------------------------------------------------------------------------

test_that("percent_cleaning", {
	expect_error(percent_cleaning(data.frame(), qtable, "v2help"))
	expect_error(percent_cleaning(df, data.frame(), "v2help"))
	expect_equal(percent_cleaning(df, qtable, ""),
		left_join(df, qtable[, c("question_id", "question_type", "choice_values")], by = "question_id"))
	expect_equal(class(percent_cleaning(df, qtable, "v2help")), "data.frame")
	expect_equal(nrow(percent_cleaning(df, qtable, "")), nrow(df))
	expect_equal(ncol(percent_cleaning(df, qtable, "")), ncol(df) + 2)

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")

	expect_error(percent_cleaning(df, qtable, ""), "dirtylist")

	dirtylist <<- list()

	expect_equal(
		percent_cleaning(
			transform(df, code = c(101, 0, -1)),
			transform(qtable, question_type = c("R", "R", "R")), ""
			)$id, 2
		)

	})

#
# Test mecenefi_cleaning
# --------------------------------------------------------------------------

test_that("mecenefi_cleaning", {
	expect_error(mecenefi_cleaning(data.frame(), "v2help"))
	expect_equal(mecenefi_cleaning(df, "v2help"), df)	
	expect_error(mecenefi_cleaning(df, "v2mecenefi"))
	expect_equal(nrow(mecenefi_cleaning(transform(df, name = rep("v2mecenefi", 3)),
			"v2mecenefi")), 0)
	expect_equal(
		mecenefi_cleaning(
			transform(df, name = rep("v2mecenefi", 3), code = c(1, 1, 1)), "v2mecenefi"
			)$id, c(2)
		)

	expect_equal(
		mecenefi_cleaning(
			transform(df, name = rep("v2mecenefi", 3),
				code = c(1, 1, 1),
				historical_date = as.Date(c("1993-12-31", "1994-12-31", "1994-12-31"), "%Y-%m-%d"),
				year = c(1993, 1994, 1994)), "v2mecenefi"
			)$id, c(1, 2, 3)
		)

	})

#
# Test percent_multiply
# --------------------------------------------------------------------------

test_that("percent_multiply", {
	expect_error(percent_multiply(data.frame()))
	expect_equal(percent_multiply(df), df)
	expect_equal(nrow(percent_multiply(df)), nrow(df))
	expect_equal(ncol(percent_multiply(df)), ncol(df))
	expect_equal(class(percent_multiply(df)), "data.frame")

	expect_equal(
		percent_multiply(
			transform(df, name = c("v2msuffrage", "v2fsuffrage", "v2asuffrage"))
			)$code, c(200, 0, 400) 
		)

	expect_equal(
		percent_multiply(
			transform(df, code = c(0.4, 0.3, 0.5),
				name = c("v2msuffrage", "v2fsuffrage", "v2asuffrage"))
			)$code, c(40, 30, 50) 
		)

	})

#
# Test clean_confidence
# --------------------------------------------------------------------------

test_that("clean_confidence", {
	# hit negative code
	expect_error(clean_confidence(data.frame(), "v2help"))
	expect_equal(clean_confidence(df, ""), df)
	expect_error(clean_confidence(df))
	expect_equal(class(clean_confidence(df, "v2help")), "data.frame")
	expect_equal(nrow(clean_confidence(df, "v2help")), nrow(df))
	expect_equal(ncol(clean_confidence(df, "v2help")), ncol(df))

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")

	expect_error(clean_confidence(df, ""), "dirtylist")

	dirtylist <<- list()

	expect_error(
		clean_confidence(transform(df, project = c(NA, "overlap", "overlap")), "v2help")
		)
	expect_equal(
		clean_confidence(transform(df, confidence = c(0, 100, 0)), "v2help")$id, 2
		)
	expect_error(
		clean_confidence(transform(df, confidence = c(119, 100, 90)), "v2help")
		)
	expect_error(
		clean_confidence(transform(df, code = c(-3, 0, 2)), "v2help")
		)

	})