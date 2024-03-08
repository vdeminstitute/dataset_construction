library(testthat)

#
# Test remove_sequential()
# --------------------------------------------------------------------------
qtable <- data.frame(
	question_id = c(1113, 1213),
	class = c("C", "B"),
	disaggregated = c(FALSE, FALSE),
	min = c(0, 0),
	code_col = c(TRUE, FALSE),
	text_col = c(FALSE, TRUE),
	question_type = c("M", "T"),
	cb_section = c("a", "b"),
	name = c("v2help", "v2helop"),
	backfill_question = c(TRUE, FALSE)
	)

country <- data.frame(country_id = c(11, 12),
	country_name = c("Russia", "USA"),
	codingstart_contemp = c(1900, 1941),
	codingstart_hist = c(1867, 1811),
	parent_country_id = c(NA, 77),
	end_date = c(1993, 2004)
	)

df <- data.frame(
	id = c(1, 2, 3),
	question_id = c(1113, 1213, 1113),
	coder_id = c(1, 2, 3),
	code = c(3, NA, 2),
	text_answer = c("", "Answer", ""),
	country_id = c(213, 110, 374),
	historical_date = as.Date(c("1992-12-13", "1993-12-31", "2007-12-31"),
		"%Y-%m-%d"),
	year = c(1992, 1993, 2007),
	timestamp = rep("2017-06-15 16:53:31.308500", 3),
	data_type = c("normal", "normal", "normal")
	)

coder <- data.frame(
	coder_id = c(1, 2, 3, 4),
	backfill_coder = c(TRUE, FALSE, TRUE, TRUE),
	test_coder = c(FALSE, FALSE, FALSE, TRUE)
	)

test_that("remove_sequential", {
	expect_error(remove_sequential(data.frame(), "v2help", qtable, coder))
	expect_error(remove_sequential(df, "", qtable, coder))
	expect_error(remove_sequential(df, "v2help", data.frame(), coder))
	expect_error(remove_sequential(df, "v2help", qtable, data.frame()))
	expect_error(remove_sequential(df, "v3help", qtable, coder))
	expect_error(remove_sequential(df, "v2help", transform(qtable, backfill_question = c(NA, TRUE)), coder))
	
	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")
	
	expect_error(remove_sequential(df, "v2help", qtable, coder), "dirtylist")

	dirtylist <<- list()

	out <- df[-1,]
	rownames(out) <- NULL
	expect_equal(remove_sequential(df, "v2help", qtable, coder), out)
	

	expect_equal(remove_sequential(df, "v2helop", qtable, coder), df)
	})

#
# Test recoding_country_ids()
# --------------------------------------------------------------------------

test_that("recoding_country_ids", {
	expect_error(recoding_country_ids(data.frame()))
	expect_equal(recoding_country_ids(df), transform(df, country_id = c(110, 110, 197)))
	expect_equal(recoding_country_ids(transform(df,
		country_id = c(NA, 213, 294)
		)), transform(df, country_id = c(NA, 110, 294)))
	})
	
#
# Test merge_reference_tables()
# --------------------------------------------------------------------------

test_that("merge_reference_tables", {
	expect_error(merge_reference_tables(data.frame(), qtable, country))
	expect_error(merge_reference_tables(df, data.frame(), country))
	expect_error(merge_reference_tables(df, qtable, data.frame()))

	expect_equal(ncol(merge_reference_tables(df, qtable, country)),
		ncol(df) + {ncol(country) - 1} + {ncol(qtable) - 2})
	expect_equal(nrow(merge_reference_tables(df, qtable, country)), nrow(df))
	expect_equal(names(merge_reference_tables(df, qtable, country)),
		c("id", "question_id", "coder_id", "code", "text_answer", "country_id",
			"historical_date", "year", "timestamp", "data_type",
			"name", "class", "disaggregated",
                            "min", "code_col", "text_col", "question_type",
                            "cb_section",
                            "country_name", "codingstart_contemp", "codingstart_hist",
                            "parent_country_id", "end_date"))
	expect_equal(merge_reference_tables(df, qtable, country)$country_name, c(NA_character_, NA_character_, NA_character_))
	expect_equal(merge_reference_tables(df, qtable, country)$name, c("v2help", "v2helop", "v2help"))
	expect_error(
		merge_reference_tables(transform(df, country_id = c(NA, 11, 12)),
			transform(qtable, name = c("v2help", "v2zzhelp"),
				class = c("C", "V")),
			country))
	})

#
# Test clean_codingstart_outside()
# --------------------------------------------------------------------------
df <- df %>%
        remove_sequential(., "v2help", qtable, coder) %>%
        recoding_country_ids %>%
        merge_reference_tables(., qtable, country)


test_that("clean_codingstart_outside", {
	# empty codingstart cols
	expect_error(clean_codingstart_outside(data.frame(), "v2help"))

	expect_equal(nrow(clean_codingstart_outside(df, "v2help")), 0)
	expect_equal(nrow(clean_codingstart_outside(
		transform(df, codingstart_contemp = c(1900, 1900),
			codingstart_hist = c(1888, 1888)),
		"v2help")), 2)
	expect_error(clean_codingstart_outside(
		transform(df, codingstart_contemp = c(1900, 1900),
			codingstart_hist = c(1888, 1888), id = c(1, 1)), 
		"v2help"))

	objs <- ls(pos = ".GlobalEnv")
	rm(list = objs[grep("dirtylist", objs)], pos = ".GlobalEnv")
	
	expect_error(clean_codingstart_outside(
		transform(df, codingstart_contemp = c(1900, 1900),
			codingstart_hist = c(1888, 1888)), 
		"v2help"), "dirtylist")

	dirtylist <<- list()
	})

#
# Test recode_and_remove_coders
# --------------------------------------------------------------------------

test_that("recode_and_remove_coders", {
	expect_error(recode_and_remove_coders(data.frame(), coder))
	expect_error(recode_and_remove_coders(df, data.frame()))

	expect_equal(nrow(
		recode_and_remove_coders(df,
			transform(coder, test_coder = c(FALSE, TRUE, FALSE, FALSE))
			)
		), 1)

	expect_equal(recode_and_remove_coders(df,
			transform(coder, test_coder = c(FALSE, TRUE, FALSE, FALSE))
			)$coder_id, 3)

	expect_equal(
		nrow(recode_and_remove_coders(
					transform(df, coder_id = c(4248, 4387)), coder
					)), 2
		)

	expect_equal(
		recode_and_remove_coders(
					transform(df, coder_id = c(4248, 4387)), coder
					)$coder_id, c(636, 419)
		)
	expect_equal(
		nrow(recode_and_remove_coders(
					transform(df, coder_id = c(831, 2),
						country_id = c(40, 11),
						cb_section = c("wsm", "b")), coder
					)), 1
		)
	})

#
# Test clean_hist_overlap
# --------------------------------------------------------------------------

test_that("clean_hist_overlap", {
	expect_error(clean_hist_overlap(data.frame()))
	expect_equal(nrow(clean_hist_overlap(df)), nrow(df))
	expect_equal(ncol(clean_hist_overlap(df)), ncol(df))

	expect_equal(
		clean_hist_overlap(
			transform(df, end_date = c(1993, 2004),
				parent_country_id = c(77, 23))
			)$country_id, c(110, 23)
		)	
	expect_equal(clean_hist_overlap(df)$country_id, df$country_id)
	})

#
# Test clean_text_answer
# --------------------------------------------------------------------------

test_that("clean_text_answer", {
	expect_error(clean_text_answer(data.frame()))
	expect_equal(
		clean_text_answer(df)$text_answer
		, c("Answer", NA))

	expect_equal(
		clean_text_answer(
			transform(df, text_answer = c("\n\rAnswer", ""))
			)$text_answer,
		c("Answer", NA)
		)
	})

#
# Test wrong_column
# --------------------------------------------------------------------------
df <- clean_text_answer(df)

test_that("wrong_column", {
	# dirty code when text_col=T
	expect_error(wrong_column(data.frame()))

	expect_equal(wrong_column(df), df)
	expect_equal(
		wrong_column(
			transform(df, text_answer = c("", "Answer"))
			)$text_answer, c("", NA)
		)
	expect_equal(
		wrong_column(
			transform(df, code = c(2, 2))
			)$code, c(NA, 2)
		)
	})

#
# Test juflow_recoding
# --------------------------------------------------------------------------

test_that("juflow_recoding", {
	expect_error(juflow_recoding(data.frame()))

	expect_equal(juflow_recoding(df), df)
	expect_equal(juflow_recoding(
		transform(df, question_id = c(2798, 2898),
			text_answer = c("yes", "no"))
		)$code, c(1, 2))
	expect_equal(juflow_recoding(
		transform(df, question_id = c(2798, 2898),
			text_answer = c("yes", "no"))
		)$text_answer, c(NA_character_, "no"))

	expect_equal(class(juflow_recoding(df)$text_answer), "character")
	expect_equal(class(juflow_recoding(df)$code), "numeric")
	})

#
# Test hist_elsnlsff_clrgunev_recoding
# --------------------------------------------------------------------------

test_that("hist_elsnlsff_clrgunev_recoding", {
	expect_error(hist_elsnlsff_clrgunev_recoding(data.frame()))

	expect_equal(hist_elsnlsff_clrgunev_recoding(df), df)
	# code == 0, 2
	expect_equal(
		hist_elsnlsff_clrgunev_recoding(
			transform(df, question_id = c(2464, 2505),
				code = c(0, 2))
			)$code, c(2, 0) 
		)

	expect_equal(
		hist_elsnlsff_clrgunev_recoding(
			transform(df, question_id = c(2464, 5),
				code = c(0, 3))
			)$code, c(2, 3) 
		)

	expect_equal(
		hist_elsnlsff_clrgunev_recoding(
			transform(df, question_id = c(5, 2505),
				code = c(3, 2))
			)$code, c(3, 2) 
		)

	expect_equal(class(hist_elsnlsff_clrgunev_recoding(
			df
			)$code), "numeric")

	})

#
# Test B_recoding
# --------------------------------------------------------------------------

test_that("B_recoding", {
	expect_error(B_recoding(data.frame()))

	expect_equal(
		B_recoding(
			transform(df, question_id = c(560, 564),
				historical_date = as.Date(c("1913-12-31", "1916-12-31"), "%Y-%m-%d"))
			)$code, c(0, 1)
		)

	expect_equal(
		B_recoding(
			transform(df, question_id = c(560, 564),
				historical_date = as.Date(c("1913-06-31", "1916-12-31"), "%Y-%m-%d"))
			)$code, c(NA, 1)
		)

	expect_equal(
		B_recoding(
			transform(df, question_id = c(8, 564),
				historical_date = as.Date(c("1913-12-31", "1916-12-31"), "%Y-%m-%d"))
			)$code, c(NA, 1)
		)

	expect_equal(
		B_recoding(
			transform(df, question_id = c(560, 564),
				historical_date = as.Date(c("1913-12-31", "1916-06-31"), "%Y-%m-%d"))
			)$code, c(0, 2)
		)

	expect_equal(
		B_recoding(
			transform(df, question_id = c(560, 5),
				historical_date = as.Date(c("1913-12-31", "1916-06-31"), "%Y-%m-%d"))
			)$code, c(0, 2)
		)

	expect_equal(class(B_recoding(df)$code), "numeric")
	})

#
# Test jupack_recoding
# --------------------------------------------------------------------------

test_that("jupack_recoding", {
	expect_error(jupack_recoding(data.frame()))

	expect_equal(jupack_recoding(df), df)
	expect_equal(
		jupack_recoding(
			transform(df, code = c(4, 0), question_id = c(580, 580))
			)$code, c(3, 0)
		)
	expect_equal(
		jupack_recoding(
			transform(df, code = c(0, 4), question_id = c(580, 580))
			)$code, c(0, 3)
		)

	})

#
# Test flag_historical_observations
# --------------------------------------------------------------------------

test_that("flag_historical_observations", {
	expect_error(flag_historical_observations(data.frame()))

	expect_equal(flag_historical_observations(df), df)
	expect_equal(
		flag_historical_observations(
			transform(df, name = c("v3help", "v2help"))
			)$data_type, c("historical", "normal")
		)
	expect_equal(
		flag_historical_observations(
			transform(df, name = c("v2help", "v3help"))
			)$data_type, c("normal", "historical")
		)

	expect_error(flag_historical_observations(
		transform(df, name = c(NA, "v2help"))))
	
	expect_equal(class(flag_historical_observations(df)$data_type), "character")
	})