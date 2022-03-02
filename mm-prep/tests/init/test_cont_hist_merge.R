library(testthat)

#
# Test merge_ref_tables
# --------------------------------------------------------------------------
df <- data.frame(id = c(111, 112, 113, 114),
	question_id = c(1311, 1311, 3311, 3311),
	country_id = c(11, 11, 11, 11),
	coder_id = c(1111, 1111, 2222, 2222),
	historical_date = as.Date(c("2020-07-31", "2020-12-31", "1911-04-30", "1912-12-31"),
		format = "%Y-%m-%d"),
	code = c(3, 3, 2, 1),
	text_answer = rep(NA_character_, 4),
	confidence = c(100, 100, 92, 60),
	year = c(2020, 2020, 1911, 1912),
	data_type = c("normal", "normal", "historical", "historical")
	)

qtable <- data.frame(question_id = c(1311, 3311),
	name = c("v2help", "v3help"),
	class = c("C", "C"),
	hist_merge_conflict = c(FALSE, FALSE),
	hist_merge_no_conflict = c(FALSE, TRUE),
	overlap_use_hist = c(FALSE, FALSE)
	)

country_unit <- data.frame(country_id = c(11),
	year = c(2020, 1911, 1912),
	project = c("contemporary-only", "historical-only", "historical-only")
	)

country <- data.frame(country_id = 11,
	codingstart_hist = 1789
	)

test_that("merge_ref_tables", {
	expect_error(merge_ref_tables(data.frame(), qtable, country_unit, country))
	expect_error(merge_ref_tables(df, data.frame(), country_unit, country))
	expect_error(merge_ref_tables(df, qtable, data.frame(), country))
	expect_error(merge_ref_tables(df, qtable, country_unit, data.frame()))
	expect_error(merge_ref_tables(cbind(df[,-2], question_id = c(NA, rep(1311, 3))), qtable, country_unit, country))
	expect_equal(ncol(merge_ref_tables(df, qtable, country_unit, country)), ncol(df) + 4 + 1 + 1)
	expect_equal(nrow(merge_ref_tables(df, qtable, country_unit, country)), nrow(df))
	})

#
# Test data_checks
# --------------------------------------------------------------------------
df <- merge_ref_tables(df, qtable, country_unit, country)
test_that("", {
	expect_error(data_checks(data.frame()))
	expect_error(data_checks(cbind(df[, -15], project = c(NA, "p", "p", "p"))))
	#expect_error(data_checks(cbind(df[,c(-13, -15)], class = c("A", "A", "A", "A"), 
	#	project = c("overlap", "contemporary", "overlap", "overlap"))))
	expect_error(data_checks(cbind(df[,-14], name = c("help", "v2help", "v3help", "v33help"))))
	})

#
# Test extractv3
# --------------------------------------------------------------------------
input <- cbind(df[,c(-13, -15)], class = c("A", "A", "A", "A"),
			project = c("contemporary-only", "contemporary-only", "overlap", "historical-only"))
inp_qtable <- cbind(qtable[, -6], overlap_use_hist = c("TRUE", "TRUE"))

test_that("extractv3", {
	expect_error(extractv3(data.frame(), "v3help", qtable))
	expect_error(extractv3(cbind(df[,-13], class = c("A", "A", "A", "A")), "", qtable))
	expect_error(extractv3(cbind(df[,-13], class = c("A", "A", "A", "A")), "v2help", data.frame()))
	expect_error(extractv3(
		cbind(df[,c(-13, -12)], class = c("A", "A", "A", "A"),
			hist_merge_no_conflict = c(rep(FALSE, 4)))
		))

 	out <- df[df$project == "historical-only",]
 	rownames(out) <- NULL
	expect_equal(extractv3(df, "", data.frame()), out)
	expect_equal(extractv3(df, "v2help", qtable), out)

	out <- cbind(df[df$project == "historical-only",-13], class = c("A", "A"))
	rownames(out) <- NULL

	expect_equal(
		extractv3(cbind(df[,-13], class = c("A", "A", "A", "A")),
			"v2help", qtable),
		out
		)

	out <- cbind(df[df$project == "historical-only",-13], class = c("A", "A"))
	out <- cbind(out[,-14], project = c("overlap", "historical-only"))
	rownames(out) <- NULL

	expect_equal(
		extractv3(df = input, VARNAME = "v2help", qtable = inp_qtable),
		out)
	})


#
# Test transform_qids
# --------------------------------------------------------------------------
v3 <- extractv3(df, "v2help", qtable)

test_that("transform_qids", {
	expect_error(transform_qids(cbind(v3[,-2], question_id = c(2446, 2556)), qtable))
	expect_error(transform_qids(cbind(v3[,-2], question_id = c(NA, 2344)), qtable))
	out <- transform(v3, question_id = c(1311, 1311))
	expect_equal(transform_qids(v3, qtable), out)
	})

#
# Test extractv2
# --------------------------------------------------------------------------
test_that("extractv2", {
	expect_error(extractv2(df, "help", qtable))
	expect_equal(extractv2(df, "v2help", qtable), df[1:2,])
	expect_equal(extractv2(df, "v3help", qtable), df[1:2,])
	expect_equal(extractv2(input, "v2help", qtable), input[1:2,])
	out <- input[1,]
	rownames(out) <- NULL	
	expect_equal(extractv2(transform(input, project = c("contemporary-only", "overlap", "historical-only", "overlap")), "v2help", inp_qtable), out)
	})

#
# Test remove_duplicates
# --------------------------------------------------------------------------
df <- bind_rows(extractv2(df, "v2help", qtable), v3)

test_that("remove_duplicates", {
	expect_equal(remove_duplicates(df), transform(df, id = 1:nrow(df)))
	expect_error(remove_duplicates(df[,-2]))
	expect_error(remove_duplicates(df[,-3]))
	expect_error(remove_duplicates(df[,-4]))
	expect_error(remove_duplicates(df[,-5]))
	out <- transform(bind_rows(df, df)[c(5,6,7,8),], id = c(5:8))
	expect_equal(remove_duplicates(bind_rows(df, df)), out)
	})

#
# Test eltype_recoding
# --------------------------------------------------------------------------
df_no_dups <- remove_duplicates(df)
eltype_df <- data.frame(
	text_answer = c("2,3,8,9", "1,2,3,5,9", "1,5,6,7")
	)

test_that("eltype_recoding", {
	expect_error(eltype_recoding(data.frame(), "v2eltype"))
	expect_error(eltype_recoding(data.frame(), "v2help"))
	expect_equal(eltype_recoding(df_no_dups, "v2help"), df_no_dups)
	# test eltype trailing commas
	expect_equal(eltype_recoding(eltype_df, "v2eltype"), data.frame(
		text_answer = c("1,5", "1,5,6,7")))
})

#
# Test lgbicam_recoding
# --------------------------------------------------------------------------
test_that("lgbicam_recoding", {
	expect_error(lgbicam_recoding(data.frame(), "v2help"))
	expect_equal(lgbicam_recoding(df, "v2help"), df)
	expect_equal(lgbicam_recoding(
		data.frame(question_id = c(595, 595), code = c(9, 2)),
		"v2lgbicam"
		)$code, c(0, 2))
	expect_equal(lgbicam_recoding(
		data.frame(question_id = c(595, 595), code = c(9, 4)),
		"v2lgbicam"
		)$code, c(0, 2))	
	expect_equal(lgbicam_recoding(data.frame(
		question_id = c(595, 596), code = c(9, 4)),
		"v2lgbicam")$code, c(0, 4)
		)
	expect_equal(ncol(lgbicam_recoding(df, "v2help")), ncol(df))
	expect_equal(lgbicam_recoding(
		data.frame(question_id = c(595, 595), code = c(9, 2)),
		"v2lgbicam"
		), data.frame(question_id = c(595, 595), code = c(0, 2)))
})

