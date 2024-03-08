library(testthat)

# be careful, this function sorts the data.frame!
test_that("data.frame interpolation", {
    df <- data.frame(
        country_text_id = c( "DEU", "DEU", "DEU", "USA", "USA"),
        historical_date = as.Date(c("1950-01-01", "1950-06-12", "1950-12-31",
            "1900-01-01", "1900-06-12")),
        year = c(1950,1950,1950,1900,1900),
        code1 = c(1, NA, 2, NA, NA),
        code2 = c(NA, NA, 3, NA, 2),
        code3 = c(3, NA, NA, 1, NA),
        stringsAsFactors = F)

    out <- data.frame(
        country_text_id = c( "DEU", "DEU", "DEU", "USA", "USA"),
        historical_date = as.Date(c("1950-01-01", "1950-06-12", "1950-12-31",
            "1900-01-01", "1900-06-12")),
        year = c(1950,1950,1950,1900,1900),
        code1 = c(1, 1, 2, NA, NA),
        code2 = c(3, 3, 3, NA, 2),
        code3 = c(3, 3, 3, 1, 1),
        stringsAsFactors = F)
    expect_equal(interpolate_df(df), out)

    # leave one id column out
    expect_error(interpolate_df({dplyr::select(df, -country_text_id)}))
    expect_error(interpolate_df({dplyr::select(df, -year)}))
    expect_error(interpolate_df({dplyr::select(df, -historical_date)}))

    # change the class of historical_date and its format
    df$historical_date <- as.character(df$historical_date)
    expect_error(interpolate_df(df))
    df$historical_date <- as.Date(df$historical_date, format = "%Y-%m-%d")
    df$historical_date <- format(df$historical_date, "%d/%m/%Y")
    expect_error(interpolate_df(df))

    # wrong input type
    df$historical_date <- as.Date(format(strptime(as.character(df$historical_date), "%d/%m/%Y"), "%Y-%m-%d"))
    df_m <- as.matrix(df)
    expect_error(interpolate_df(df_m))
    expect_error(interpolate_df(list()))
    expect_error(interpolate_df(c(1,3,4,5)))

})


#
# Tests for interpolate_df
# --------------------------------------------------------------------------
# interpolate_df sorts the observations, so it's more convenient to order it upfront
df <- data.frame(country_text_id = c(rep("FRA", 3), rep("RUS", 3)),
    historical_date = as.Date(c("1991-12-01", "1991-12-31", "1992-12-31",
        "2000-12-31", "2001-11-24", "2001-12-31")
        , format = "%Y-%m-%d"),
    year = c(1991, 1991, 1992, 2000, 2001, 2001),
    code_111 = c(2, 3, NA, 4, NA, 5),
    code_1112 = c(NA, 3, 4, 3, 3, NA),
    stringsAsFactors = FALSE)

testthat::test_that("expected columns",
    {testthat::expect_error(interpolate_df(df[, -1]))
        testthat::expect_error(interpolate_df(df[, -2]))
        testthat::expect_error(interpolate_df(df[, -3]))
        testthat::expect_error(interpolate_df(df[, -4:-5]))
        })
testthat::test_that("expected dim", {
    testthat::expect_equal(nrow(interpolate_df(df)), 6)
    testthat::expect_equal(ncol(interpolate_df(df)), 5)
    })
testthat::test_that("expected output", {
    # 1 obs in year, which is NA & NA within a year with -12-31 obs
    testthat::expect_equal(interpolate_df(df)[[4]], c(2, 3, NA, 4, 5, 5))
    # 1 obs in year, which is NA & NA at -12-31 with another obs
    df[2, 4] <- NA
    df[5, 4] <- 5
    df[6, 4] <- NA
    testthat::expect_equal(interpolate_df(df)[[4]], c(2, 2, NA, 4, 5, 5))
    })


#
# Tests for interpolate_across_years
# --------------------------------------------------------------------------
df <- data.frame(
        country_text_id = c( "DEU", "DEU", "DEU", "USA", "USA", "USA"),
        historical_date = as.Date(c("1950-01-01", "1950-06-12", "1950-12-31",
            "1899-12-31", "1900-01-01", "1900-06-12")),
        year = c(1950,1950,1950, 1899,1900,1900),
        code1 = c(1, 1, 2, 1, NA, NA),
        code2 = c(3, 3, 3, 2, NA, 2),
        code3 = c(3, 3, 3, 3, 1, 1),
        stringsAsFactors = F)

testthat::test_that("expected dim", {
    testthat::expect_equal(nrow(interpolate_across_years(df)), 6)
    # hm, where is the coder_id column?
    testthat::expect_equal(ncol(interpolate_across_years(df)), 6)
    })
testthat::test_that("expected output", {
	# interpolation across years works only for the cases where 
	# -12-31 value is carried forward if there're missing date before -12-31
	# in the next year
	out <- df
	out[5, 5] <- 2
	testthat::expect_equal(interpolate_across_years(df), out)
    })


df <- data.frame(
        country_text_id = c("DEU", "DEU", "DEU", "DEU", "USA", "USA"),
        historical_date = as.Date(c("1949-12-31", "1950-01-01", "1950-06-12", "1950-12-31",
            "1900-01-01", "1900-06-12")),
        year = c(1949, 1950,1950,1950,1900,1900),
        code1 = c(1, 1, NA, 2, NA, NA),
        code2 = c(NA, NA, NA, 3, NA, 2),
        code3 = c(3, 3, NA, NA, 1, NA),
        code4 = c(1, NA, 2, 2, 2, 2),
        code5 = c(1, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE)

    out <- data.frame(
        country_text_id = c("DEU", "DEU", "DEU", "DEU", "USA", "USA"),
        historical_date = as.Date(c("1949-12-31", "1950-01-01", "1950-06-12", "1950-12-31",
            "1900-01-01", "1900-06-12")),
        year = c(1949, 1950,1950,1950,1900,1900),
        code1 = c(1, 1, 1, 2, NA, NA),
        code2 = c(NA, 3, 3, 3, NA, 2),
        code3 = c(3, 3, 3, 3, 1, 1),
        code4 = c(1, 1, 2, 2, 2, 2),
        code5 = c(1, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE)
 
testthat::expect_equal(interpolate_steps(df), out)

test_that("only interpolate_df works,
	interpolate_across_years should return its output", {
	df <- data.frame(country_text_id = rep("RUS", 5),
		historical_date = as.Date(c("2011-01-21", "2011-12-31", "2012-12-31", 
			"2013-05-12", "2013-12-31"), format = "%Y-%m-%d"),
		year = c(2011, 2011, 2012, 2013, 2013),
		code1 = c(NA_integer_, 2, NA_integer_, 3, NA_integer_),
		code2 = c(2, NA_integer_, 2, NA_integer_, NA_integer_),
		code3 = c(NA_integer_, NA_integer_, 3, NA_integer_, NA_integer_))

	out <- data.frame(country_text_id = rep("RUS", 5),
		historical_date = as.Date(c("2011-01-21", "2011-12-31", "2012-12-31", 
			"2013-05-12", "2013-12-31"), format = "%Y-%m-%d"),
		year = c(2011, 2011, 2012, 2013, 2013),
		code1 = c(2, 2, NA_integer_, 3, 3),
		code2 = c(2, 2, 2, NA_integer_, NA_integer_),
		code3 = c(NA_integer_, NA_integer_, 3, NA_integer_, NA_integer_))

	testthat::expect_equal(interpolate_df(df), out)
	testthat::expect_equal(interpolate_across_years(out), out)
	testthat::expect_equal(interpolate_steps(df), out)
	})

test_that("only interpolate_across_years works,
	interpolate_df should not do anything", {
	df <- data.frame(country_text_id = rep("RUS", 5),
		historical_date = as.Date(c("2011-01-21", "2011-05-31", "2012-12-31", 
			"2013-05-12", "2013-06-30"), format = "%Y-%m-%d"),
		year = c(2011, 2011, 2012, 2013, 2013),
		code1 = c(2, 2, 3, NA_integer_, 3),
		code2 = c(2, 2, 2, NA_integer_, NA_integer_),
		code3 = c(NA_integer_, NA_integer_, 3, NA_integer_, NA_integer_))

	out <- data.frame(country_text_id = rep("RUS", 5),
		historical_date = as.Date(c("2011-01-21", "2011-05-31", "2012-12-31", 
			"2013-05-12", "2013-06-30"), format = "%Y-%m-%d"),
		year = c(2011, 2011, 2012, 2013, 2013),
		code1 = c(2, 2, 3, 3, 3),
		code2 = c(2, 2, 2, NA_integer_, NA_integer_),
		code3 = c(NA_integer_, NA_integer_, 3, NA_integer_, NA_integer_))

	testthat::expect_equal(interpolate_df(df), df)
	testthat::expect_equal(interpolate_across_years(df), out)
	testthat::expect_equal(interpolate_steps(df), out)
	})
