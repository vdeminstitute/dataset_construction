
 test_that("default stretch", {
 	# make dummy cu table
 	utable <- data.frame(
		country_text_id = c(rep("DEU", 10), rep("USA", 4)),
		year = c(seq(1940, 1944, 1), seq(1949, 1953, 1), seq(2013, 2016, 1)),
		gap_idx = c(rep(80, 5), rep(81, 5), rep(21, 4)),
		stringsAsFactors = FALSE
	)
     # general case
     x <- matrix(1:8, nrow = 2, ncol = 4, byrow = T)
     rownames(x) <- c("DEU 1949-12-31", "DEU 1952-12-31")
     by <- c("DEU 1949-12-31", "DEU 1950-12-31", "DEU 1951-12-31", "DEU 1952-12-31")
     # out
     out <- matrix(c(1, 2, 3, 4,
                    1, 2, 3, 4,
                    1, 2, 3, 4,
                    5, 6, 7, 8), nrow = 4, ncol = 4, byrow = T)
     rownames(out) <- by
     expect_equal(stretch(x, by, utable = utable), out)

     # Gaps, gaps, gaps, gaps, gaps, gaps, gaps, gaps
     x <- matrix(1:9, 3, 3, byrow = T,
               dimnames = list(c("DEU 2010-12-31", "DEU 2014-12-31", "DEU 2016-12-31"),
                               NULL))
     by <- c("DEU 2012-12-31", "DEU 2010-12-31", "DEU 2014-12-31",
            "DEU 2017-12-31", "DEU 2015-12-31", "DEU 2016-12-31",
           "DEU 2020-12-31")

     out <- matrix(c(1, 2, 3,
                   NA, NA, NA,
                   4, 5, 6,
                   4, 5, 6,
                  7, 8, 9,
                   7, 8, 9,
                   NA, NA, NA), nrow = 7, ncol = 3, byrow = T)
     rownames(out) <- sort(by)
     expect_equal(stretch(x, by), out)

     # invalid country date format
     x <- matrix(seq(1, 8, 1), nrow = 2, ncol = 4, byrow = T)
     rownames(x) <- c("DEU-2013-12-31", "DEU 2016-12-31")
     by = c("DEU-2013-12-31", "DEU 2014-12-31", "DEU 2015-12-31", "DEU 2016-12-31")
     expect_error(stretch(x, by))

     # invalid rownames
     by = c("USA 2013-12-31", "DEU 2014-12-31", "DEU 2015-12-31", "DEU 2016-12-31")
     expect_error(stretch(x, by))

     # 2 countries
     x <- matrix(seq(1, 16, 1), nrow = 4, ncol = 4, byrow = T)
     rownames(x) <- c("DEU 2013-12-31", "DEU 2016-12-31", "USA 2013-12-31", "USA 2016-12-31")
     by = c("DEU 2013-12-31", "DEU 2014-12-31", "DEU 2015-12-31", "DEU 2016-12-31",
            "USA 2013-12-31", "USA 2014-12-31", "USA 2015-12-31", "USA 2016-12-31")

     # out
     out <- matrix(c(1, 2, 3, 4,
                    1, 2, 3, 4,
                    1, 2, 3, 4,
                    5, 6, 7, 8,
                    9, 10, 11, 12,
                    9, 10, 11, 12,
                    9, 10, 11, 12,
                    13, 14, 15, 16), nrow = 8, ncol = 4, byrow = T)
     rownames(out) <- by
     expect_equal(stretch(x, by), out)

     # countries different gap years and no previous obs
     x <- matrix(seq(1, 16, 1), nrow = 4, ncol = 4, byrow = T)

     rownames(x) <- c("DEU 2013-12-31", "DEU 2015-12-31", "USA 2014-12-31", "USA 2016-12-31")

     by <- c("DEU 2013-12-31", "DEU 2014-12-31", "DEU 2015-12-31", "DEU 2016-12-31",
            "USA 2013-12-31", "USA 2014-12-31", "USA 2015-12-31", "USA 2016-12-31")
     # out
     out <- matrix(c(1, 2, 3, 4,
                    1, 2, 3, 4,
                    5, 6, 7, 8,
                    5, 6, 7, 8,
                    NA, NA, NA, NA,
                    9, 10, 11, 12,
                    9, 10, 11, 12,
                    13, 14, 15, 16), nrow = 8, ncol = 4, byrow = T)
     rownames(out) <- by
     #check
     expect_equal(stretch(x, by), out)

     # Finally, check that we don't error on single col matrix (i.e.,
     # dropping matrix class)
     m <- matrix(1:3, 3, 1)
     rownames(m) <- c("DEU 1910-01-01", "DEU 1908-01-01", "USA 2015-01-01")

     by <- c("DEU 1951-01-01", "DEU 1950-01-01", "DEU 1949-01-01",
                        "USA 2015-01-01", "USA 2016-01-01")

     out <- stretch(m, c("DEU 1910-01-01", "DEU 1909-01-01", "DEU 1908-01-01",
                        "USA 2015-01-01", "USA 2016-01-01"))

     y <- matrix(c(2, 2, 1, 3, 3), 5, 1, dimnames = list(c("DEU 1908-01-01", "DEU 1909-01-01",
                                                          "DEU 1910-01-01", "USA 2015-01-01",
                                                          "USA 2016-01-01")))

     expect_equal(out, y)
 })

 test_that("stretch test function args", {
 	 # make utable
 	 utable <- data.frame(
		country_text_id = rep("AFG", 6),
		year = c(1900:1901, 1903:1906),
		gap_idx = c(rep(1, 2), rep(2, 4)),
		stringsAsFactors = FALSE
	)
     # Gaps
     x <- matrix(1:3, 3, 1, dimnames = list(c("AFG 1901-12-31", "AFG 1904-12-31",
                                             "AFG 1905-01-01"), NULL))
     # by contains a gap date
     by <- c("AFG 1901-12-31", "AFG 1903-12-31", "AFG 1904-12-31", "AFG 1905-01-01", "AFG 1906-11-11")
     y <- matrix(c(1, NA, 2, 3, NA), 5, 1, dimnames = list(by, NULL))

     expect_equal(stretch(x, by), y)
     expect_equal(stretch(x, by, gaps = F), locf(y))
     # test utable
     # no utable
     expect_error(stretch(x, by, rule_366 = FALSE))
     # utable is there
     y[5, 1] <- 3
     expect_equal(stretch(x, by, rule_366 = FALSE, utable = utable), y)

     # Now check if we can preserve NAs in the original matrix
     x <- matrix(c(1, NA, 2), 3, 1, dimnames = list(c("AFG 1900-12-31", "AFG 1901-05-01",
                                                     "AFG 1902-12-31"), NULL))

     by <- c("AFG 1900-12-31", "AFG 1901-05-01", "AFG 1901-12-31",
            "AFG 1902-12-31", "AFG 1903-12-31")
     y <- matrix(c(1, NA, NA, 2, 2), 5, 1, dimnames = list(by, NULL))

     expect_equal(stretch(x, by, utable = utable), y)
     expect_equal(stretch(x, by, preserve.na = F), locf(y))

     # Lastly, test interpolation (ie backfilling when there's a single
     # observation at -12-31)
     x <- matrix(1:2, 2, 1, dimnames = list(c("AFG 1790-12-31", "AFG 1791-12-31"), NULL))

     by <- c("AFG 1790-12-31", "AFG 1791-05-04", "AFG 1791-12-31")
     y <- matrix(c(1, 2, 2), 3, 1, dimnames = list(by, NULL))

     expect_equal(stretch(x, by, interpolate = T), y)

     # Sanity check, behavior should default to normal locf
     y[2, 1] <- 1
     expect_equal(stretch(x, by, interpolate = F), y)

     # Backfilling NA
     x[2, 1] <- NA
     y[2:3, 1] <- NA
     expect_equal(stretch(x, by, preserve.na = T, interpolate = T), y)

     # Except when we don't want to preserve NA
     y[1:3, 1] <- 1
     expect_equal(stretch(x, by, preserve.na = F, interpolate = T), y)

     # Interpolating with intra-year change should just be normal locf
     x <- matrix(1:3, 3, 1, dimnames = list(c("AFG 1902-12-31", "AFG 1903-05-04",
     	"AFG 1903-12-31"), NULL))

     by <- c("AFG 1902-12-31", "AFG 1903-01-02", "AFG 1903-05-04", "AFG 1903-12-31")
     y <- matrix(c(1, 1, 2, 3), 4, 1, dimnames = list(by, NULL))

     expect_equal(stretch(x, by, interpolate = T), y)
 })
