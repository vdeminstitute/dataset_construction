# ==========================================================================
# Tests for mm-prep/R/mm_prep/raw_mean.R script
# ==========================================================================

library(testthat)

#
# cy_df()
# --------------------------------------------------------------------------
context("Aggregation to CY version")

cd_df <- data.frame(
	x = as.numeric(c(1.2, 1.2, 1.2, 1, 1))
	)
country_text_id = rep("HTI", 5)
historical_date = as.Date(paste0("1957-", c("01-01", "05-25", "06-14", "10-22", "12-31")), format = "%Y-%m-%d")

out <- data.frame(x = weighted.mean(x = cd_df$x, w = c(as.numeric(diff(historical_date)), 1)),
	year = 1957,
	country_text_id = "HTI")

test_that("Test the result", {
	expect_equal(cy.day_mean(cd_df, dates = historical_date, by = country_text_id, mc.cores = 1),
		out)
	})

