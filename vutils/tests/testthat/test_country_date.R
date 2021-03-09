test_that("is_country_date_regular", {
    expect_true(is_country_date("SWE 1960-12-31"))

    # We're not currently checking if the date is valid
    expect_true(is_country_date("SVN 2002-99-99"))

    expect_equal(is_country_date(c("AFG 1901-01-01", NA_character_)), c(T, F))

    expect_false(is_country_date("historical_1_v1"))
    expect_false(is_country_date("BRN 66-12-31"))
    expect_false(is_country_date("pre 1945-10-11"))
    expect_false(is_country_date("AFG1900-01-01"))
    expect_false(is_country_date("AAAA 1904-02-13"))
    expect_false(is_country_date("abc 1900-01-01"))
    expect_false(is_country_date("AFG 1999-100-99"))
    expect_false(is_country_date(NA))
    expect_false(is_country_date(1))

    expect_equal(is_country_date(NULL), logical(0))
})

test_that("is_country_date_party", {
    expect_true(is_country_date("SWE 1274 1960-12-31", party = TRUE))

    # We're not currently checking if the date is valid
    expect_true(is_country_date("SVN 22222 2002-99-99", party = TRUE))

    expect_equal(is_country_date(c("AFG 1 1901-01-01", NA_character_), party = TRUE), c(T, F))

    expect_false(is_country_date("historical_1_v1", party = TRUE))
    expect_false(is_country_date("BRN 33 66-12-31", party = TRUE))
    expect_false(is_country_date("pre 11 1945-10-11", party = TRUE))
    expect_false(is_country_date("AFG111900-01-01", party = TRUE))
    expect_false(is_country_date("AAAA 1904-02-13", party = TRUE))
    expect_false(is_country_date("abc 33 1900-01-01", party = TRUE))
    expect_false(is_country_date("AFG 33 1999-100-99", party = TRUE))
    expect_false(is_country_date(NA, party = TRUE))
    expect_false(is_country_date(1, party = TRUE))

    expect_equal(is_country_date(NULL, party = TRUE), logical(0))

    expect_false(is_country_date("RUS 1234567 2000-02-02", party = TRUE))
    expect_false(is_country_date("rus 1234567 2000-02-02", party = TRUE))
})

test_that("is_vignette", {
    expect_equal(is_vignette(c("A_v2clacfree_1_v1", "B_v2clacfree_3_v3",
                               "B_v2clacfree_2_v1", "BRA 1871-12-01")),
                 c(T, T, T, F))
    expect_equal(is_vignette(c("A_contemporary_2_v2", NA, "MDG 1900")), c(T, F, F))
})

test_that("get_date_regular", {
    expect_equal(get_date(c("UUU 1900-01-01", "ARG 2016-11-24")),
                 as.Date(c("1900-01-01", "2016-11-24")))
    expect_error(get_date("historical_3_v3"))
    expect_error(get_date(c(NA, "USA 1879-11-10")))
    expect_error(get_date("NAM 1999-99-99"))
})

test_that("get_date_party", {
    expect_equal(get_date(c("UUU 123 1900-01-01", "ARG 123 2016-11-24"), party = TRUE),
                 as.Date(c("1900-01-01", "2016-11-24")))
    expect_error(get_date("historical_3_v3", party = TRUE))
    expect_error(get_date(c(NA, "USA 123 1879-11-10"), party = TRUE))
    expect_error(get_date("NAM 123 1999-99-99", party = TRUE))
    expect_error(get_date("NAM 1999-99-99", party = TRUE))
})

test_that("get_text_id_regular", {
    expect_equal(get_text_id(c("ARG 1906-06-01", "EDD 1789-12-31")), c("ARG", "EDD"))
    expect_error(get_text_id("pilot_1_v1"))
    expect_error(get_text_id(c(NA, "ARG 1906-06-01")))
    expect_error(get_text_id(c("MMMM 1888-08-08")))
})

test_that("get_text_id_party", {
    expect_equal(get_text_id(c("ARG 12 1906-06-01", "EDD 12 1789-12-31"), party = TRUE), c("ARG", "EDD"))
    expect_error(get_text_id("pilot_1_v1", party = TRUE))
    expect_error(get_text_id(c(NA, "ARG 1 1906-06-01"), party = TRUE))
    expect_error(get_text_id(c("MMMM 1888-08-08", party = TRUE)))
})

test_that("sort.country_date", {
    input <- c("AFG 1800-01-01", "A_pilot_2_v1", "A_pilot_1_v1",
              "B_contemporary_2_v2", "AFG 1776-12-31", "B_contemporary_2_v1")
    output <- c("AFG 1776-12-31", "AFG 1800-01-01", "A_pilot_1_v1", "A_pilot_2_v1",
                "B_contemporary_2_v1", "B_contemporary_2_v2")
    expect_equal(sort_text_id(input), output)

    input <- c("AFG 1900-01-01", "USA 1890-12-31", "USA 1891-12-31", "USA 1891-02-03")
    output <- c("AFG 1900-01-01", "USA 1890-12-31", "USA 1891-02-03", "USA 1891-12-31")
    expect_equal(sort_text_id(input), output)

    expect_error(sort_text_id(c("USA 1900-01-01", "USA 1888-08-04", NA)))
})

test_that("to_year", {
    expect_equal(to_year(as.Date("1900-01-01")), 1900L)
    expect_equal(to_year(as.Date("2016-11-24")), 2016L)
    expect_equal(to_year(as.Date(c(NA, "1995-01-01"))), c(NA_integer_, 1995L))
    expect_equal(to_year(as.Date("16-12-14")), 16L) # This is weird, but it's a valid date

    expect_error(to_year(2016))
    # expect_error(to_year("2016-11-24"))
    expect_error(to_year(2016-11-24))
    expect_error(to_year(NA))
})

test_that("create_idx", {
    x <- c(1900, 1901, 1903, 1904, 1906, 1907)
    expect_equal(create_idx(x), c(1, 1, 2, 2, 3, 3))

    # Hope your input is sorted properly
    x <- c(1999, 1998, 1997)
    expect_equal(create_idx(x), c(1, 1, 1))

    x <- as.Date(c("1900-01-01", "1901-11-08", "1901-12-31", "1904-01-01", "1905-01-01"))
    expect_equal(create_idx(x), c(1, 2, 2, 3, 3))

    expect_error(create_idx(c(1900, NA)))
    expect_error(create_idx(as.Date(c("1900-01-01", NA))))
    expect_error(create_idx("1900-01-01"))
    expect_error(create_idx(c(90, 12)))
})

test_that("get_party_id", {
    expect_equal(get_party_id("RUS 123 2000-01-01"), 123)
    expect_error(get_party_id("RUS 1234567 2000-01-01"))
    expect_error(get_party_id("RUS123 2000-01-01"))
    expect_error(get_party_id("RUS 2000-01-01"))
    })