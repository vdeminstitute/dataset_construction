test_that("get_root", {
    # Normal C var
    x <- c("v2clacfree", "v2clacfree_osp", "v2clacfree_nr", "v2dlunivl_osp", "v2dd_or_i")
    y <- c(rep("v2clacfree", 3), "v2dlunivl", "v2dd_or_i")

    expect_identical(get_root(x), y)

    # Indices
    x <- c("v2x_freexp", "v2x_freexp_codelow", "v2xlg_elecreg", "v2ex_hosw",
          "v2xdd_or_i", "v2x_freexp_altinf")
    y <- c(rep("v2x_freexp", 2), "v2xlg_elecreg", "v2ex_hosw",
          "v2xdd_or_i", "v2x_freexp_altinf")

    expect_identical(get_root(x), y)

    # Identifiers
    x <- c("country_name", "codingstart_contemp")
    y <- c("country_name", "codingstart_contemp")

    expect_identical(get_root(x), y)

    # E ordinalized indices
    x <- c("e_v2x_freexp_3C", "e_v2x_neopat_codehigh")
    y <- c("e_v2x_freexp_3C", "e_v2x_neopat")

    expect_identical(get_root(x), y)

    # E external variables
    x <- c("e_Civil_war", "e_cowcode", "e_Vanhanen_urban_ipo")
    y <- c("e_Civil_war", "e_cowcode", "e_Vanhanen_urban_ipo")

    expect_identical(get_root(x), y)

    # NA
    expect_equal(get_root(NA), NA_character_)

    # expect class
    expect_equal(class(get_root(1)), "character")
    expect_equal(class(get_root(NA)), "character")
    expect_equal(class(get_root("v2help")), "character")

    # vector length
     x <- c("v2clacfree", "v2clacfree_osp", "v2clacfree_nr", "v2dlunivl_osp", "v2dd_or_i")
    expect_vector(x, size = length(x))
    expect_vector(x)

})

test_that("is.contemp/is.hist", {
    x <- c("v2clacfree", "v3clacfree", "v4clacfree")

    expect_equal(is.contemp(x), c(T, F, F))
    expect_equal(is.hist(x), c(F, T, F))
    expect_equal(is.hist(NA), NA)
})

test_that("is.shared_tag", {
    x <- c("v2clacfree", "v3clacfree", "v3elage", "v2elpdcamp")
    ttable <- data.frame(name = x, stringsAsFactors = F)

    expect_equal(is.shared_tag(x, ttable), c(T, T, F, F))
    expect_equal(is.shared_tag(c("v3clacfree", "v3elage", "v2elpdcamp"), ttable), c(T, F, F))
    expect_error(is.shared_tag("not_in_table", ttable))

    ttable <- data.frame(no_name = x, stringsAsFactors = F)
    expect_error(is.shared_tag(x, ttable), "Missing name column in ttable")
})

test_that("normalize_qids", {
    ttable <- data.frame(name = c("v2clacfree", "v3clacfree", "v2elage",
                                 "v3elage", "v3stflag", "v2elpdcamp"),
                        question_id = 1:6, stringsAsFactors = F)

    expect_equal(normalize_qids(c(1, 2, 3, 4), ttable), c(1, 1, 3, 3))
    expect_error(normalize_qids(c(1, 2, 4, 5, 6), ttable))
    expect_error(normalize_qids(440, ttable))
    expect_error(normalize_qids(1:4))
    expect_error(normalize_qids(1:4, data.frame(name = indicators)))
})

test_that("get_survey", {
    expect_equal(get_survey(c("v2clacfree", "v2dlcountr", "v3partyid")),
                 c("cl", "dl", "pa"))
    expect_error(get_survey(c(1, 2, 3)))
    expect_error(get_survey(c("badtag")))
    expect_error(get_survey(c("v2bad tag")))
    expect_error(get_survey(c("v2x_elecreg")))
    expect_error(get_survey(c("v2BADTAG")))
    expect_error(get_survey(Inf))
    expect_error(get_survey(NA))
    expect_error(get_survey(""))
    expect_error(get_survey(NaN))
    expect_equal(get_survey(c(a = "v2clacfree", b = "v2dlcountr", c = "v3partyid")),
                 c(a = "cl", b = "dl", c = "pa"))
})
