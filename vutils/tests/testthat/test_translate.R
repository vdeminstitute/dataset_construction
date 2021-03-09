ttable <- data.frame(question_id = c(1, 2, 3),
                    name = c("q1", "q2", "q3"),
                    label = c("Short text 1", "Short text 2", "Short text 3"),
                    stringsAsFactors = FALSE)

test_that("to_qids", {
    x <- c("q1", "q2", "q3")

    expect_equal(to_qids(x, ttable), c(1, 2, 3))
    expect_equal(to_qids(c("q2", "q3"), ttable), c(2, 3))

    expect_error(to_qids("something_else"))
    expect_error(to_qids(., ttable))
    expect_error(to_qids(x, .))
})

test_that("to_qnames", {
    x <- c("q1", "q2", "q3")

    expect_equal(to_qnames(c(1, 2, 3), ttable), x)
    expect_equal(to_qnames(c(2, 3), ttable), c("q2", "q3"))

    expect_error(to_qnames(NA))
    expect_error(to_qnames(., ttable))
    expect_error(to_qnames(x, .))
})

test_that("to_qlabels", {
    x <- c("q2", "unknown_variable", NA)
    y <- c("Short text 2", "", "")

    expect_equal(to_qlabels(x, ttable), y)

    expect_error(to_qlabels(x))
    expect_error(to_qlabels(x, data.frame()))
})

# to_cids(), to_cnames(), and to_ctext_ids() tests
ttable <- data.frame(country_text_id = c("AFG", "AGO", "ALB"),
                    country_id = c(1, 2, 3),
                    name = c("Afghanistan", "Angola", "Albania"),
                    stringsAsFactors = FALSE)

x <- c("AFG", "AGO", "ALB")
y <- c(1, 2, 3)
z <- c("Afghanistan", "Angola", "Albania")

test_that("to_cids", {
    expect_equal(to_cids(x, ttable), y)
    expect_equal(to_cids(c("AGO", "ALB"), ttable), c(2, 3))

    expect_error(to_cids(c("SWE", "ALB", "AGO"), ttable))
    expect_error(to_cids(z, ttable))
    expect_error(to_cids(c("A", "AL"), ttable))
})


test_that("to_cnames", {
    expect_equal(to_cnames(x, ttable), z)
    expect_equal(to_cnames(c("ALB", "AGO"), ttable), c("Albania", "Angola"))
    expect_equal(to_cnames(c("AGO", "ALB"), ttable), c("Angola", "Albania"))
})

test_that("to_ctext_ids", {
    expect_equal(to_ctext_ids(y, ttable), x)
    expect_equal(to_ctext_ids(c(2, 3), ttable), c("AGO", "ALB"))

    expect_error(to_ctext_ids(4, ttable))
})
