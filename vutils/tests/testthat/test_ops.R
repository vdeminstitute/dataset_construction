
test_that("%||%", {
    expect_equal(NA %||% "me", "me")
    expect_equal(NULL %||% "me", "me")
    expect_equal("me" %||% NA, "me")
    expect_equal("me" %||% NULL, "me")
    expect_equal(c(NA, NULL, 1) %||% c(2, 3, 2), c(NA, 1))
    expect_equal(c(1, 2, NA) %||% c(2, 3, 2), c(1, 2, NA))
    expect_equal(c(1, NULL, 2) %||% c(2, 3, 2), c(1, 2))
    expect_equal(data.frame() %||% "hello", "hello")
    expect_equal(c(1,2,3) %||% "me", c(1,2,3))
    expect_equal(var_does_not_exist %||% "me", "me")
    expect_equal(c(NA, 1) %||% 1, c(NA, 1))
    expect_equal(c(NA, NA) %||% 1, 1)

    # Test missing and undefined objects
    expect_equal(sdf %||% 1, 1)
    expect_equal(Call[["x"]] %||% F, F)

    l <- list(x = 1)
    expect_identical(l %||% "Nope", l)
    expect_equal(l$x %||% "World", 1)
})

test_that("%^%", {

	expect_equal("a" %^% "b", "ab")
	expect_equal(0 %^% "b", "0b")
	expect_equal(0 %^% 0, "00")
	expect_equal(c("a", "b") %^% c("a", "b"), c("aa", "bb"))
	expect_equal(c("a", "b", "c") %^% c("a", "b"), c("aa", "bb", "ca"))

	# I'm OK with this
	expect_equal(NA %^% "a", "NAa")
	expect_equal(NULL %^% "a", "a")

})



