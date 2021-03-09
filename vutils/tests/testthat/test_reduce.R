test_that("reduce",{

    # unit-test data.frames with 3-4 rows
    # per country data.frame
    # col.names are code_ conf_
    # NAs instead of -1
    # ordered decreasing by rownames
    # rownames are country_dates
    # wdata and conf_mat are cbinded.

    # row names dont matter because it uses the vector for decisions.


    # test 1: interyear change #
    m_input <- data.frame(code_1 = c(2, NA, 2, 2),
                          code_2 = c(1,2,2,2),
                          code_3 = c(NA,NA,1,NA),
                          row.names = c("USA 1900-12-31", "USA 1901-02-23", "USA 1901-12-31", "USA 1902-12-31"))

    m_input <- as.matrix(m_input[order(rownames(m_input), decreasing = T), ])

    dates <- as.Date(sub("^\\S{3}\\s", "", rownames(m_input)))
    gaps <- c(F,F,F,F)
    intra_year <- format(dates, "%m-%d") != "12-31"

    m_output <- data.frame(code_1 = c(2, 2),
                           code_2 = c(1,2),
                           code_3 = c(NA,1),
                           row.names = c("USA 1900-12-31", "USA 1901-02-23"))

    m_output <- as.matrix(m_output[order(rownames(m_output), decreasing = T), ])

    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # test 2:#
    m_input <- data.frame(code_1 = c(NA,NA,NA),
                          code_2 = c(1,2,2),
                          code_3 = c(NA,NA,1),
                          row.names = c("USA 1900-12-31", "USA 1901-02-23", "USA 1901-12-31"))

    m_input <- as.matrix(m_input[order(rownames(m_input), decreasing = T), ])

    dates <- as.Date(sub("^\\S{3}\\s", "", rownames(m_input)))
    gaps <- c(F,F,F)
    intra_year <- format(dates, "%m-%d") != "12-31"

    m_output <- data.frame(code_1 = c(NA,NA),
                           code_2 = c(1,2),
                           code_3 = c(NA,1),
                           row.names = c("USA 1900-12-31", "USA 1901-02-23"))

    m_output <- as.matrix(m_output[order(rownames(m_output), decreasing = T), ])

    result <- reduce(m_input, gaps, intra_year)

    expect_equal(reduce(m_input, gaps, intra_year), m_output)




    # test 3 #
    m_input <- data.frame(code_1 = c(NA,NA,NA),
                          code_2 = c(1,2,2),
                          code_3 = c(NA,NA,1),
                          row.names = c("ARG 1900-12-31", "USA 1901-02-23", "USA 1901-12-31"))

    m_input <- as.matrix(m_input[order(rownames(m_input), decreasing = T), ])

    dates <- as.Date(sub("^\\S{3}\\s", "", rownames(m_input)))
    gaps <- c(F,F,F)
    intra_year <- format(dates, "%m-%d") != "12-31"

    m_output <- data.frame(code_1 = c(NA,NA),
                           code_2 = c(1,2),
                           code_3 = c(NA,1),
                           row.names = c("USA 1900-12-31", "USA 1901-02-23"))

    m_output <- as.matrix(m_output[order(rownames(m_output), decreasing = T), ])

    result <- reduce(m_input, gaps, intra_year)

    expect_false(identical(reduce(m_input, gaps, intra_year), m_output))


    # test 4: gaps #
    m_input <- data.frame(code_1 = c(NA,NA,NA),
                          code_2 = c(2,2,2),
                          code_3 = c(1,1,1),
                          row.names = seq(1,3))

    m_input <- as.matrix(m_input[order(rownames(m_input), decreasing = T), ])

    # remember this is a reversed table !
    gaps <- c(F,F,T) # first one is always false
    intra_year <- c(F,F,F)

    m_output <- data.frame(code_1 = c(NA,NA),
                           code_2 = c(2,2),
                           code_3 = c(1,1),
                           row.names = c(1,2))

    m_output <- as.matrix(m_output[order(rownames(m_output), decreasing = T), ])


    result <- reduce(m_input, gaps, intra_year)
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    ### MORE TEST CASES ###

    # Testcase 1 #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,2),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,2),
                           row.names = c(2,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 2 #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,2),
                          code_2 = c(1,NA,2),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,2),
                           code_2 = c(1,2),
                           row.names = c(2,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 3 #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,1),
                          code_2 = c(1,NA,2),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,1),
                           code_2 = c(1,2),
                           row.names = c(2,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 4 #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,2),
                          code_2 = c(NA,1,1),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,2),
                           code_2 = c(1,1),
                           row.names = c(2,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 5 #

    m_input <- as.matrix(data.frame(code_1 = c(1,NA,1,2),
                          code_2 = c(NA,1,NA,1),
                          row.names = seq(1,4)))
    gaps <- c(F,F,F,F)
    intra_year <- c(F,F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,2),
                           code_2 = c(1,1),
                           row.names = c(3,4)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)



    # Testcase 6 #

    m_input <- as.matrix(data.frame(code_1 = c(NA,NA,NA,1),
                          code_2 = c(1,1,1,2),
                          row.names = seq(1,4)))
    gaps <- c(F,F,F,F)
    intra_year <- c(F,F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(NA,1),
                           code_2 = c(1,2),
                           row.names = c(3,4)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 7: gaps #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,1),
                          row.names = seq(1,3)))
    gaps <- c(F,T,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,1),
                           row.names = c(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 8: gaps #

    m_input <- as.matrix(data.frame(code_1 = c(1,1,1),
                          row.names = seq(1,3)))
    gaps <- c(T,F,F)
    intra_year <- c(F,F,F)
    m_output <- as.matrix(data.frame(code_1 = c(1),
                           row.names = c(3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 9: intrayear #

    m_input <- as.matrix(data.frame(code_1 = c(1,2,2),
                          code_2 = c(1,NA,2),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(F,T,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,2),
                           code_2 = c(1,2),
                           row.names = c(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 10: intrayear #

    m_input <- as.matrix(data.frame(code_1 = c(1,2,3),
                          code_2 = c(NA,2,NA),
                          row.names = seq(1,3)))
    gaps <- c(F,F,F)
    intra_year <- c(T,F,T)
    m_output <- as.matrix(data.frame(code_1 = c(1,2,3),
                           code_2 = c(2,2,NA),
                           row.names = seq(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)



    # Testcase 11: intrayear and gap combinations #

    m_input <- as.matrix(data.frame(code_1 = c(1,NA,1),
                          code_2 = c(1,2,1),
                          row.names = seq(1,3)))
    gaps <- c(F,F,T)
    intra_year <- c(F,T,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,NA,1),
                           code_2 = c(1,2,1),
                           row.names = seq(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)


    # Testcase 12: intrayear and gap combinations #

    m_input <- as.matrix(data.frame(code_1 = c(1,NA,1),
                          code_2 = c(1,1,1),
                          row.names = seq(1,3)))
    gaps <- c(F,T,F)
    intra_year <- c(F,T,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,1),
                           code_2 = c(1,1),
                           row.names = c(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)



    # Testcase 13: intrayear and gap combinations #

    m_input <- as.matrix(data.frame(code_1 = c(1,NA,2),
                          code_2 = c(1,1,2),
                          row.names = seq(1,3)))
    gaps <- c(F,T,F)
    intra_year <- c(F,T,F)
    m_output <- as.matrix(data.frame(code_1 = c(1,NA,2),
                           code_2 = c(1,1,2),
                           row.names = seq(1,3)))
    expect_equal(reduce(m_input, gaps, intra_year), m_output)
    # want output:
    # m_output <- as.matrix(data.frame(code_1 = c(1,2,2),
    #                       code_2 = c(1,1,2),
    #                       row.names = seq(1,3)))


    # Testcase show that there is intrayear backfilling before anything else
    m_input <- data.frame(code_1 = c(NA,1,1,2),
           row.names = c("USA 1920-01-01", "USA 1920-06-12", "USA 1920-12-31",
                         "USA 1921-12-31"))
    m_input <- as.matrix(m_input[order(rownames(m_input), decreasing = T), , drop = F])
    dates <- rownames(m_input) %>% get_date
    gaps <- c(F, diff(dates) < -366)
    intra_year <- format(dates, "%m-%d") != "12-31"
    m_input <- reduce(m_input, gaps, intra_year)
    m_input <- m_input[order(rownames(m_input)), , drop = F]
    m_output <- as.matrix(data.frame(code_1 = c(1, 2),
                           row.names = c("USA 1920-01-01", "USA 1921-12-31")))
    expect_equal(m_input, m_output)














})
