test_that("clean_names works", {

    test_names <- 
        c("  a", "a  ", "a %", "a", "$a", "$$$a", "GDP ($)", "GDP (us$)", 
          "a (#)", "a & b", "#", "$", "a_cnt", "Aa&Bb", "camelCasePhrases", 
          "AlphaBetaGamma", "Alpha       Beta", "Beta  !!! Gamma", "a + b", 
          "a - b", "a * b")

    expected_lower_case <- 
        c("a", "a", "a_pct", "a", "dollars_a", "dollars_a", 
                  "gdp_dollars", "gdp_us_dollars", "a_cnt", "a_and_b", "cnt", 
                  "dollars", "a_cnt", "aa_and_bb", "camel_case_phrases", 
                  "alpha_beta_gamma", "alpha_beta", "beta_gamma", "a_plus_b", 
                  "a_minus_b", "a_star_b"
        )

    res <- clean_names(test_names, to_lower = TRUE, to_upper = FALSE)
    expect_equal(res, expected_lower_case)

    res <- clean_names(test_names, to_lower = FALSE, to_upper = TRUE)
    expect_equal(res, toupper(expected_lower_case))

})
