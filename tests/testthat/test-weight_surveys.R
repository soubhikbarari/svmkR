test_that("known target distributions are available", {
  expect_error(get_target("us_genpop_acs18"), NA)
  expect_error(get_target("us_genpop_acs19"), NA)
})

test_that("basic weighting function runs without error", {
  expect_error((wtd <- weight_to(smda23, target = "us_genpop_acs19", verbose = FALSE)) %>%
                 suppressWarnings(), NA)
  expect_error((wtd <- weight_to(smda23, target = get_target("us_genpop_acs19"), verbose = FALSE) %>%
                  suppressWarnings()), NA)
})

test_that("basic weighting function produces sensible weights for SM study", {
  wtd <- weight_to(smda23, target = "us_genpop_acs19", verbose = FALSE) %>%
    suppressWarnings()
  expect_lte(mean(is.na(wtd$weights)), 0.5)
  expect_lte(max(wtd$weights, na.rm=T), 6)
})