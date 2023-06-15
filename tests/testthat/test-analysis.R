test_that("design effect calculation works", {
  expect_equal(design_eff(rep(1, n=100)), 1)
  expect_equal(design_eff(c(rep(1, n=100), rep(0.25, n=100))), 1.36)
})

test_that("effective sample size calculation works", {
  expect_equal(n_eff(rep(1, n=100)), 1)
  expect_equal(n_eff(c(rep(1, n=100), rep(0.5, n=100))), 1.8)
})

test_that("margin of error simulation works", {
  expect_equal(simu_moe(n=250), 0.06)
  expect_error(simu_moe(weights=rnorm(n=250)))  
})

test_that("margin of error estimation works", {
  expect_equal(esti_moe(n=100), 0.05)
  expect_error(esti_moe(weights=rnorm(n=250)))  
})
