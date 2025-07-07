test_that("calc_effective_reference_price basic functionality works", {

  mya_prices <- c(4.72,5.16,4.58,5.05,7.63)
  srp <- 5.50
  result <- calc_effective_reference_price(mya_prices = mya_prices, srp = srp)
  expect_equal(result, 5.50)

  mya_prices <- c(2.82,2.77,4.55,4.57,3.92)
  srp <- 2.40
  result <- calc_effective_reference_price(mya_prices = mya_prices, srp = srp)
  expect_equal(result, 2.76)

  mya_prices <- c(.37,.37,.39,.41,.40)
  srp <- 0.2015
  result <- round(calc_effective_reference_price(mya_prices = mya_prices, srp = srp),4)
  expect_equal(result, .2317)

})
