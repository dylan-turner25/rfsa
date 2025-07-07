test_that("get_arcco_actual_revenue basic functionality works", {
  # This test requires the FSA datasets to be available
  
  # Test revenue calculation when MYA price > NMLR (should use MYA price)
  expect_warning(
    revenue_high_mya <- get_arcco_actual_revenue("corn", 2024, mya_price = 5.00, nmlr = 4.50, quiet = FALSE),
    "No location parameters supplied.*national average.*actual yield"
  )
  
  expect_type(revenue_high_mya, "double")
  expect_length(revenue_high_mya, 1)
  expect_gt(revenue_high_mya, 0)
  
  # Test revenue calculation when MYA price < NMLR (should use NMLR)
  revenue_low_mya <- get_arcco_actual_revenue("corn", 2024, mya_price = 4.00, nmlr = 4.50, quiet = TRUE)
  
  expect_type(revenue_low_mya, "double")
  expect_length(revenue_low_mya, 1)
  expect_gt(revenue_low_mya, 0)
  
  # Revenue with higher price should be greater than revenue with lower price
  # (assuming same actual yield is used)
  expect_gt(revenue_high_mya, revenue_low_mya)
})