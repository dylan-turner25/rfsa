test_that("calc_arcco_payment basic functionality works", {
  # This test requires the FSA datasets to be available

  # Test basic ARC-CO payment calculation with provided values
  result <- calc_arcco_payment(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 4.50,
    nmlr = 2.50,
    oa_benchmark_yield = 180,
    oa_benchmark_price = 4.85,
    actual_revenue = 720,  # 160 yield * 4.50 price = lower than benchmark
    quiet = TRUE
  )

  # Should return a numeric value
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0)  # Payment should never be negative

  # With actual revenue (720) < benchmark revenue (180 * 4.85 = 873) * 0.86 = 750.78
  # Payment should be triggered since 720 < 750.78
  expect_gt(result, 0)
})

test_that("calc_arcco_payment_vectorized basic functionality works", {
  # This test requires the FSA datasets to be available

  # Test basic ARC-CO payment calculation with provided values using vectorized version
  result <- calc_arcco_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 4.50,
    nmlr = 2.50,
    oa_benchmark_yield = 180,
    oa_benchmark_price = 4.85,
    actual_revenue = 720,  # 160 yield * 4.50 price = lower than benchmark
    quiet = TRUE
  )

  # Should return a numeric value
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0)  # Payment should never be negative

  # With actual revenue (720) < benchmark revenue (180 * 4.85 = 873) * 0.86 = 750.78
  # Payment should be triggered since 720 < 750.78
  expect_gt(result, 0)
})

test_that("calc_arcco_payment_vectorized recreates known values from FSA", {
  value <- round(calc_arcco_payment_vectorized(crop = "peanuts",
                                    program_year = 2023,
                                    fips = "01001",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T))

  expect_equal(value, 52)


  value <- round(calc_arcco_payment_vectorized(crop = "wheat",
                                    program_year = 2023,
                                    fips = "45003",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 4.41)


  value <- round(calc_arcco_payment_vectorized(crop = "grain sorghum",
                                    program_year = 2023,
                                    fips = "13027",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 15.34)


  value <- round(calc_arcco_payment_vectorized(crop = "corn",
                                    program_year = 2021,
                                    fips = "46105",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 19.26)


})

test_that("calc_arcco_payment recreates known values from FSA", {
  # This test requires the FSA datasets to be available

  value <- round(calc_arcco_payment(crop = "peanuts",
                     program_year = 2023,
                     fips = "01001",
                     yield_type = "All",
                     cov_lvl = 1,
                     quiet = T))

  expect_equal(value, 52)


  value <- round(calc_arcco_payment(crop = "wheat",
                                    program_year = 2023,
                                    fips = "45003",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 4.41)


  value <- round(calc_arcco_payment(crop = "grain sorghum",
                                    program_year = 2023,
                                    fips = "13027",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 15.34)


  value <- round(calc_arcco_payment(crop = "corn",
                                    program_year = 2021,
                                    fips = "46105",
                                    yield_type = "All",
                                    cov_lvl = 1,
                                    quiet = T),2)

  expect_equal(value, 19.26)



})




