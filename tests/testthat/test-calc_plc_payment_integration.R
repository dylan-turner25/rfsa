test_that("calc_plc_payment integrates with real FSA data", {
  # These tests use actual FSA datasets and may take longer to run
  # Skip if datasets are not available

  skip_on_cran()
  skip_if_not(file.exists("data/fsaMyaPrice.rda"))
  skip_if_not(file.exists("data/fsaEffectiveRefPrices.rda"))
  skip_if_not(file.exists("data/fsaPlcPaymentRate.rda"))
  skip_if_not(file.exists("data/fsaPlcYields.rda"))

  # Test with corn - a crop that should have data
  result_corn <- calc_plc_payment(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  expect_type(result_corn, "double")
  expect_length(result_corn, 1)
  expect_gte(result_corn, 0)

  # Test with soybeans
  result_soybeans <- calc_plc_payment(
    crop = "soybeans",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  expect_type(result_soybeans, "double")
  expect_length(result_soybeans, 1)
  expect_gte(result_soybeans, 0)
})

test_that("calc_plc_payment handles crop types correctly with real data", {
  skip_on_cran()
  skip_if_not(file.exists("data/fsaPlcYields.rda"))

  # Test rice with different crop types
  result_rice_long <- calc_plc_payment(
    crop = "rice",
    crop_type = "long grain",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  result_rice_medium <- calc_plc_payment(
    crop = "rice",
    crop_type = "short/medium grain",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  # Both should be valid
  expect_type(result_rice_long, "double")
  expect_type(result_rice_medium, "double")
  expect_gte(result_rice_long, 0)
  expect_gte(result_rice_medium, 0)

  # Test chickpeas with different sizes
  result_chickpeas_large <- calc_plc_payment(
    crop = "chickpeas",
    crop_type = "large",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  result_chickpeas_small <- calc_plc_payment(
    crop = "chickpeas",
    crop_type = "small",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  expect_type(result_chickpeas_large, "double")
  expect_type(result_chickpeas_small, "double")
  expect_gte(result_chickpeas_large, 0)
  expect_gte(result_chickpeas_small, 0)
})

test_that("calc_plc_payment handles location parameters with real data", {
  skip_on_cran()
  skip_if_not(file.exists("data/fsaPlcYields.rda"))

  # Test with Iowa (major corn producing state)
  expect_warning(
    result_iowa <- calc_plc_payment(
      crop = "corn",
      program_year = 2024,
      base_acres = 100,
      state = "Iowa",
      quiet = FALSE
    ),
    "state average"
  )

  # Test with state abbreviation
  expect_warning(
    result_ia <- calc_plc_payment(
      crop = "corn",
      program_year = 2024,
      base_acres = 100,
      state = "IA",
      quiet = FALSE
    ),
    "state average"
  )

})


test_that("calc_plc_payment consistency across multiple calls", {
  skip_on_cran()
  skip_if_not(file.exists("data/fsaMyaPrice.rda"))

  # Same parameters should give same results
  params <- list(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    quiet = TRUE
  )

  result1 <- do.call(calc_plc_payment, params)
  result2 <- do.call(calc_plc_payment, params)
  result3 <- do.call(calc_plc_payment, params)

  expect_equal(result1, result2)
  expect_equal(result2, result3)
})

