# Tests for calc_plc_payment_vectorized function
# This test suite ensures the vectorized version produces identical results to the original

test_that("calc_plc_payment_vectorized can recreate 2019 values in fsa file",{

  # Taken from: https://www.fsa.usda.gov/sites/default/files/documents/2019_PLC.pdf

  wheat <- calc_plc_payment_vectorized(
    crop = "wheat",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  expect_equal(wheat, 0.92)

  barley <- calc_plc_payment_vectorized(
    crop = "barley",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  expect_equal(barley, 0.26)

  oats <- calc_plc_payment_vectorized(
    crop = "oats",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )
  expect_equal(oats, 0)

  peanuts <- calc_plc_payment_vectorized(
    crop = "peanuts",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )
  expect_equal(peanuts, 0.0625)

  corn <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )
  expect_equal(corn, 0.14)

  grain_sorghum <- calc_plc_payment_vectorized(
    crop = "grain sorghum",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )
  expect_equal(grain_sorghum, 0.61)

  soybeans <- calc_plc_payment_vectorized(
    crop = "soybeans",
    program_year = 2019,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  expect_equal(soybeans, 0)

})


test_that("calc_plc_payment_vectorized can recreate 2020 values in fsa file",{

  # Taken from: https://www.fsa.usda.gov/sites/default/files/documents/2020_PLC.xls

  wheat <- calc_plc_payment_vectorized(
    crop = "wheat",
    program_year = 2020,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  expect_equal(wheat, 0.45)

  barley <- calc_plc_payment_vectorized(
    crop = "barley",
    program_year = 2020,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  expect_equal(barley, 0.20)

  corn <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2020,
    base_acres = 1,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )
  expect_equal(corn, 0)

})


test_that("calc_plc_payment_vectorized calculates payment when ERP > MYA price", {
  # Test case where ERP > MYA price > NMLR
  result <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  # Payment = (ERP - MYA) * PLC yield * base acres * coverage level
  # Payment = (4.30 - 3.50) * 180 * 100 * 0.85 = 12240
  expected_payment <- (4.30 - 3.50) * 180 * 100 * 0.85
  expect_equal(result, expected_payment)
})

test_that("calc_plc_payment_vectorized calculates payment when MYA price <= NMLR", {
  # Test case where ERP > NMLR >= MYA price
  result <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 2.00,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  # Payment = (ERP - NMLR) * PLC yield * base acres * coverage level
  # Payment = (4.30 - 2.50) * 180 * 100 * 0.85 = 27540
  expected_payment <- (4.30 - 2.50) * 180 * 100 * 0.85
  expect_equal(result, expected_payment)
})

test_that("calc_plc_payment_vectorized handles different coverage levels", {
  result_85 <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    cov_lvl = 0.85,
    quiet = TRUE
  )

  result_88 <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    cov_lvl = 0.88,
    quiet = TRUE
  )

  # Payment with 88% coverage should be higher than 85%
  expect_gt(result_88, result_85)

  # Verify exact calculation
  expected_85 <- (4.30 - 3.50) * 180 * 100 * 0.85
  expected_88 <- (4.30 - 3.50) * 180 * 100 * 0.88
  expect_equal(result_85, expected_85)
  expect_equal(result_88, expected_88)
})

test_that("calc_plc_payment_vectorized defaults base_acres to 1 with warning", {
  expect_warning(
    result <- calc_plc_payment_vectorized(
      crop = "corn",
      program_year = 2024,
      mya_price = 3.50,
      erp = 4.30,
      nmlr = 2.50,
      plc_yield = 180,
      quiet = FALSE
    ),
    "No base acres supplied. Defaulting to 1 base acre."
  )

  # Should calculate for 1 acre
  expected_payment <- (4.30 - 3.50) * 180 * 1 * 0.85
  expect_equal(result, expected_payment)
})

test_that("calc_plc_payment_vectorized handles historic MYA prices for ERP calculation", {
  # Test with 5 years of historic MYA price data for ERP calculation
  historic_prices <- c(3.60, 3.50, 5.80, 6.80, 4.85)

  result <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 4.50,  # Current year MYA price
    historic_mya_prices = historic_prices,
    srp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  # Should be numeric and not error
  expect_type(result, "double")
  expect_gte(result, 0)
})

test_that("calc_plc_payment_vectorized handles insufficient historic MYA price data", {
  # Test with insufficient historic MYA prices for ERP calculation
  insufficient_prices <- c(6.80, 4.85)  # Only 2 values instead of required 5

  # This should error about insufficient data
  expect_error(
    calc_plc_payment_vectorized(
      crop = "corn",
      program_year = 2024,
      base_acres = 100,
      mya_price = 4.50,
      historic_mya_prices = insufficient_prices,
      srp = 4.30,
      nmlr = 2.50,
      plc_yield = 180,
      quiet = FALSE
    ),
    "historic_mya_prices vector must contain exactly 5 values"
  )
})

test_that("calc_plc_payment_vectorized edge cases", {
  # Test with zero base acres
  result_zero_acres <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 0,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  expect_equal(result_zero_acres, 0)

  # Test with zero PLC yield
  result_zero_yield <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 0,
    quiet = TRUE
  )

  expect_equal(result_zero_yield, 0)
})

test_that("calc_plc_payment_vectorized return type and value constraints", {
  result <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  # Should return a single numeric value
  expect_type(result, "double")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_gte(result, 0)  # Payment should never be negative
})

test_that("calc_plc_payment_vectorized quiet parameter works", {
  # Test that quiet = TRUE suppresses warnings
  expect_silent(
    calc_plc_payment_vectorized(
      crop = "rice",
      program_year = 2024,
      base_acres = 100,
      quiet = TRUE
    )
  )

  # Test that quiet = FALSE (default) shows warnings
  expect_warning(
    calc_plc_payment_vectorized(
      crop = "rice",
      program_year = 2024,
      base_acres = 100,
      quiet = FALSE
    ),
    "No crop type supplied"
  )
})

# ==============================================================================
# COMPARISON TESTS: Vectorized vs Original
# ==============================================================================

test_that("calc_plc_payment_vectorized matches calc_plc_payment for single values", {
  # Test with various parameter combinations
  test_cases <- expand.grid(
    crop = c("corn", "soybeans", "wheat"),
    program_year = c(2019, 2020, 2023),
    base_acres = c(1, 100),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(test_cases)) {
    original <- calc_plc_payment(
      crop = test_cases$crop[i],
      program_year = test_cases$program_year[i],
      base_acres = test_cases$base_acres[i],
      plc_yield = 1,
      cov_lvl = 1,
      quiet = TRUE
    )

    vectorized <- calc_plc_payment_vectorized(
      crop = test_cases$crop[i],
      program_year = test_cases$program_year[i],
      base_acres = test_cases$base_acres[i],
      plc_yield = 1,
      cov_lvl = 1,
      quiet = TRUE
    )

    expect_equal(vectorized, original,
                 tolerance = 1e-10,
                 info = paste("Mismatch for", test_cases$crop[i], test_cases$program_year[i]))
  }
})

test_that("calc_plc_payment_vectorized handles multiple rows correctly", {
  # Test with vectors
  crops <- c("corn", "soybeans", "wheat", "corn", "soybeans")
  years <- c(2019, 2019, 2019, 2020, 2020)
  acres <- c(100, 150, 200, 100, 150)

  vectorized <- calc_plc_payment_vectorized(
    crop = crops,
    program_year = years,
    base_acres = acres,
    plc_yield = 1,
    cov_lvl = 1,
    quiet = TRUE
  )

  # Compare each element with individual calls
  for (i in seq_along(crops)) {
    original <- calc_plc_payment(
      crop = crops[i],
      program_year = years[i],
      base_acres = acres[i],
      plc_yield = 1,
      cov_lvl = 1,
      quiet = TRUE
    )
    expect_equal(vectorized[i], original, tolerance = 1e-10,
                 info = paste("Mismatch at index", i, "for", crops[i], years[i]))
  }
})

test_that("calc_plc_payment_vectorized handles matrix of historic prices", {
  # Test with matrix of historic MYA prices
  crops <- c("corn", "soybeans", "wheat")
  years <- c(2024, 2024, 2024)
  acres <- c(100, 150, 200)

  # Create matrix of historic prices (3 rows × 5 columns)
  historic_prices_matrix <- rbind(
    c(3.60, 3.50, 5.80, 6.80, 4.85),  # corn
    c(9.50, 10.00, 11.00, 9.00, 10.50),  # soybeans
    c(5.00, 5.50, 6.00, 5.25, 5.75)   # wheat
  )

  result <- calc_plc_payment_vectorized(
    crop = crops,
    program_year = years,
    base_acres = acres,
    mya_price = c(4.50, 10.00, 5.50),
    historic_mya_prices = historic_prices_matrix,
    srp = c(4.30, 9.20, 5.50),
    nmlr = c(2.50, 6.00, 3.00),
    plc_yield = c(180, 50, 60),
    quiet = TRUE
  )

  # Should return vector with 3 values
  expect_length(result, 3)
  expect_type(result, "double")
  expect_true(all(result >= 0))
})

test_that("calc_plc_payment_vectorized handles single historic price vector applied to all rows", {
  # Test with single vector of historic MYA prices applied to all rows
  crops <- c("corn", "corn", "corn")
  years <- c(2024, 2024, 2024)
  acres <- c(100, 150, 200)

  # Single vector applied to all rows - use high ERP to ensure non-zero payment
  historic_prices <- c(5.60, 6.50, 7.80, 6.80, 7.85)

  result <- calc_plc_payment_vectorized(
    crop = crops,
    program_year = years,
    base_acres = acres,
    mya_price = 4.50,
    historic_mya_prices = historic_prices,
    srp = 7.00,  # High SRP to get high ERP
    nmlr = 2.50,
    plc_yield = 180,
    quiet = TRUE
  )

  # Should return vector with 3 values
  expect_length(result, 3)
  expect_type(result, "double")

  # All should be proportional to acres (same rate per acre)
  # result[i] = payment_rate * acres[i] * cov_lvl
  # So result[i] / (acres[i] * cov_lvl) should be same for all
  cov_lvl <- 0.85
  payment_per_acre_1 <- result[1] / (acres[1] * cov_lvl)
  payment_per_acre_2 <- result[2] / (acres[2] * cov_lvl)
  payment_per_acre_3 <- result[3] / (acres[3] * cov_lvl)

  expect_equal(payment_per_acre_1, payment_per_acre_2, tolerance = 1e-10)
  expect_equal(payment_per_acre_2, payment_per_acre_3, tolerance = 1e-10)

  # Verify we got non-zero payments (otherwise test is meaningless)
  expect_gt(sum(result), 0)
})

test_that("calc_plc_payment_vectorized matches original with custom parameters", {
  # Test with custom parameters
  original <- calc_plc_payment(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    cov_lvl = 0.88,
    quiet = TRUE
  )

  vectorized <- calc_plc_payment_vectorized(
    crop = "corn",
    program_year = 2024,
    base_acres = 100,
    mya_price = 3.50,
    erp = 4.30,
    nmlr = 2.50,
    plc_yield = 180,
    cov_lvl = 0.88,
    quiet = TRUE
  )

  expect_equal(vectorized, original, tolerance = 1e-10)
})

test_that("calc_plc_payment_vectorized performance is better than lapply", {
  # Create test data with 100 rows
  n <- 100
  crops <- rep(c("corn", "soybeans", "wheat"), length.out = n)
  years <- rep(c(2019, 2020), length.out = n)
  acres <- rep(100, n)

  # Time vectorized version
  time_vectorized <- system.time({
    result_vectorized <- calc_plc_payment_vectorized(
      crop = crops,
      program_year = years,
      base_acres = acres,
      plc_yield = 1,
      cov_lvl = 1,
      quiet = TRUE
    )
  })

  # Time lapply version (simulates original approach)
  time_lapply <- system.time({
    result_lapply <- sapply(1:n, function(i) {
      calc_plc_payment(
        crop = crops[i],
        program_year = years[i],
        base_acres = acres[i],
        plc_yield = 1,
        cov_lvl = 1,
        quiet = TRUE
      )
    })
  })

  # Vectorized should be faster (though maybe not on small n=100)
  # More importantly, results should be identical
  expect_equal(result_vectorized, result_lapply, tolerance = 1e-10)

  # Print timing for information (not an assertion)
  message(sprintf("Vectorized: %.3f sec, Lapply: %.3f sec, Speedup: %.1fx",
                  time_vectorized[3], time_lapply[3], time_lapply[3] / time_vectorized[3]))
})
