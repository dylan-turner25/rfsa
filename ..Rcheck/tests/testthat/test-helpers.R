test_that("calc_effective_reference_price works correctly", {
  # Test basic ERP calculation
  mya_prices <- c(3.70, 3.60, 3.50, 3.40, 3.30)
  srp <- 3.90
  
  result <- calc_effective_reference_price(mya_prices, srp)
  
  # Olympic average of (3.70, 3.60, 3.50, 3.40, 3.30) removes 3.70 and 3.30
  # Average of (3.60, 3.50, 3.40) = 3.50
  # 85% of 3.50 = 2.975
  # Since 2.975 < 3.90 (SRP), ERP should be 3.90
  expect_equal(result, 3.90)
  
  # Test where 85% of Olympic average is used
  mya_prices_high <- c(5.70, 5.60, 5.50, 5.40, 5.30)
  result_high <- calc_effective_reference_price(mya_prices_high, srp)
  
  # Olympic average = (5.60 + 5.50 + 5.40) / 3 = 5.50
  # 85% of 5.50 = 4.675
  # Since 4.675 > 3.90 but < 1.15 * 3.90 = 4.485, but 4.675 >= 4.485
  # So ERP should be 1.15 * 3.90 = 4.485
  expected_erp <- 1.15 * 3.90
  expect_equal(result_high, expected_erp)
})

test_that("calc_effective_reference_price validates inputs", {
  # Test with wrong number of prices
  expect_error(
    calc_effective_reference_price(c(3.50, 3.40, 3.30), 3.90),
    "Exactly 5 MYA prices are required"
  )
  
  # Test with missing SRP
  expect_error(
    calc_effective_reference_price(c(3.70, 3.60, 3.50, 3.40, 3.30), NULL),
    "Statutory reference price.*must be a numeric value"
  )
  
  # Test with non-numeric SRP
  expect_error(
    calc_effective_reference_price(c(3.70, 3.60, 3.50, 3.40, 3.30), "not_numeric"),
    "Statutory reference price.*must be a numeric value"
  )
})

test_that("get_plc_yield basic functionality", {
  # This test requires the FSA datasets to be available
  
  # Test basic yield retrieval (should use national average and warn)
  expect_warning(
    yield <- get_plc_yield("corn", 2024, quiet = FALSE),
    "No PLC yield.*location.*national average"
  )
  
  expect_type(yield, "double")
  expect_length(yield, 1)
  expect_gt(yield, 0)
})

test_that("get_plc_yield handles crop types", {
  # This test requires the FSA datasets to be available
  
  # Test with rice crop type
  yield_rice_long <- get_plc_yield("rice", 2024, crop_type = "long grain", quiet = TRUE)
  yield_rice_medium <- get_plc_yield("rice", 2024, crop_type = "short/medium grain", quiet = TRUE)
  
  expect_type(yield_rice_long, "double")
  expect_type(yield_rice_medium, "double")
  expect_gt(yield_rice_long, 0)
  expect_gt(yield_rice_medium, 0)
  
  # Yields might be different for different types
  # (though this isn't guaranteed, so we just test they're valid)
})

test_that("get_plc_yield handles location parameters", {
  # This test requires the FSA datasets to be available
  
  # Test with FIPS code
  expect_warning(
    yield_fips <- get_plc_yield("corn", 2024, fips = "19169", quiet = FALSE),
    "county average PLC yield"
  )
  
  # Test with state only
  expect_warning(
    yield_state <- get_plc_yield("corn", 2024, state = "Iowa", quiet = FALSE),
    "state average PLC yield"
  )
  
  # Test with state and county
  yield_county <- get_plc_yield("corn", 2024, state = "Iowa", county = "Story")
  
  expect_type(yield_fips, "double")
  expect_type(yield_state, "double")
  expect_type(yield_county, "double")
  expect_gt(yield_fips, 0)
  expect_gt(yield_state, 0)
  expect_gt(yield_county, 0)
})

test_that("get_plc_yield handles historical years", {
  # This test requires the FSA datasets to be available
  
  # Test that years before 2018 default to 2018
  expect_warning(
    yield_2017 <- get_plc_yield("corn", 2017, quiet = FALSE),
    "national average"
  )
  
  expect_warning(
    yield_2018 <- get_plc_yield("corn", 2018, quiet = FALSE),
    "national average"
  )
  
  expect_type(yield_2017, "double")
  expect_type(yield_2018, "double")
  expect_gt(yield_2017, 0)
  expect_gt(yield_2018, 0)
})

test_that("get_plc_yield state name conversion", {
  # This test requires the FSA datasets to be available
  
  # Test state abbreviation conversion
  expect_warning(
    yield_abbrev <- get_plc_yield("corn", 2024, state = "IA", quiet = FALSE),
    "state average"
  )
  
  expect_warning(
    yield_full <- get_plc_yield("corn", 2024, state = "Iowa", quiet = FALSE),
    "state average"
  )
  
  # Should give same result
  expect_equal(yield_abbrev, yield_full)
})

test_that("clean_crop_names2 function works correctly", {
  # Test basic crop name cleaning
  expect_equal(clean_crop_names2("corn"), "corn")
  expect_equal(clean_crop_names2("rice-long grain"), "rice")
  expect_equal(clean_crop_names2("chickpeas_large"), "chickpeas")
  expect_equal(clean_crop_names2("beans- chickpeas"), "chickpeas")
  
  # Test with parentheses and numbers
  expect_equal(clean_crop_names2("corn (1)"), "corn")
  expect_equal(clean_crop_names2("wheat 2/"), "wheat")
  
  # Test abbreviation conversion - these return named vectors, so test differently
  expect_equal(as.character(clean_crop_names2("barly")), "barley")
  expect_equal(as.character(clean_crop_names2("pnuts")), "peanuts")
  expect_equal(as.character(clean_crop_names2("soybn")), "soybeans")
  
  # Test with NA
  expect_true(is.na(clean_crop_names2(NA)))
  
  # Test trailing hyphens
  expect_equal(clean_crop_names2("cotton-"), "cotton")
})

test_that("extract_crop_type function works correctly", {
  # Test rice types
  expect_equal(extract_crop_type("rice-long grain"), "long grain")
  expect_equal(extract_crop_type("rice-med grain"), "short/medium grain")
  expect_equal(extract_crop_type("rice-temp japonica"), "temperate japonica")
  
  # Test chickpea types
  expect_equal(extract_crop_type("chickpeas_large"), "large")
  expect_equal(extract_crop_type("chickpeas_small"), "small")
  
  # Test cotton types
  expect_equal(extract_crop_type("cotton seed"), "seed")
  expect_equal(extract_crop_type("cotton upland"), "upland")
  
  # Test crops with no type
  expect_true(is.na(extract_crop_type("corn")))
  expect_true(is.na(extract_crop_type("soybeans")))
  
  # Test RMA codes
  expect_equal(extract_crop_type("rice-long grain", rma_code = TRUE), "453")
  expect_equal(extract_crop_type("rice-med grain", rma_code = TRUE), "451/452")
})

test_that("assign_rma_cc function works correctly", {
  # Test major crop codes
  expect_equal(assign_rma_cc("corn"), 41)
  expect_equal(assign_rma_cc("soybeans"), 81)
  expect_equal(assign_rma_cc("wheat"), 11)
  expect_equal(assign_rma_cc("rice"), 18)
  expect_equal(assign_rma_cc("cotton"), 21)
  expect_equal(assign_rma_cc("sorghum"), 51)
  expect_equal(assign_rma_cc("barley"), 91)
  expect_equal(assign_rma_cc("oats"), 16)
  expect_equal(assign_rma_cc("peanuts"), 75)
  expect_equal(assign_rma_cc("sunflowers"), 78)
  
  # Test case insensitivity
  expect_equal(assign_rma_cc("CORN"), 41)
  expect_equal(assign_rma_cc("Soybeans"), 81)
  
  # Test unknown crop
  expect_true(is.na(assign_rma_cc("unknown_crop")))
})

test_that("clean_fips function works correctly", {
  # Test 4-digit FIPS to 5-digit - function returns named vector
  expect_equal(as.character(clean_fips(fips = "1001")), "01001")
  
  # Test 5-digit FIPS unchanged
  expect_equal(as.character(clean_fips(fips = "19169")), "19169")
  
  # Test construction from state and county
  expect_equal(as.character(clean_fips(state = "19", county = "169")), "19169")
  expect_equal(as.character(clean_fips(state = "1", county = "1")), "01001")
  
  # Test invalid short FIPS - function prints warning but doesn't throw it
  result <- clean_fips(fips = "1")
  expect_true(is.na(result))
})

test_that("valid_state function works correctly", {
  # Test valid state names
  expect_equal(valid_state("Iowa"), "Iowa")
  expect_equal(valid_state("illinois"), "illinois")
  
  # Test valid abbreviations
  expect_equal(valid_state("IA"), "IA")
  expect_equal(valid_state("il"), "il")
  
  # Test valid FIPS codes
  expect_equal(valid_state("19"), "19")
  expect_equal(valid_state("17"), "17")
  
  # Test invalid state
  expect_error(valid_state("NotAState"), "Parameter value for state not valid")
})