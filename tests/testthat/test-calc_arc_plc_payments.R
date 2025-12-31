test_that("calc_arc_plc_payments produce origional values",{

  result <- calc_arc_plc_payments(program_year = 2025,
                        crop = "corn",
                        policy_environment = "obbb",
                        payment_type = "higher",
                        aggregate_level = "total",
                        quiet = T)$total_payment

  expect_equal(result, 6098403353, tolerance = 1)

  result <- calc_arc_plc_payments(program_year = 2025,
                                  crop = "wheat",
                                  sequestration_rate = 5.7,
                                  policy_environment = "obbb",
                                  payment_type = "higher",
                                  aggregate_level = "total",
                                  quiet = T)$total_payment

  expect_equal(result, 2781351101, tolerance = 1)

})

test_that("calc_arc_plc_payments works with custom price parameter",{

  # Test that custom price produces different result
  result_custom <- calc_arc_plc_payments(program_year = 2025,
                                  crop = "corn",
                                  policy_environment = "obbb",
                                  payment_type = "higher",
                                  price = 5.0,
                                  aggregate_level = "total",
                                  quiet = T)$total_payment

  result_current <- calc_arc_plc_payments(program_year = 2025,
                                   crop = "corn",
                                   policy_environment = "obbb",
                                   payment_type = "higher",
                                   aggregate_level = "total",
                                   quiet = T)$total_payment

  # Custom price should produce different (lower) payment for higher price
  expect_true(result_custom < result_current)

})


test_that("decomposing results by crop and state doesn't alter total",{

  # higher under obbb and higher payment
  result1 <- calc_arc_plc_payments(program_year = 2025,
                                  policy_environment = "obbb",
                                  payment_type = "higher",
                                  aggregate_level = "total",
                                  quiet = T)$total_payment

  result2 <- calc_arc_plc_payments(program_year = 2025,
                                   policy_environment = "obbb",
                                   payment_type = "higher",
                                   aggregate_level = "crop",
                                   quiet = T)$total_payment

  result3 <- calc_arc_plc_payments(program_year = 2025,
                                   policy_environment = "obbb",
                                   payment_type = "higher",
                                   aggregate_level = "state",
                                   quiet = T)$total_payment

  expect_equal(result1, sum(result2))
  expect_equal(result1, sum(result3))


  # arc under fb18 in 2024
  result1 <- calc_arc_plc_payments(program_year = 2024,
                                   policy_environment = "fb18",
                                   payment_type = "arc",
                                   aggregate_level = "total",
                                   quiet = T)$total_payment

  result2 <- calc_arc_plc_payments(program_year = 2024,
                                   policy_environment = "fb18",
                                   payment_type = "arc",
                                   aggregate_level = "crop",
                                   quiet = T)$total_payment

  result3 <- calc_arc_plc_payments(program_year = 2024,
                                   policy_environment = "fb18",
                                   payment_type = "arc",
                                   aggregate_level = "state",
                                   quiet = T)$total_payment

  expect_equal(result1, sum(result2))
  expect_equal(result1, sum(result3))

})


test_that("vector sequestration_rate applies different rates per program year", {

  # Test multi-year with vector sequestration rates
  result_vector_seq <- calc_arc_plc_payments(
    program_year = c(2024, 2025),
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    sequestration_rate = c(0, 5.7),
    aggregate_level = "none",
    quiet = TRUE
  )

  # Check that sequestration_rate column exists and has correct values
  expect_true("sequestration_rate" %in% names(result_vector_seq))
  expect_equal(unique(result_vector_seq$sequestration_rate[result_vector_seq$program_year == 2024]), 0)
  expect_equal(unique(result_vector_seq$sequestration_rate[result_vector_seq$program_year == 2025]), 5.7)

  # Compare with single year calculations to verify rates are applied correctly
  result_2024_no_seq <- calc_arc_plc_payments(
    program_year = 2024,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    sequestration_rate = 0,
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  result_2025_with_seq <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    sequestration_rate = 5.7,
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # Aggregate the vector result by year
  result_vector_aggregated <- result_vector_seq %>%
    group_by(program_year) %>%
    summarize(total = sum(total_payment_value, na.rm = TRUE))

  expect_equal(result_vector_aggregated$total[result_vector_aggregated$program_year == 2024],
               result_2024_no_seq)
  expect_equal(result_vector_aggregated$total[result_vector_aggregated$program_year == 2025],
               result_2025_with_seq)

  # Test validation: mismatched vector lengths should error
  expect_error(
    calc_arc_plc_payments(
      program_year = c(2024, 2025),
      crop = "corn",
      policy_environment = "obbb",
      payment_type = "higher",
      sequestration_rate = c(0, 5.7, 6.0),  # Wrong length
      quiet = TRUE
    ),
    "length must equal the number of program years"
  )

})


test_that("crop_type aggregate_level produces correct aggregations", {

  # Test aggregation by crop_type
  result_crop_type <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "crop_type",
    quiet = TRUE
  )

  # Check that crop_type column exists in results
  expect_true("crop_type" %in% names(result_crop_type))

  # Check that we have multiple crop types
  expect_true(length(unique(result_crop_type$crop_type)) > 1)

  # Verify total payments match when aggregating by crop_type vs total
  result_total <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  expect_equal(sum(result_crop_type$total_payment), result_total)

  # Test combining crop_type with other aggregate levels
  result_crop_type_state <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = c("crop_type", "state"),
    quiet = TRUE
  )

  # Should have both crop_type and state_name columns
  expect_true("crop_type" %in% names(result_crop_type_state))
  expect_true("state_name" %in% names(result_crop_type_state))

  # Total should still match
  expect_equal(sum(result_crop_type_state$total_payment), result_total)

})


test_that("single year vs multi-year vector produces identical results", {

  # Test that passing a single year produces the same result as passing
  # multiple years and filtering to that year. This prevents bugs where
  # the single-parameter and multi-parameter code paths diverge.

  # Single year calculation for 2025
  result_single <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = c("county", "crop", "crop_type"),
    quiet = TRUE
  )

  # Multi-year calculation including 2025
  result_multi <- calc_arc_plc_payments(
    program_year = 2019:2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = c("county", "crop", "crop_type"),
    quiet = TRUE
  ) %>%
    filter(program_year == 2025)

  # Results should be identical
  expect_equal(nrow(result_single), nrow(result_multi))
  expect_equal(sum(result_single$total_payment), sum(result_multi$total_payment))

  # Test with FB18 policy as well
  result_single_fb18 <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "fb18",
    payment_type = c("arc", "plc"),
    aggregate_level = c("county", "crop", "crop_type"),
    quiet = TRUE
  )

  result_multi_fb18 <- calc_arc_plc_payments(
    program_year = 2019:2025,
    crop = "corn",
    policy_environment = "fb18",
    payment_type = c("arc", "plc"),
    aggregate_level = c("county", "crop", "crop_type"),
    quiet = TRUE
  ) %>%
    filter(program_year == 2025)

  expect_equal(nrow(result_single_fb18), nrow(result_multi_fb18))
  expect_equal(sum(result_single_fb18$total_payment), sum(result_multi_fb18$total_payment))

  # Test at total aggregate level for simpler comparison
  total_single <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  total_multi <- calc_arc_plc_payments(
    program_year = 2024:2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "total",
    quiet = TRUE
  ) %>%
    filter(program_year == 2025) %>%
    pull(total_payment)

  expect_equal(total_single, total_multi)

})


# Tests for "lower" payment_type option

test_that("payment_type 'lower' uses per-acre minimum with total enrollment", {

  # Calculate with "lower" payment type
  result_lower <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # Calculate with "higher"
  result_higher <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # "lower" should always be less than or equal to "higher"
  expect_true(result_lower <= result_higher)

  # Both should return valid numeric values
  expect_true(is.numeric(result_lower))
  expect_true(is.numeric(result_higher))
  expect_false(is.na(result_lower))
  expect_false(is.na(result_higher))
})


test_that("payment_type 'lower' works correctly with different aggregation levels", {

  # Test with crop-level aggregation
  result_crop <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "crop",
    quiet = TRUE
  )

  # Check that we have multiple crops
  expect_true(nrow(result_crop) > 1)
  expect_true("crop" %in% names(result_crop))

  # Verify total matches
  result_total <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  expect_equal(sum(result_crop$total_payment), result_total)

  # Test with state-level aggregation
  result_state <- calc_arc_plc_payments(
    program_year = 2025,
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "state",
    quiet = TRUE
  )

  expect_true(nrow(result_state) > 1)
  expect_true("state_name" %in% names(result_state))
  expect_equal(sum(result_state$total_payment), result_total)
})


test_that("payment_type 'lower' works in multi-parameter scenarios", {

  # Test multiple payment types including "lower"
  result_multi <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = c("higher", "lower", "arc", "plc"),
    aggregate_level = "total",
    quiet = TRUE
  )

  # Should have 4 rows - one for each payment type
  expect_equal(nrow(result_multi), 4)
  expect_true("payment_type" %in% names(result_multi))
  expect_true(all(c("higher", "lower", "arc", "plc") %in% result_multi$payment_type))

  # Extract individual results
  lower_payment <- result_multi$total_payment[result_multi$payment_type == "lower"]
  higher_payment <- result_multi$total_payment[result_multi$payment_type == "higher"]

  # Verify that "lower" is always less than or equal to "higher"
  expect_true(lower_payment <= higher_payment)

  # Verify all payment types return valid numeric values
  expect_true(all(is.numeric(result_multi$total_payment)))
  expect_true(all(!is.na(result_multi$total_payment)))
})


test_that("single year vs multi-year produces identical results for 'lower'", {

  # Single year calculation for 2025 with "lower"
  result_single <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = c("county", "crop"),
    quiet = TRUE
  )

  # Multi-year calculation including 2025 with "lower"
  result_multi <- calc_arc_plc_payments(
    program_year = c(2024, 2025),
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = c("county", "crop"),
    quiet = TRUE
  ) %>%
    filter(program_year == 2025)

  # Results should be identical
  expect_equal(nrow(result_single), nrow(result_multi))
  expect_equal(sum(result_single$total_payment), sum(result_multi$total_payment))

  # Test at total level for exact comparison
  total_single <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  total_multi <- calc_arc_plc_payments(
    program_year = c(2024, 2025),
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  ) %>%
    filter(program_year == 2025) %>%
    pull(total_payment)

  expect_equal(total_single, total_multi)
})


test_that("'lower' uses correct enrolled acres formula in aggregation", {

  # Get detailed results with no aggregation
  result_none <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "none",
    quiet = TRUE
  )

  # Manually calculate expected total_payment_value
  # For "lower": final_payment * (enrolled_base_PLC + enrolled_base_ARCCO)
  expected_total <- result_none %>%
    mutate(
      expected_payment = final_payment * (enrolled_base_PLC + enrolled_base_ARCCO)
    ) %>%
    pull(expected_payment)

  # Compare with actual total_payment_value
  actual_total <- result_none$total_payment_value

  # Should match (allowing for floating point precision)
  expect_equal(actual_total, expected_total, tolerance = 1e-8)

  # Verify the formula is same as "higher"
  result_higher_none <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "higher",
    aggregate_level = "none",
    quiet = TRUE
  )

  # Both should use the same enrolled acres multiplier
  # (even though final_payment differs)
  # Filter out rows where final_payment is 0 or NA to avoid division issues
  valid_rows <- !is.na(result_none$final_payment) & result_none$final_payment != 0 &
                !is.na(result_higher_none$final_payment) & result_higher_none$final_payment != 0

  lower_multiplier <- result_none$total_payment_value[valid_rows] / result_none$final_payment[valid_rows]
  higher_multiplier <- result_higher_none$total_payment_value[valid_rows] / result_higher_none$final_payment[valid_rows]

  expect_equal(lower_multiplier, higher_multiplier, tolerance = 1e-8)
})


test_that("'lower' works correctly with both FB18 and OBBB policies", {

  # Test with FB18
  result_fb18 <- calc_arc_plc_payments(
    program_year = 2024,
    crop = "soybeans",
    policy_environment = "fb18",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # Test with OBBB
  result_obbb <- calc_arc_plc_payments(
    program_year = 2024,
    crop = "soybeans",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # Both should return numeric values (not NA or error)
  expect_true(is.numeric(result_fb18))
  expect_true(is.numeric(result_obbb))
  expect_false(is.na(result_fb18))
  expect_false(is.na(result_obbb))

  # Results should differ between policies (due to different parameters)
  expect_true(result_fb18 != result_obbb)

  # Compare both policies for multiple payment types
  result_multi_policy <- calc_arc_plc_payments(
    program_year = 2024,
    crop = "soybeans",
    policy_environment = c("fb18", "obbb"),
    payment_type = c("higher", "lower"),
    aggregate_level = "total",
    quiet = TRUE
  )

  # Should have 4 rows (2 policies × 2 payment types)
  expect_equal(nrow(result_multi_policy), 4)
})


# Integration tests for "lower"

test_that("'lower' integrates correctly with custom prices", {

  # Test that custom price affects lower calculation
  result_custom <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    price = 5.0,
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  result_current <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # Custom price should produce different result
  expect_true(result_custom != result_current)
})


test_that("'lower' works with sequestration rates", {

  result_no_seq <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    sequestration_rate = 0,
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  result_with_seq <- calc_arc_plc_payments(
    program_year = 2025,
    crop = "corn",
    policy_environment = "obbb",
    payment_type = "lower",
    sequestration_rate = 5.7,
    aggregate_level = "total",
    quiet = TRUE
  )$total_payment

  # With sequestration should be less
  expect_true(result_with_seq < result_no_seq)

  # Should be exactly 94.3% of no sequestration (100% - 5.7%)
  expect_equal(result_with_seq, result_no_seq * 0.943, tolerance = 1e-8)
})
