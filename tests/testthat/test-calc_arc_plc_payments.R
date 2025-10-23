test_that("calc_arc_plc_payments produce origional values",{

  result <- calc_arc_plc_payments(program_year = 2025,
                        crop = "corn",
                        policy_environment = "obbb",
                        payment_type = "higher",
                        aggregate_level = "total",
                        quiet = T)$total_payment

  expect_equal(result, 6746198001)

  result <- calc_arc_plc_payments(program_year = 2025,
                                  crop = "wheat",
                                  sequestration_rate = 5.7,
                                  policy_environment = "obbb",
                                  payment_type = "higher",
                                  aggregate_level = "total",
                                  quiet = T)$total_payment

  expect_equal(result, 2571646103)

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
