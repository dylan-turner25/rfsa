test_that("calc_arc_plc_payments produce origional values",{

  result <- calc_arc_plc_payments(program_year = 2025,
                        crop = "corn",
                        policy_environment = "obbb",
                        payment_type = "higher",
                        aggregate_level = "total",
                        quiet = T)$total_payment

  expect_equal(result, 5834405910)

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
