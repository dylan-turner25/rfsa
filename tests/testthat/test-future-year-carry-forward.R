# Tests for the future-year carry-forward convention in fsaArcPlcData.
# Rows for program years beyond the latest FSA releases carry forward PLC
# yields, base acres, enrollment elections, and irrigation shares from the
# most recent observed year (see data-raw/fsaArcPlc/supplementary_files/
# fsaArcPlcData.R). These tests guard against regressions where a newly added
# future year ships with those columns entirely missing.

test_that("most recent program year carries forward farm attributes", {
  data(fsaArcPlcData, package = "rfsa")

  max_yr <- max(fsaArcPlcData$program_year, na.rm = TRUE)
  prev_yr <- max_yr - 1

  cur <- fsaArcPlcData[fsaArcPlcData$program_year == max_yr, ]
  prev <- fsaArcPlcData[fsaArcPlcData$program_year == prev_yr, ]

  na_share <- function(x) mean(is.na(x))

  carry_cols <- c(
    "plc_yield", "base_acres", "enrolled_base_ARCCO",
    "enrolled_base_PLC", "planted_irrigated_share"
  )

  for (col in carry_cols) {
    # NA share in the newest year should be comparable to the prior year's
    # (within 10 points), never the ~100% seen when a source table is missing
    # the year entirely
    expect_lt(
      na_share(cur[[col]]), na_share(prev[[col]]) + 0.10,
      label = sprintf("NA share of %s in %s", col, max_yr)
    )
  }

  # enrollment must not be uniformly zero/NA (would zero out payment_type = "sum")
  expect_gt(sum(cur$enrolled_base_ARCCO, na.rm = TRUE), 0)
  expect_gt(sum(cur$enrolled_base_PLC, na.rm = TRUE), 0)
})

test_that("payments compute for the most recent program year in both policy environments", {
  data(fsaArcPlcData, package = "rfsa")
  max_yr <- max(fsaArcPlcData$program_year, na.rm = TRUE)

  totals <- sapply(c("fb18", "obbb"), function(env) {
    res <- calc_arc_plc_payments(
      program_year = max_yr,
      policy_environment = env,
      payment_type = "sum",
      sequestration_rate = 5.7,
      aggregate_level = "none",
      quiet = TRUE
    )
    sum(res$total_payment_value, na.rm = TRUE)
  })

  expect_gt(totals[["fb18"]], 0)
  expect_gt(totals[["obbb"]], 0)
  # OBBB raises reference prices and benchmark floors, so it should never pay
  # less than the 2018 Farm Bill environment in aggregate
  expect_gte(totals[["obbb"]], totals[["fb18"]])
})
