# Tests for policy-environment statutory reference price sourcing.
# The published statutory_reference_price column in fsaArcPlcData reflects
# FSA-published values, which are the OBBBA-raised prices from program year
# 2026 on. The fb18 environment must use the 2018 Farm Bill schedule
# (fb18_srps) instead, or the counterfactual is contaminated and floor-bound
# crops (rice, peanuts) show identical payments under both environments.

test_that("fb18_srps carries the 2018 Farm Bill schedule into 2026", {
  data(fsaArcPlcData, package = "rfsa")
  d <- setup_fb18_parameters(fsaArcPlcData)

  pick <- function(yr, cr, ct = NULL) {
    idx <- !is.na(d$program_year) & d$program_year == yr & d$crop == cr
    if (!is.null(ct)) idx <- idx & !is.na(d$crop_type) & d$crop_type == ct
    unique(stats::na.omit(d$fb18_srps[idx]))
  }

  # 2026 rows must show 2018 Farm Bill prices, not the published OBBBA values
  expect_equal(pick(2026, "corn"), 3.70)
  expect_equal(pick(2026, "soybeans"), 8.40)
  expect_equal(pick(2026, "peanuts"), 0.2675)
  expect_equal(pick(2026, "rice", "long grain"), 0.14)

  # Through 2025 the published column IS the 2018 Farm Bill schedule
  idx25 <- !is.na(d$program_year) & d$program_year == 2025
  expect_identical(d$fb18_srps[idx25], d$statutory_reference_price[idx25])
})

test_that("floor-bound crops show a real fb18-vs-obbb gap in 2026", {
  totals <- sapply(c("fb18", "obbb"), function(env) {
    res <- calc_arc_plc_payments(
      program_year = 2026,
      policy_environment = env,
      payment_type = "plc",
      crop = c("rice", "peanuts"),
      sequestration_rate = 5.7,
      aggregate_level = "none",
      quiet = TRUE
    )
    sum(res$total_payment_value, na.rm = TRUE)
  })
  # Before the fix these were identical (both used OBBBA statutory prices)
  expect_gt(totals[["obbb"]], totals[["fb18"]] * 1.5)
})

test_that("temperate japonica obbb uses the published 2026 SRP", {
  data(fsaArcPlcData, package = "rfsa")
  d <- setup_obbb_parameters(fsaArcPlcData)
  jap <- unique(stats::na.omit(
    d$obbb_srps[d$crop == "rice" &
                  !is.na(d$crop_type) & d$crop_type == "temperate japonica"]
  ))
  expect_equal(jap, 0.2433)

  # The OBBBA scenario must not pay LESS than fb18 for japonica in 2026
  jp <- sapply(c("fb18", "obbb"), function(env) {
    res <- calc_arc_plc_payments(
      program_year = 2026,
      policy_environment = env,
      payment_type = "plc",
      crop = "rice",
      sequestration_rate = 5.7,
      aggregate_level = "none",
      quiet = TRUE
    )
    res <- res[!is.na(res$crop_type) & res$crop_type == "temperate japonica", ]
    sum(res$total_payment_value, na.rm = TRUE)
  })
  expect_gte(jp[["obbb"]], jp[["fb18"]])
})
