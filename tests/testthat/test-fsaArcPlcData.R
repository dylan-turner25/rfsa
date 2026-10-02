test_that("no county-crop-year carries base on both All and split rows", {
  data("fsaArcPlcData", envir = environment())
  both <- fsaArcPlcData %>%
    dplyr::filter(!is.na(yield_type)) %>%
    dplyr::mutate(carries = dplyr::coalesce(base_acres, 0) > 0 |
                    dplyr::coalesce(enrolled_base_ARCCO, 0) > 0 |
                    dplyr::coalesce(enrolled_base_PLC, 0) > 0) %>%
    dplyr::group_by(fips, crop, crop_type, program_year) %>%
    dplyr::summarize(
      all = any(carries & yield_type == "All"),
      split = any(carries & yield_type %in% c("Irrigated", "Nonirrigated")),
      .groups = "drop"
    ) %>%
    dplyr::filter(all, split)
  expect_equal(nrow(both), 0)
})

test_that("planted acres are carried once per county-crop-year", {
  data("fsaArcPlcData", envir = environment())
  data("fsaCropAcreageCC", envir = environment())

  # same cotton mapping as the build; acres with no irrigation practice reach
  # only All totals, so split rows may fall short by those acres
  src <- fsaCropAcreageCC %>%
    dplyr::mutate(
      crop_type = gsub("upland|extra long staple", "seed", crop_type),
      fips = as.numeric(fips)
    ) %>%
    dplyr::group_by(program_year = crop_yr, crop, crop_type, fips) %>%
    dplyr::summarize(
      src = sum(planted_acres, na.rm = TRUE),
      unknown = sum(planted_acres[!irrigation_practice %in% c("I", "N")],
                    na.rm = TRUE),
      .groups = "drop"
    )
  panel <- fsaArcPlcData %>%
    dplyr::group_by(program_year, crop, crop_type, fips) %>%
    dplyr::summarize(panel = sum(planted_acres), .groups = "drop") %>%
    dplyr::inner_join(src, by = c("program_year", "crop", "crop_type", "fips"))

  expect_gt(nrow(panel), 0)
  expect_true(all(panel$panel <= panel$src + 0.01))
  expect_true(all(panel$panel >= panel$src - panel$unknown - 0.01))
})
