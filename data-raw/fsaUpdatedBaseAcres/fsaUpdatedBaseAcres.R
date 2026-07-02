# fsaUpdatedBaseAcres --------------------------------------------------------
# County x crop estimates of the ADDITIONAL base acres allocated under the
# One Big Beautiful Bill Act (OBBBA) base-acre update, effective the 2026 crop
# year. Snapshotted from the `proj-base-vs-planted-acres` project, which
# implements the statutory allocation formula (5-yr 2019-2023 covered-commodity
# plantings + capped noncovered adjustment, less existing base, prorated to the
# national 30 million acre cap) under 8 policy interpretations (scenarios s1-s8;
# s1 is the main scenario). See estimate_base_acre_updates.R / build_dataset.R
# in that project for the methodology.
#
# The snapshot CSVs live in ./data-raw/fsaUpdatedBaseAcres/s{1..8}/ and are a
# point-in-time copy; re-copy them if the project re-estimates.

library(dplyr)

scenario_ids <- paste0("s", 1:8)
base_dir <- "./data-raw/fsaUpdatedBaseAcres"

fsaUpdatedBaseAcres <- purrr::map_dfr(scenario_ids, function(sid) {
  fp <- file.path(base_dir, sid, "base_acres_2026_county_commodity.csv")
  if (!file.exists(fp)) stop("Missing scenario snapshot: ", fp)

  read.csv(fp, stringsAsFactors = FALSE) %>%
    # Map RMA sub-type crop names to the whole-crop names used in fsaArcPlcData
    # (mirrors proj-base-vs-planted-acres/data-raw/build_dataset.R:172-182).
    mutate(crop = case_when(
      grepl("^chickpeas_", crop) ~ "chickpeas",
      grepl("^rice_", crop)      ~ "rice",
      TRUE                        ~ crop
    )) %>%
    group_by(fips, crop) %>%
    summarise(additional_base_acres = sum(additional_base_acres, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(scenario = sid, fips = as.numeric(fips)) %>%
    select(fips, crop, scenario, additional_base_acres)
})

# Quick sanity report
message("fsaUpdatedBaseAcres: ", nrow(fsaUpdatedBaseAcres), " rows across ",
        dplyr::n_distinct(fsaUpdatedBaseAcres$scenario), " scenarios")
print(fsaUpdatedBaseAcres %>%
        group_by(scenario) %>%
        summarise(total_additional_base = sum(additional_base_acres, na.rm = TRUE),
                  .groups = "drop"))

usethis::use_data(fsaUpdatedBaseAcres, overwrite = TRUE)
