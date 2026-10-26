# fsaEnrolledCountyBaseAcres must have one row per fips-year-crop
#
# FSA's 2023 and 2024 enrollment files list two renamed county offices under
# both their old and new names with the same acres (Henrico-Richmond City /
# Henrico VA, Snohomish-North King / Snohomish WA). Any consumer that joins on
# fips, program_year, crop and crop_type then doubles those rows; arpcAcres did,
# and it counted the counties' planted and base acres twice. The build script
# now collapses the variants; this test pins that.

test_that("fsaEnrolledCountyBaseAcres has no duplicate fips-year-crop keys", {
  data("fsaEnrolledCountyBaseAcres", envir = environment())
  d <- fsaEnrolledCountyBaseAcres
  key <- paste(d$fips, d$program_year, d$crop, d$crop_type, sep = "\r")
  expect_equal(anyDuplicated(key), 0L)
})

test_that("Renamed county offices keep their enrolled acres once", {
  data("fsaEnrolledCountyBaseAcres", envir = environment())
  d <- fsaEnrolledCountyBaseAcres
  henrico <- d[d$fips == "51087" & d$program_year == 2024 & d$crop == "corn", ]
  expect_equal(nrow(henrico), 1L)
  expect_equal(henrico$county, "Henrico")
  expect_equal(henrico$enrolled_base_ARCCO, 2172.20)
})
