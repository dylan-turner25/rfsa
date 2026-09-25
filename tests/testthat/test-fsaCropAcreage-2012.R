test_that("2012 acreage uses the final release and preserves crop classifications", {
  acreage <- new.env()
  data("fsaCropAcreage", package = "rfsa", envir = acreage)
  all_crops <- acreage$fsaCropAcreage
  acres_2012 <- all_crops[which(all_crops$crop_yr == 2012), ]

  expect_gt(nrow(acres_2012), 0)
  expect_equal(unique(acres_2012$release_date), as.Date("2013-01-01"))
  expect_true(all(acres_2012$current_release))

  # National controls from the final January 2013 county workbook. These also
  # distinguish the final release from the retained in-season releases.
  corn <- acres_2012[acres_2012$crop == "corn", ]
  measures <- c("planted_acres", "failed_acres", "prevented_acres",
                "planted_and_failed_acres")
  expected <- c(93947622.02, 122311.82, 262467.10, 94069933.84)
  expect_equal(round(unname(colSums(corn[measures])), 2), expected)

  # Literal NULL in the source's type column must not hide the cotton subtype
  # embedded in the crop name.
  cotton <- acres_2012[acres_2012$crop == "cotton", ]
  expect_setequal(unique(cotton$fsa_crop_type), c("upland", "extra long staple"))
  expect_equal(round(sum(cotton$planted_acres[cotton$fsa_crop_type == "upland"]), 2),
               9673500.52)
  expect_equal(round(sum(cotton$planted_acres[cotton$fsa_crop_type == "extra long staple"]), 2),
               230972.41)

  data("fsaCropAcreageCC", package = "rfsa", envir = acreage)
  cc <- acreage$fsaCropAcreageCC
  cc_corn <- cc[which(cc$crop_yr == 2012 & cc$crop == "corn"), ]
  expect_gt(nrow(cc_corn), 0)
  expect_equal(round(unname(colSums(cc_corn[measures])), 2), expected)

  data("fsaCoveredCommodityShares", package = "rfsa", envir = acreage)
  shares <- acreage$fsaCoveredCommodityShares
  shares <- shares[which(shares$crop_yr == 2012), ]
  expect_gt(nrow(shares), 0)
  expect_equal(sum(shares$total_planted_acres), sum(acres_2012$planted_acres))
  expect_equal(sum(shares$cc_planted_acres),
               sum(acres_2012$planted_acres[acres_2012$covered_commodity]))
})
