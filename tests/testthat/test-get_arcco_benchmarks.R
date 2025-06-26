test_that("get_arcco_benchmarks basic functionality works", {
  skip_if_not(file.exists("data/fsaArcCoBenchmarks.rda"))

  # Test basic yield retrieval (should use national average and warn)
  expect_warning(
    yield <- get_arcco_benchmarks("corn", 2024, "yield", quiet = FALSE),
    "No location parameters supplied.*national average.*yield"
  )

  expect_type(yield, "double")
  expect_length(yield, 1)
  expect_gt(yield, 0)

  # Test basic price retrieval
  expect_warning(
    price <- get_arcco_benchmarks("corn", 2024, "price", quiet = FALSE),
    "No location parameters supplied.*national average.*price"
  )

  expect_type(price, "double")
  expect_length(price, 1)
  expect_gt(price, 0)
})


test_that("get_arcco_benchmarks reproduce known values from FSA", {
  yield <- get_arcco_benchmarks(crop = "corn",
                       program_year = 2024,
                       fips = "01001",
                       yield_type = "All",
                       benchmark_type = "yield",
                       quiet = T)

  price <- get_arcco_benchmarks(crop = "corn",
                                program_year = 2024,
                                fips = "01001",
                                yield_type = "All",
                                benchmark_type = "price",
                                quiet = T)

  expect_equal(yield, 168.57)
  expect_equal(price, 4.85)
})

test_that("get_arcco_benchmarks calculates Olympic average from historical prices", {
  # Test Olympic average calculation with historical prices
  # Expected: sort c(3.50,3.50,5.50,5.50,7.63) = c(3.50,3.50,5.50,5.50,7.63)
  # Remove highest (7.63) and lowest (3.50), average middle 3: (3.50+5.50+5.50)/3 = 4.833333
  
  price <- get_arcco_benchmarks(crop = "corn",
                                program_year = 2024,
                                fips = "01001",  # This will be ignored when historical_prices is provided
                                yield_type = "All",
                                benchmark_type = "price",
                                historical_prices = c(3.50,3.50,5.50,5.50,7.63),
                                quiet = TRUE)

  # Expected Olympic average: (3.50 + 5.50 + 5.50) / 3 = 4.833333
  expect_type(price, "double")
  expect_length(price, 1)
  expect_equal(round(price, 6), 4.833333)
})

