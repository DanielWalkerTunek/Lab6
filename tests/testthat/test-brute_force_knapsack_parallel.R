suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("brute_force_knapsack_parallel() returns correct output type and structure", {
  res <- brute_force_knapsack_parallel(knapsack_objects[1:8, ], W = 3500, parallel = TRUE)

  # output type
  expect_type(res, "list")

  # output elements
  expect_true(all(c("value", "elements") %in% names(res)))

  # value must be numeric and positive
  expect_true(is.numeric(res$value))
  expect_gt(res$value, 0)

  # elements must be integers within range
  expect_true(all(res$elements %in% seq_len(8)))
})

test_that("brute_force_knapsack_parallel() handles invalid input correctly", {
  bad_df <- data.frame(weight = 1:5, value = 6:10)

  # missing columns
  expect_error(brute_force_knapsack_parallel(bad_df, 10, parallel = TRUE))

  # negative values
  bad_df <- data.frame(w = c(-1, 2), v = c(3, 4))
  expect_error(brute_force_knapsack_parallel(bad_df, 10, parallel = TRUE))

  # non-numeric W
  good_df <- data.frame(w = c(1, 2), v = c(3, 4))
  expect_error(brute_force_knapsack_parallel(good_df, "not_number", parallel = TRUE))
})

test_that("brute_force_knapsack_parallel() returns same result as brute_force_knapsack()", {
  skip_on_cran() # avoid parallel runs on CRAN machines

  bfk_seq <- brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
  bfk_par <- brute_force_knapsack_parallel(x = knapsack_objects[1:8, ], W = 3500, parallel = TRUE)

  # identical results
  expect_equal(round(bfk_seq$value, 5), round(bfk_par$value, 5))
  expect_equal(sort(bfk_seq$elements), sort(bfk_par$elements))
})

test_that("brute_force_knapsack_parallel() returns correct results for given examples", {
  skip_on_cran() # safe skip for CRAN
  expect_silent(bfk <- brute_force_knapsack_parallel(x = knapsack_objects[1:8, ], W = 3500, parallel = TRUE))
  expect_named(bfk, c("value", "elements"))

  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack_parallel(x = knapsack_objects[1:12, ], W = 3500, parallel = TRUE)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack_parallel(x = knapsack_objects[1:8, ], W = 2000, parallel = TRUE)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  bfk <- brute_force_knapsack_parallel(x = knapsack_objects[1:12, ], W = 2000, parallel = TRUE)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  st <- system.time(
    bfk <- brute_force_knapsack_parallel(x = knapsack_objects[1:16, ], W = 2000, parallel = TRUE)
  )
  expect_true(as.numeric(st)[2] >= 0.00)
})
