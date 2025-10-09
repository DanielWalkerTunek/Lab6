context("brute_force_knapsack")

test_that("brute_force_knapsack() returns correct output type and structure", {
  n <- 8
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )

  res <- brute_force_knapsack(knapsack_objects, W = 3500)

  # output type
  expect_type(res, "list")

  # output elements
  expect_true(all(c("value", "elements") %in% names(res)))

  # value must be numeric and positive
  expect_true(is.numeric(res$value))
  expect_gt(res$value, 0)

  # elements must be integers within range
  expect_true(all(res$elements %in% seq_len(n)))
})


test_that("brute_force_knapsack() handles invalid input correctly", {
  bad_df <- data.frame(weight = 1:5, value = 6:10)

  # missing columns
  expect_error(brute_force_knapsack(bad_df, 10))

  # negative values
  bad_df <- data.frame(w = c(-1, 2), v = c(3, 4))
  expect_error(brute_force_knapsack(bad_df, 10))

  # non-numeric W
  good_df <- data.frame(w = c(1, 2), v = c(3, 4))
  expect_error(brute_force_knapsack(good_df, "not_number"))
})



# the given tests

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that("functions rejects errounous input.", {
  expect_error(brute_force_knapsack("hej", 3500))
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  st <- system.time(bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})




