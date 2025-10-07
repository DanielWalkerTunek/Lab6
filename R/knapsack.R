
#' Brute Force Knapsack Solver
#'
#' @param x data.frame with two variables v and w
#' @param W the knapsack size
#'
#' @return A list with:
#' \describe{
#'   \item{value}{The maximum total value found.}
#'   \item{elements}{The indices of the selected items.}
#' }
#'
#' @examples
#' # Example with 8 items
#' n <- 8
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = n, replace = TRUE),
#'   v = runif(n = n, 0, 10000)
#' )
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @export

brute_force_knapsack <- function(x, W){

  # inputcheck
  if (!is.data.frame(x))
    stop("Input x must be a data.frame.", call. = FALSE)
  if (!all(c("w", "v") %in% names(x)))
    stop("x must contain columns named 'w' and 'v'.", call. = FALSE)
  if (any(x$w <= 0) || any(x$v <= 0))
    stop("All weights and values must be positive.", call. = FALSE)
  if (!is.numeric(W) || W <= 0)
    stop("W must be a positive numeric value.", call. = FALSE)


  # safeguard for big n
  n <- nrow(x)
  if (n > 20)
    warning("Brute-force knapsack is extremely slow for n > 20.", call. = FALSE)

  # create placeholders
  best_value <- 0
  best_combination <- NULL

  # loop over all combinations
  for (i in 1:(2^n - 1)) {
    # binary representation of combination
    combo <- as.integer(intToBits(i))[1:n]
    total_weight <- sum(x$w * combo)
    total_value  <- sum(x$v * combo)

    if (total_weight <= W && total_value > best_value) {
      best_value <- total_value
      best_combination <- combo
    }
  }

  # create output obj
  result <- list(
    value = round(best_value, 2),
    elements = which(best_combination == 1)
  )

  return(result)
}
