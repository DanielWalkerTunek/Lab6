
#' Brute Force Knapsack Solver (with optional parallelization)
#'
#' @param x data.frame with two variables v and w
#' @param W the knapsack size
#' @param parallel logical; if TRUE, the search will be parallelized across available cores.
#'
#' @return A list with:
#' \describe{
#'   \item{value}{The maximum total value found.}
#'   \item{elements}{The indices of the selected items.}
#' }
#'
#' @examples
#' n <- 8
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = n, replace = TRUE),
#'   v = runif(n = n, 0, 10000)
#' )
#' brute_force_knapsack_parallel(x = knapsack_objects[1:8, ], W = 3500, parallel = TRUE)
#'
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply detectCores
#'
#' @export
brute_force_knapsack_parallel <- function(x, W, parallel = FALSE) {

  # check the imput
  if (!is.data.frame(x))
    stop("Input x must be a data.frame.", call. = FALSE)
  if (!all(c("w", "v") %in% names(x)))
    stop("x must contain columns named 'w' and 'v'.", call. = FALSE)
  if (any(x$w <= 0) || any(x$v <= 0))
    stop("All weights and values must be positive.", call. = FALSE)
  if (!is.numeric(W) || W <= 0)
    stop("W must be a positive numeric value.", call. = FALSE)

  n <- nrow(x)
  if (n > 20)
    warning("Brute-force knapsack is extremely slow for n > 20.", call. = FALSE)

  combos <- 1:(2^n - 1)

  # if parrallel = TRUE
  if (parallel) {

    no_cores <- max(1, min(2, parallel::detectCores() - 1))
    cl <- parallel::makeCluster(no_cores)

    # send variables to each worker
    parallel::clusterExport(cl, varlist = c("x", "W", "n"), envir = environment())

    results <- parallel::parLapply(cl, combos, function(i) {
      combo <- as.integer(intToBits(i))[1:n]
      total_weight <- sum(x$w * combo)
      total_value  <- sum(x$v * combo)
      if (total_weight <= W) return(list(value = total_value, combo = combo))
      else return(NULL)
    })

    parallel::stopCluster(cl)

    # filter the results
    results <- Filter(Negate(is.null), results)

    if (length(results) == 0)
      return(list(value = 0, elements = integer(0)))

    best <- results[[which.max(sapply(results, function(r) r$value))]]

    result <- list(
      value = round(best$value, 2),
      elements = which(best$combo == 1)
    )

  } else {
    # parallel = FALSE call OG func.()
    result <- brute_force_knapsack(x, W)
  }

  return(result)
}
