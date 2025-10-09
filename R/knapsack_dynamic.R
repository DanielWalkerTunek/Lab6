
#' Dynamic Programming Knapsack Solver
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
#'
#' knapsack_dynamic(knapsack_objects[1:8,], W = 3500)
#'
#' @export

knapsack_dynamic <- function(x, W) {

  # input check
  if (!is.data.frame(x))
    stop("Input x must be a data.frame.", call. = FALSE)
  if (!all(c("w", "v") %in% names(x)))
    stop("x must contain columns named 'w' and 'v'.", call. = FALSE)
  if (any(x$w <= 0) || any(x$v <= 0))
    stop("All weights and values must be positive.", call. = FALSE)
  if (!is.numeric(W) || W <= 0)
    stop("W must be a positive numeric value.", call. = FALSE)


  n <- nrow(x)
  m <- matrix(0, nrow = n + 1, ncol = W + 1)

  # code for the loop
  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        m[i + 1, w + 1] <- max(
          m[i, w + 1],
          m[i, w + 1 - x$w[i]] + x$v[i]
        )
      } else {
        m[i + 1, w + 1] <- m[i, w + 1]
      }
    }
  }

  # Backtrack to find selected elements
  best_value <- m[n + 1, W + 1]
  w <- W
  best_combination <- rep(0, n)

  for (i in n:1) {
    if (m[i + 1, w + 1] != m[i, w + 1]) {
      best_combination[i] <- 1
      w <- w - x$w[i]
    }
  }


  result <- list(
    value = round(best_value, 2),
    elements = which(best_combination == 1)
  )



  return(result)

}








