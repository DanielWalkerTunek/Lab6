
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


  # create output obj
  result <- list(
    value = round(best_value, 2),
    elements = which(best_combination == 1)
  )

  return(result)

}








