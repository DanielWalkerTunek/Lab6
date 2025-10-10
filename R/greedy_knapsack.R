#' Greedy Knapsack Algorithm
#'
#' @param x data.frame with two variables v and w
#' @param W the knapsack size
#'
#' @return A list with:
#' \describe{
#'   \item{value}{The maximum total value found.}
#'   \item{elements}{The indices of the selected items.}
#' }
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = n, replace = TRUE),
#'   v = runif(n, 0, 10000)
#' )
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'
#' @export
greedy_knapsack <- function(x, W) {
  # Input check
  if (!is.data.frame(x))
    stop("Input x must be a data.frame.", call. = FALSE)
  if (!all(c("w", "v") %in% names(x)))
    stop("x must contain columns named 'w' and 'v'.", call. = FALSE)
  if (any(x$w <= 0) || any(x$v <= 0))
    stop("All weights and values must be positive.", call. = FALSE)
  if (!is.numeric(W) || W <= 0)
    stop("W must be a positive numeric value.", call. = FALSE)

  # Calculate value-to-weight ratio
  x$ratio <- x$v / x$w

  # Store original row indices before sorting
  x$original_index <- seq_len(nrow(x))

  # Sort items by ratio in descending order
  sorted_x <- x[order(x$ratio, decreasing = TRUE), ]

  # Calculate the cumulative weight of the sorted items
  cumulative_weights <- cumsum(sorted_x$w)

  # Find the indices of all items that can fit
  items_to_take_indices <- which(cumulative_weights <= W)

  # If any items fit, select them
  if (length(items_to_take_indices) > 0) {
    selected_items <- sorted_x[items_to_take_indices, ]
    total_value <- sum(selected_items$v)
    elements <- selected_items$original_index
  } else {
    # Handle the case where no items fit
    total_value <- 0
    elements <- c()
  }

  return(list(value = round(total_value), elements = sort(elements)))
}

greedy_knapsack_original <- function(x, W) {
  x$ratio <- x$v / x$w
  x$original_index <- seq_len(nrow(x))
  sorted_x <- x[order(x$ratio, decreasing = TRUE), ]
  total_weight <- 0
  total_value <- 0
  elements <- c()
  for (i in 1:nrow(sorted_x)) {
    item <- sorted_x[i, ]
    if (total_weight + item$w <= W) {
      total_weight <- total_weight + item$w
      total_value <- total_value + item$v
      elements <- c(elements, item$original_index)
    }
  }
  return(list(value = round(total_value), elements = sort(elements)))
}
