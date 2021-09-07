#' Sum of the diagonal elements of a square matrix
#'
#' @param x A square matrix
#' @return Sum of the diagonal elements of a square matrix.
#' @examples
#' x <- matrix(c(1:9), 3, 3)
#' tr(x)
tr <- function (x) {
  sum(diag(x))
}
