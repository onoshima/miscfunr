#' Convert correlation matrix to covariance matrix
#'
#' @param cormat A correlation matrix.
#' @param sd A sd vector.
#' @return A covariance matrix.
#' @examples
#' R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' SD <- c(2, 3)
#' V <- convert_cor_to_cov(R, SD)
convert_cor_to_cov <- function(cormat, sd) {
    if (nrow(cormat) != ncol(cormat)) {
        stop("The input matrix is not squared.")
    } else if (nrow(cormat) != length(sd)) {
        stop("The number of variables of the correlation matrix and SD vector are dirfferent.")
    }
    D <- diag(sd)
    covmat <- D %*% cormat %*% D
    rownames(covmat) <- rownames(cormat)
    colnames(covmat) <- colnames(cormat)
    covmat
}
