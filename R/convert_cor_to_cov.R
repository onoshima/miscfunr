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
