
#' mvnpdf
#'
#' @param x your dataframe
#' @param mean vector of mean
#' @param varcovM variance/covariance matrix
#' @param Log if Lof == TRUE, it's return logarythm value
#'
#' @return list of vector, matrix x and the value of density
#' @export
#'
#' @examples
mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {

  # mean <- rep(0, nrow((x)))
  # varcovM = diag(nrow(x))

  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean

  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL

  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM - 0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }

  if (!Log) {
    y <- exp(y)
  }

  #return(y)
  return(list(x=x, y=y))
}

