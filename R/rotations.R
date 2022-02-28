IDENTITY_MATRIX <- diag(3)

#' is_rotation checker
#'
#' @param m A matrix
#' @return boolean
#' @export
is_rotation <- function(m) {
  if (is.matrix(m)) {
    d <- dim(m)
    mt <- t(m)
    if ((d[1] == d[2]) && (det(m) == 1) && (m %*% mt == IDENTITY_MATRIX)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Rotate wrt x axis
#' also is_rotation checker
#'
#' @param theta A float
#' @return a 3x3 matrix
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")
rotate_x <- function(theta) {
  return(
    matrix(c(
      1, 0, 0,
      0, cos(theta), -sin(theta),
      0, sin(theta), cos(theta)
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}

rotate_y <- function(theta) {
  return(
    matrix(c(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}

rotate_z <- function(theta) {
  return(
    matrix(c(
      cos(theta), -sin(theta), 0,
      sin(theta), cos(theta), 0,
      0, 0, 1
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}

