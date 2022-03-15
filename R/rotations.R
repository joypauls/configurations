IDENTITY_MATRIX <- diag(3)


#' is_rotation checker
#'
#' @param m A matrix
#' @return boolean
#' @export
#'
#' @examples
#' is_rotation(diag(3))
is_rotation <- function(m) {
  if (is.matrix(m)) {
    d <- dim(m)
    mt <- t(m)
    # check if orthonormal with determinant 1
    if ((d[1] == d[2]) && (det(m) == 1) && ((m %*% mt) == IDENTITY_MATRIX)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Rotate wrt x axis
#'
#' @param theta A float
#' @return a 3x3 matrix
#' @export
#'
#' @examples
#' rotate_x(pi)
rotate_x <- function(theta) {
  return(
    matrix(c(
      1, 0, 0,
      0, cos(theta), -sin(theta),
      0, sin(theta), cos(theta)
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}


#' Rotate wrt y axis
#'
#' @param theta A float
#' @return a 3x3 matrix
#' @export
#'
#' @examples
#' rotate_x(pi)
rotate_y <- function(theta) {
  return(
    matrix(c(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}


#' Rotate wrt z axis
#'
#' @param theta A float
#' @return a 3x3 matrix
#' @export
#'
#' @examples
#' rotate_x(pi)
rotate_z <- function(theta) {
  return(
    matrix(c(
      cos(theta), -sin(theta), 0,
      sin(theta), cos(theta), 0,
      0, 0, 1
    ), nrow=3, ncol=3, byrow=TRUE)
  )
}

