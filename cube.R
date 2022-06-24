library(ggplot2)

unit_cube <- cbind(
  c(0, 0, 0),
  c(0, 0, 1),
  c(0, 1, 0),
  c(0, 1, 1),
  c(1, 0, 0),
  c(1, 0, 1),
  c(1, 1, 0),
  c(1, 1, 1)
)

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


# return 3x3
# pure function
# can use general formula instead! faster!
random_rotation <- function() {
  angles <- runif(3, 0, 2*pi)
  return(
    rotate_z(angles[3]) %*% rotate_y(angles[2]) %*% rotate_x(angles[1])
  )
}


# takes 3xN and projects to 2-space
projection <- function(object, axis="x") {
  p <- matrix(c(
    1, 0, 0,
    0, 1, 0
  ), nrow=2, ncol=3, byrow=TRUE)
  return(p %*% object)
}

# unit_cube
# rotate_x(0.5)

coords_to_df <- function(m) {
  df <- as.data.frame(t(m))
  colnames(df) <- c("x", "y")
  return(df)
}


# projection(unit_cube)

# df <- coords_to_df(projection(unit_cube))
# df <- coords_to_df(projection(random_rotation() %*% (unit_cube + 1)))
# df
# ggplot(df, aes(x, y)) +
#   geom_point()


# to exclude origin
unit_cube_adj <- unit_cube + 1

plot_rotations <- function(n=10) {
  vectors <- projection(random_rotation() %*% unit_cube_adj)


  for (i in 1:(n-1)) {
    vectors <- cbind(vectors, projection(random_rotation() %*% unit_cube_adj))
  }

  df <- coords_to_df(vectors)
  ggplot(df, aes(x, y)) +
    geom_point(alpha=0.2, size=0.05, stroke=0.1, color="#FFFFFF") +
    theme_void()
}

# plot_rotations(20000)

# ggsave("test_cube.png", units="px", width=2000, height=2000, bg="#2f2633", dpi="retina")
# ggsave("test_cube.png", units="px", width=2000, height=2000, bg="#FFFFFF", dpi="retina")

t0 <- Sys.time()
plot_rotations(10000)
t1 <- Sys.time()
print(t1-t0)

# ggsave("test_cube.png", units="px", width=2000, height=2000, bg="#2f2633", dpi="retina")
ggsave("test_cube.png", units="px", width=2000, height=2000, bg="#2f2633", dpi="retina")


# plot_rotations <- function(n=10) {
#   vectors <- projection(random_rotation() %*% (unit_cube))
#   for (i in 1:(n-1)) {
#     vectors <- cbind(vectors, projection(random_rotation() %*% (unit_cube)))
#   }
#   df <- coords_to_df(vectors)
#   ggplot(df, aes(x, y)) +
#     geom_point(alpha=0.1, size=0.03, stroke=0) +
#     theme_void()
# }
#
# plot_rotations(10000)
#
# ggsave("test_cube2.png", units="px", width=2000, height=2000, bg="#ffffff", dpi="retina")





