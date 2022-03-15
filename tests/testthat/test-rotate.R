# counterexample <- matrix()

test_that("Rotation matrix checker", {
  # identity
  expect_true(is_rotation(IDENTITY_MATRIX))
  # rotation matrix builders
  expect_true(is_rotation(rotate_x(pi)))
  expect_true(is_rotation(rotate_y(pi)))
  expect_true(is_rotation(rotate_z(pi)))
  expect_true(is_rotation(rotate_x(pi)))
  expect_true(is_rotation(rotate_y(pi)))
  expect_true(is_rotation(rotate_z(pi)))
})

test_that("Identity matrices", {
  expect_equal(rotate_x(0), IDENTITY_MATRIX)
  expect_equal(rotate_y(0), IDENTITY_MATRIX)
  expect_equal(rotate_z(0), IDENTITY_MATRIX)
})
