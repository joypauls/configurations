test_that("Rotation matrix checker", {
  expect_true(is_rotation(IDENTITY_MATRIX))
})

test_that("Identity matrices", {
  expect_equal(rotate_x(0), IDENTITY_MATRIX)
  expect_equal(rotate_y(0), IDENTITY_MATRIX)
  expect_equal(rotate_z(0), IDENTITY_MATRIX)
})
