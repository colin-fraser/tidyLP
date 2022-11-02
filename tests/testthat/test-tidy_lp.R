test_that("tidy_lp", {
  # from the lpSolve examples:
  #
  # Set up problem: maximize
  #   x1 + 9 x2 +   x3 subject to
  #   x1 + 2 x2 + 3 x3  <= 9
  # 3 x1 + 2 x2 + 2 x3 <= 15
  #
  df <- tibble::tibble(
    obj = c(1, 9, 1),
    const_1 = c(1, 2, 3),
    const_2 = c(3, 2, 2),
    labels = c('x1', 'x2', 'x3')
  )
  lp <- tidy_lp(df,
          obj,
          const_lhs = c(const_1, const_2),
          const_rhs = c(9, 15),
          direction = 'max', obj_labels = labels)
  solution <- lp_solve(lp)
  expect_equal(solution$objval, 40.5)
})

test_that("compute_const_dir", {
  expect_equal(compute_const_dir("<=", "max", 5), rep("<=", 5))
  expect_equal(compute_const_dir(">=", "max", 5), rep(">=", 5))
  expect_equal(compute_const_dir(NULL, "max", 5), rep("<=", 5))
  expect_equal(compute_const_dir(NULL, "min", 5), rep(">=", 5))
  expect_equal(compute_const_dir(c("<=", ">="), "min", 2), c("<=", ">="))
}
)


