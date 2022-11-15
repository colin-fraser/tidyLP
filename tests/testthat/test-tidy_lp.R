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
    labels = c("x1", "x2", "x3")
  )
  lp <- tidy_lp(
    df,
    obj,
    const_1 ~ leq(9),
    const_2 ~ leq(15)
  )
  solution <- lp_solve(lp)
  expect_equal(solution$sol$objval, 40.5)

  lp <- tidy_lp(
    df,
    obj,
    const_1 ~ 9,
    const_2 ~ 15
  )
  solution <- lp_solve(lp)
  expect_equal(solution$sol$objval, 40.5)
})


test_that("read_constraint_rhs", {
  expect_equal(read_constraint_rhs(f ~ 3, 'max'), constraint_rhs(3, '<='))
  expect_equal(read_constraint_rhs(f ~ 3, 'min'), constraint_rhs(3, '>='))
  expect_equal(read_constraint_rhs(f ~ leq(3), 'min'), constraint_rhs(3, '<='))
  expect_equal(read_constraint_rhs(f ~ geq(3), 'min'), constraint_rhs(3, '>='))
  expect_equal(read_constraint_rhs(f ~ eq(3), 'min'), constraint_rhs(3, '=='))
}
)
