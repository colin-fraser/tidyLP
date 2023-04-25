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

  # check that constraints work for calls to const rhs functions
  lp <- tidy_lp(
    df,
    obj,
    const_1 ~ leq(9),
    const_2 ~ leq(15)
  )
  solution <- lp_solve(lp)
  expect_equal(solution$sol$objval, 40.5)

  # check that constraints work for constant rhs calls
  lp <- tidy_lp(
    df,
    obj,
    const_1 ~ 9,
    const_2 ~ 15
  )
  solution <- lp_solve(lp)
  expect_equal(solution$sol$objval, 40.5)

  # check that constraints rhs functions work in function calls with variables in the rhs
  solver <- function(rhs1, rhs2) {
    lp <- tidy_lp(
      df,
      obj,
      const_1 ~ leq(rhs1),
      const_2 ~ leq(rhs2)
    )

    lp_solve(lp)
  }
  expect_equal(solver(9, 15)$sol$objval, 40.5)

  # check that constraints rhs functions work in function calls with variables in the rhs
  solver <- function(rhs1, rhs2) {
    lp <- tidy_lp(
      df,
      obj,
      const_1 ~ rhs1,
      const_2 ~ rhs2
    )

    lp_solve(lp)
  }
  expect_equal(solver(9, 15)$sol$objval, 40.5)
})


test_that("read_constraint_rhs", {
  expect_equal(read_constraint_rhs(f ~ 3, "max"), constraint_rhs(3, "<="))
  expect_equal(read_constraint_rhs(f ~ 3, "min"), constraint_rhs(3, ">="))
  expect_equal(read_constraint_rhs(f ~ leq(3), "min"), constraint_rhs(3, "<="))
  expect_equal(read_constraint_rhs(f ~ geq(3), "min"), constraint_rhs(3, ">="))
  expect_equal(read_constraint_rhs(f ~ eq(3), "min"), constraint_rhs(3, "=="))
  expect_error(read_constraint_rhs(f ~ sum(3), 'max'), regexp = "RHS of constraint formula must be either a call to geq, leq, eq; or numeric")
})

test_that("tidy_lp raises error for NA values", {
  test_data <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
  expect_error(tidy_lp(test_data, a < 1), "Column a contains NA values")
})

test_that("tidy_lp raises error for infinite values", {
  test_data <- data.frame(a = c(1, 2, Inf), b = c(4, 5, 6))
  expect_error(tidy_lp(test_data, a < 1), "Column a contains infinite values")
})

test_that("tidy_lp raises error for infinite values", {
  test_data <- data.frame(a = c(1, 2, -Inf), b = c(4, 5, 6))
  expect_error(tidy_lp(test_data, a < 1), "Column a contains infinite values")
})

test_that("tidy_lp works with clean data", {
  test_data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  expect_silent(tidy_lp(test_data, a < 1))
})

test_that("tidy_lp solves the tables and chairs problem correctly", {
  df <- tibble::tribble(
    ~ product_type, ~ metal_units, ~ wood_units, ~ profit,
    'table', 1, 3, 200,
    'chair', 2, 1, 100
  )

  solution <- df |>
    tidy_lp(profit, metal_units ~ leq(6000), wood_units ~ leq(9000)) |>
    lp_solve() |>
    bind_solution(name = 'quantity') |>
    select(product_type, quantity)

  expected_solution <- tibble::tribble(
    ~ product_type, ~ quantity,
    'table', 2400,
    'chair', 1800
  )

  expect_equal(solution, expected_solution)
})

test_that("tidy_lp solves the fantasy sports problem correctly", {
  team <- fantasy_points |>
    tidy_lp(
      Projected_FP,
      Salary ~ leq(60000),
      (Position == 'PG') ~ eq(2),
      (Position == 'SG') ~ eq(2),
      (Position == 'SF') ~ eq(2),
      (Position == 'PF') ~ eq(2),
      (Position == 'C') ~ eq(1),
      categorical_constraint(Team) ~ leq(2),
      .all_bin = TRUE
    ) |>
    lp_solve() |>
    bind_solution(filter_nonzero = TRUE) |>
    select(Name, Position, Projected_FP, Salary, Team)

  expect_lt(sum(team$Salary), 60000) # Check if the total salary is 60000
  expect_equal(table(team$Position), table(c('C', rep(c('PF', 'PG', 'SF', 'SG'), 2)))) # Check if the positions' constraints are met
  expect_true(all(table(team$Team) <= 2)) # Check if the team constraint is met
})
