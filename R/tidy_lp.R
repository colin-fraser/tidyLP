TidyLP <- function(data, objective, constraints, direction,
                   all_int = FALSE, all_bin = FALSE) {
  structure(
    list(
      data = data, objective = rlang::enquo(objective),
      constraints = constraints,
      direction = direction,
      all_int = all_int,
      all_bin = all_bin
    ),
    class = "TidyLP"
  )
}

check_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }

  for (col_name in colnames(data)) {
    column <- data[[col_name]]

    if (any(is.na(column))) {
      stop(paste("Column", col_name, "contains NA values"))
    }

    if (any(is.null(column))) {
      stop(paste("Column", col_name, "contains NULL values"))
    }

    if (any(is.infinite(column))) {
      stop(paste("Column", col_name, "contains infinite values"))
    }
  }

  TRUE
}

#' A pretty representation of a tidy LP problem
#'
#' @param x a tidy LP problem
#'
#' @return as string
#' @export
#'
pretty_tlp <- function(x) {
  obj <- paste(x$direction, rlang::quo_name(x$objective))
  consts <- paste("  ", sapply(x$constraints, pretty_constraint),
    collapse = "\n"
  )
  paste(obj, "subject to:", consts, sep = "\n")
}

#' Create a TidyLP object
#'
#' @param .data a data.frame that will be used to construct the object
#' @param .objective an expression for the objective function
#' @param ... constraints
#' @param .direction the direction of obtimization
#' @param .all_int should the solution be integer-valued?
#' @param .all_bin should the solution be binary-valued?
#'
#' @return a TidyLPProblem object
#' @export
#'
tidy_lp <- function(.data, .objective, ..., .direction = "max",
                    .all_int = FALSE, .all_bin = FALSE) {
  check_data(.data)
  TidyLP(.data, {{ .objective }},
    constraints_from_formulas(list(...), direction = .direction, env = parent.frame(n = 1)),
    direction = .direction, all_int = .all_int, all_bin = .all_bin
  )
}

#' Solve a TidyLP
#'
#' @param tlp a TidyLP object
#' @param ... additional arguments passed to the solver
#' @param solver the solver to use. Currently does nothing as only lpSolve is implemented
#'
#' @return the solution
#' @export
#'
lp_solve <- function(tlp, ..., solver = "lpSolve") {
  constraints <- materialize_constraints(tlp)
  solution <- lpSolve::lp(
    direction = tlp$direction,
    objective.in = build_objective_coefficients(tlp),
    const.mat = constraints$lhs,
    const.dir = constraints$dir,
    const.rhs = constraints$rhs,
    transpose.constraints = FALSE,
    all.int = tlp$all_int,
    all.bin = tlp$all_bin
  )
  TidyLPSolution(solution, tlp)
}

#' Add constraints to a tidy_lp
#'
#' @param tlp a tidy_lp object
#' @param ... unnamed constraints specified as formulas
#'
#' @export
add_constraints <- function(tlp, ...) {
  new_constraints <- constraints_from_formulas(list(...), tlp$direction)
  tlp$constraints <- c(tlp$constraints, new_constraints)
  tlp
}


build_objective_coefficients <- function(tlp) {
  dplyr::transmute(tlp$data, !!tlp$objective)
}

materialize_constraint <- function(constraint, tlp) {
  lhs <- as.matrix(dplyr::transmute(tlp$data, !!constraint$lhs))
  n <- ncol(lhs)
  dir <- rep(constraint$direction, n)
  rhs <- rep(constraint$rhs, n)
  list(lhs = lhs, dir = dir, rhs = rhs)
}

materialize_constraints <- function(tlp) {
  materialized_constraints <- purrr::map(tlp$constraints, materialize_constraint, tlp = tlp)
  list(
    lhs = do.call(cbind, purrr::map(materialized_constraints, "lhs")),
    dir = do.call(c, purrr::map(materialized_constraints, "dir")),
    rhs = do.call(c, purrr::map(materialized_constraints, "rhs"))
  )
}

build_constraint_matrix <- function(tlp) {
  constraints <- purrr::map(tlp$constraints, "lhs")
  as.matrix(do.call(dplyr::transmute, rlang::list2(tlp$data, !!!constraints)))
}

build_constraint_direction_vector <- function(tlp) {
  purrr::map_chr(tlp$constraints, c("rhs", "direction"))
}

build_constraint_rhs <- function(tlp) {
  purrr::map_dbl(tlp$constraints, c("rhs", "value"))
}

# Constraints ---------------------------------------------------------------------------------

constraint <- function(lhs, rhs) {
  list(lhs = lhs, direction = rhs$direction, rhs = rhs$value)
}

constraint_rhs <- function(value, direction) {
  list(value = value, direction = direction)
}

#' Constraint direction helpers
#'
#' @param value the right hand side of the constraint
#'
#' @description
#'   These functions are used to specify the direction of the constraint
#'   formulas. These should only be used within a constraint formula.
#'
#'   `geq` specifies a >= constraint
#'   `leq` specifies a <= constraint
#'   `eq` denotes a == constraint
#'
#' @return a `constraint_rhs` object
#' @export
#'
#'
leq <- function(value) {
  constraint_rhs(value, "<=")
}

#' @rdname leq
geq <- function(value) {
  constraint_rhs(value, ">=")
}

#' @rdname leq
eq <- function(value) {
  constraint_rhs(value, "==")
}

#' export
all_variables <- function() {
  1
}

constraints_from_formulas <- function(const_list, direction, env) {
  constraint_from_formula <- function(fml, direction) {
    lhs <- read_constraint_lhs(fml)
    rhs <- read_constraint_rhs(fml, direction, env)
    constraint(lhs, rhs)
  }
  purrr::map(const_list, constraint_from_formula, direction = direction)
}



read_constraint_lhs <- function(fml) {
  fml[[2]]
}

read_constraint_rhs <- function(fml, direction, env = parent.frame()) {
  x <- fml[[3]]
  if (is.call(x)) {
    if (any(x[[1]] == quote(geq), x[[1]] == quote(leq), x[[1]] == quote(eq))) {
      out <- eval(x, env) # evaluate in the calling frame
    } else {
    stop("RHS of constraint formula must be either a call to geq, leq, eq; or numeric")
    }
  } else if (is.numeric(eval(x, env))) {
    out <- constraint_rhs(eval(x, env), infer_constraint_direction(direction))
  } else {
    stop("RHS of constraint formula must be either a call to geq, leq, eq; or numeric")
  }
  out
}

infer_constraint_direction <- function(direction) {
  switch(direction,
    "max" = "<=",
    "min" = ">="
  )
}

#' String representation of a constraint
#'
#' @param x a constraint
#'
#' @return a string
#' @export
#'
pretty_constraint <- function(x) {
  paste(rlang::quo_name(x$lhs), x$direction, x$rhs)
}


# group_constraint ----------------------------------------------------------------------------


#' Categorical Constraints
#'
#' @param col the column to generate a categorical constraint set from
#'
#' @return a tibble
#' @export
#' @importFrom tidyr spread
#' @importFrom dplyr select mutate
categorical_constraint <- function(col) {
  tibble::tibble(
    .x1 = col,
    .x2 = 1
  ) |>
    mutate(.n = dplyr::row_number()) |>
    tidyr::spread(.data$.x1, .data$.x2, fill = 0) |>
    select(-.data$.n)
}




# TidyLPSolution ------------------------------------------------------------------------------
TidyLPSolution <- function(sol, tlp) {
  structure(list(sol = sol, tlp = tlp), class = "TidyLPSolution")
}


#' Bind the solution to the original data
#'
#' Returns the original supplied data frame with a new column containing the solution.
#'
#' @param tidy_lp_solution a solved TidyLPSolution
#' @param filter_nonzero if true, remove rows corresponding to 0's in the solution
#' @param name the name of the solution column

#' @export
bind_solution <- function(tidy_lp_solution,
                          filter_nonzero = FALSE,
                          name = ".solution") {
  tidy_lp_solution$tlp$data[name] <- tidy_lp_solution$sol$solution
  if (filter_nonzero) {
    tidy_lp_solution$tlp$data <- dplyr::filter(tidy_lp_solution$tlp$data, tidy_lp_solution$sol$solution > 0)
  }
  tidy_lp_solution$tlp$data
}


# Data ----------------------------------------------------------------------------------------

#' Fantasy Points
#'
#' A modified and stripped down version of a fantasy basketball dataset originating from Fanduel.
#'
#' @format ## `fantasy_points`
#' A data frame with 322 rows and 6 columns.
#'
#' \describe{
#'   \item{Id}{A unique identifier for the player}
#'   \item{Position}{The player's position}
#'   \item{Name}{The player's name}
#'   \item{Projected_FP}{The player's projected fantasy points}
#'   \item{Salary}{The player's salary}
#'   \item{Team}{The player's NBA team}
#' }
"fantasy_points"
