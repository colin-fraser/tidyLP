#' Title
#'
#' @param .data
#' @param objective_coefs
#' @param const_lhs
#' @param const_rhs
#' @param direction
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr select pull
tidy_lp <- function(.data,
                    objective_coefs,
                    const_lhs,
                    const_rhs,
                    direction = c('max', 'min'),
                    const_dir = NULL,
                    obj_labels = NULL) {
  direction <- match.arg(direction)
  objective_coefs <- dplyr::pull(.data, {{ objective_coefs }}, name = {{ obj_labels }})
  n <- length(objective_coefs)

  const_lhs <- as.matrix(dplyr::select(.data, {{ const_lhs }}))
  m <- ncol(const_lhs)

  const_dir <- compute_const_dir(const_dir, direction, m)

  LPProblem(.data, objective_coefs, const_lhs, const_dir, const_rhs, direction)
}

#' Infer the constraints direction
#'
#' If the const_dir is length 1, repeats it m times.
#' If it's NULL, infers from the problem direction.
#' If it's m, returns it as is.
#'
#' @noRd
compute_const_dir <- function(const_dir, direction, m) {
  if (length(const_dir) == m) {
    return(const_dir)
  }
  if (is.null(const_dir)) {
    return(switch(direction, "max" = rep("<=", m), "min" = rep(">=", m)))
  }
  if (length(const_dir) == 1) {
    return(rep(const_dir, m))
  }
  stop("const_dir must be length 0, 1, or m.")
}

LPProblem <- function(.data, objective_coefs, const_lhs, const_dir,
                      const_rhs, direction) {
  structure(
    list(
      .data = .data,
      objective_coefs = objective_coefs,
      const_lhs = const_lhs,
      const_dir = const_dir,
      const_rhs = const_rhs,
      direction = direction
    ),
    class = 'LPProblem'
  )
}

#' Solve tidy_lp problem
#'
#' @param lp_problem
#'
#' @return
#' @export
#'
#' @examples
lp_solve <- function(lp_problem, ...) {
  lpSolve::lp(direction = lp_problem$direction,
     objective.in = lp_problem$objective_coefs,
     const.mat = lp_problem$const_lhs,
     const.dir = lp_problem$const_dir,
     const.rhs = lp_problem$const_rhs,
     transpose.constraints = FALSE,
     ...)
}

parse_constraint <- function(expr) {
  TRUE
}

parse_constraint_string <- function(constraint_string) {
  check_valid_constraint_string(constraint_string)
}

check_valid_constraint_string <- function(constraint_string) {
  TRUE
}
