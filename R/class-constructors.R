#' The 'bbm_sim' class.
#'
#' An object of class `bbm_sim` is a [tibble][tibble::tibble()] with `d + 2`
#' columns, where `d` is the number of dimensions in the space of the
#' simulation. One column for time, one for the particle ID number and the other
#' `d` for the positions of the particles in `d`-dimensional space.
#' \itemize{\item Column 1 has name `t` and is of type `<dbl>`. \item Column 2
#' has name `id` and is of type `<int>`. \item The rest of the columns are named
#' `x1`, `x2`, `x3` and so on and are of type `<dbl>`.} There are three necessary
#' attributes `sim_D`, `sim_dim` and `bleach_rate` which tell you the diffusivity constant,
#' the dimensions of the space used during the simulation and the bleaching rate (see [bbm_bleach_simulation()]).
#'
#' @param tbl An appropriate [tibble][tibble::tibble()].
#' @param sim_D The diffusivity constant used during the simulation which
#'   created `tbl`.
#' @param sim_dim The dimensions of the space used during the simulation which
#'   created `tbl`.
#' @param bleach_rate The bleaching rate (see [bbm_bleach_simulation()]).
#'
#' @return An object of class `bbm_sim`.
#'
#' @examples
#' tbl <- tibble::tibble(t = 0, id = 1L, x1 = 0.5)
#' str(bbm_sim_class(tbl, sim_D = 1, sim_dim = 99.9))
#'
#' @export
bbm_sim_class <- function(tbl, sim_D, sim_dim, bleach_rate = 0) {
  assert_numeric(sim_dim, lower = 0)
  assert_number(sim_D, lower = 0)
  assert_number(bleach_rate, lower = 0, upper = 1)
  if (any(sim_dim <= 0)) {
    stop("All elements of `dim` must be greater than zero.", "\n",
         "The least element of your `dim` is ", min(dim), ".")
  }
  if (sim_D <= 0) {
    stop("sim_D must be greater than zero.", "\n",
         "* You used `sim_D = ", sim_D, "`.")
  }
  assert_class(tbl, "tbl_df")
  namez <- names(tbl)
  if (ncol(tbl) < 3) {
    stop("tbl must have at least 3 columns.", "\n",
         "* Yours has ", ncol(tbl), ".")
  }
  if (namez[2] != "id") {
    stop("The second column of tbl must be named 'id'.", "\n",
         "* Yours is named '", namez[1], "'.")
  }
  if (typeof(tbl$id) != "integer") {
    stop("The column 'id' should have type 'integer'.", "\n",
         "* Yours is of type '", typeof(tbl$id), "'.")
  }
  if (!isTRUE(check_integer(tbl$id, lower = 0))) {
    stop("All elements of the column 'id' must be non-negative.", "\n",
         "* Yours has elements as low as ", min(tbl$id), ".")
  }
  if (namez[1] != "t") {
    stop("The first column of tbl must be named 't'.", "\n",
         "* Yours is named '", namez[2], "'.")
  }
  if (typeof(tbl$t) != "double") {
    stop("The column 't' should have type 'double'.", "\n",
         "* Yours is of type '", typeof(tbl$t), "'.")
  }
  i <- 3
  while (i <= ncol(tbl)) {
    right_name <- paste0("x", i - 2)
    if (namez[i] != right_name) {
      stop("Column ", i, " of tbl must be named 'x", i - 2, "'.", "\n",
           "* Yours is named '", namez[i], "'.")
    }
    if (typeof(tbl[[right_name]]) != "double") {
      stop("The column '", right_name, "' should have type 'double'.", "\n",
           "Yours is of type '", typeof(tbl[[right_name]]), "'.")
    }
    i <- i + 1
  }
  tbl %<>% structure(sim_D = sim_D, sim_dim = sim_dim,
                     bleach_rate = bleach_rate)
  class(tbl) %<>%
    c("bbm_sim", .) %>%
    unique()
  tbl
}
