#' Simulate bounded brownian motion.
#'
#' Given the details of a rectangular boundary in 1, 2 or 3 dimensions, and a
#' diffusion constant, simulate the brownian trajectories of particles.
#'
#' @param n_particles A natural number. The number of particles to use in the
#'   simulation.
#' @param D The diffusivity constant.
#' @param dim A vector giving the dimensions of the simulation space.
#' @param time_interval The time interval between observations.
#' @param end_time The time at which the simulation should end. The last
#'   timpoint in the simulation will be before or at this time.
#' @param init_pos An \eqn{n} x d matrix giving the initial positions of the `n`
#'   particles. If this is not set, then a random configuration is generated.
#'
#' @return An object of class `bbm_sim`: a [tibble][tibble::tibble()] with `d +
#'   2` columns, where `d` is the number of dimensions in the space of the
#'   simulation. One column for time, one for the particle ID number and the
#'   other `d` for the positions of the particles in `d`-dimensional space.
#'
#' @seealso [bbm_simulate_img()]
#'
#' @examples
#' bbm_simulate(n_particles = 2, D = 1.5, dim = c(3, 5), end_time = 2)
#'
#' @export
bbm_simulate <- function(n_particles, D, dim, time_interval = 1, end_time,
                         init_pos = NULL) {
  assert_int(n_particles, lower = 1)
  if (n_particles > .Machine$integer.max) {
    stop("The number of particles must be less than or equal to ",
         .Machine$integer.max, ".")
  }
  assert_number(D, lower = 0)
  if (D == 0) {
    stop("D must be greater than zero.", "\n",
         "* You used `D = ", D, "`.")
  }
  assert_numeric(dim, lower = 0, min.len = 1)
  if (any(dim <= 0)) {
    stop("All elements of `dim` must be greater than zero.", "\n",
         "The least element of your `dim` is ", min(dim), ".")
  }
  assert_number(time_interval, lower = 0)
  assert_number(end_time, lower = 0)
  if (time_interval == 0) stop("time_interval must be greater than zero.")
  if (time_interval > end_time)
    stop("time_interval must be less than or equal to end_time.")
  if (is.null(init_pos)) {
    init_pos <- purrr::map(dim, ~ runif(n_particles, max = .)) %>%
      purrr::reduce(cbind)
  }
  if (length(dim) == 1) init_pos %<>% as.matrix()
  assert_matrix(init_pos)
  assert_numeric(init_pos)
  if (nrow(init_pos) != n_particles) {
    stop("You have said there are ", n_particles, " particles, but you have ",
         "specified initial positions for ", nrow(init_pos), " particles.")
  }
  if (length(dim) != ncol(init_pos)) {
    stop("You have specified ", length(dim), " dimensions, but you have ",
         "also specified initial positions for particles in ", ncol(init_pos),
         " dimensions.")
  }
  init_pos %<>% enlist_cols()
  out <- purrr::pmap(list(positions = init_pos, boundary = dim),
                     one_dim_simulate, D = D,
                     time_interval = time_interval, end_time = end_time) %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::map2(paste0("x", seq_along(.)),
                ~ dplyr::rename(.x, !!.y := pos)) %>% {
      xs <- purrr::map(., ~ .x[stringr::str_detect(names(.x), "^x\\d+$")])
      purrr::invoke(dplyr::bind_cols, c(dplyr::select(.[[1]], -x1), xs))
    } %>%
    assertr::assert(!anyNA(.)) %>%
    dplyr::mutate(id = as.integer(id))
  bbm_sim_class(out, sim_dim = dim, sim_D = D)
}

one_dim_simulate_one <- function(positions, boundary, D, time_interval) {
  assert_atomic_vector(positions, min.len = 1)
  assert_number(boundary, lower = 0)
  assert_numeric(positions, lower = 0, upper = boundary)
  assert_numeric(positions)
  assert_number(time_interval, lower = 0)
  if (time_interval == 0) stop("time_interval must be greater than zero.")
  sigma <- sqrt(2 * D * time_interval)
  moves <- rnorm(length(positions), sd = sigma) %% (2 * boundary)
  positions %<>% {. + moves}
  indices <- which(positions < -boundary)  ## leftleft indices
  positions[indices] %<>% {. + 2 * boundary}
  indices <- which(positions < 0)  ## left indices
  positions[indices] %<>% {-.}
  indices <- which(positions > 2 * boundary)  ## rightright indices
  positions[indices] %<>% {. - 2 * boundary}
  indices <- which(positions > boundary)  ## right indices
  positions[indices] %<>% {. - 2 * (. - boundary)}
  assert_numeric(positions, lower = 0, upper = boundary)
  positions
}

one_dim_simulate <- function(positions, boundary, D, time_interval, end_time) {
  assert_atomic_vector(positions, min.len = 1)
  assert_number(boundary, lower = 0)
  assert_numeric(positions, lower = 0, upper = boundary)
  assert_numeric(positions)
  assert_number(time_interval, lower = 0)
  assert_number(end_time, lower = 0)
  if (time_interval == 0) stop("time_interval must be greater than zero.")
  if (time_interval > end_time)
    stop("time_interval must be less than or equal to end_time.")
  n_reps <- floor(end_time %/% time_interval)
  n_pos <- length(positions)
  out <- numeric(n_pos * (n_reps + 1))
  slnp <- seq_len(n_pos)
  out[slnp] <- positions
  for (i in seq_len(n_reps)) {
    positions %<>% one_dim_simulate_one(boundary = boundary, D = D,
                                        time_interval = time_interval)
    out[(i * n_pos) + slnp] <- positions
  }
  cbind(t = rep(seq(0, end_time, by = time_interval), each = n_pos),
        id = rep(seq_len(n_pos), n_reps + 1),
        pos = out)
}
