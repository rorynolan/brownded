#' Bleach a simulation.
#'
#' Remove particles from the simulation at a specified rate. If a particle is
#' removed at a given time-point, it never returns.
#'
#' @param sim An object of class [bbm_sim][bbm_sim_class()]: the output of a
#'   [bbm_sim()].
#' @param rate The rate at which the bleaching should occur. Each particle has
#'   probability `rate` of being removed for the next frame. I.e. `rate` must be
#'   between 0 aand 1 and a low `rate` means not much bleaching, whereas a high
#'   `rate` means a lot of bleaching.
#'
#' @return An object of class [bbm_sim][bbm_sim_class()].
#'
#'
#' @export
bbm_bleach_simulation <- function(sim, rate) {
  assert_class(sim, "bbm_sim")
  assert_number(rate, lower = 0, upper = 1)
  time_points <- unique(sim$t)
  n_time_points <- length(time_points)
  n_particles <- length(unique(sim$id))
  if (n_time_points <= 1) return(sim)
  unbleached <- list(seq_len(n_particles))
  anti_rate <- 1 - rate
  for (i in 2:n_time_points) {
    unbleached[[i]] <- unbleached[[i - 1]] %>% {
      .[purrr::rbernoulli(length(.), p = anti_rate)]
    }
  }
  unbleached %<>% purrr::map2(time_points,
                              ~ tibble::tibble(t = .y, id = .x)) %>%
    purrr::invoke(dplyr::bind_rows, .)
  suppressMessages(dplyr::semi_join(sim, unbleached)) %>%
    bbm_sim_class(attr(sim, "sim_D"), attr(sim, "sim_dim"), bleach_rate = rate)
}
