#' Create an image series from a `bbm_sim`.
#'
#' Given an output of [bbm_sim()], create an image in the style of an
#' [ijtiff_img][ijtiff::ijtiff_img] where each frame represents a time-point in
#' the simulation. You must set the pixel size and there is the option for the
#' pixel values to be counts of the number of molecules in that pixel, or
#' Poisson photon counts where each particle has a user-defined brightness.
#'
#' @param bbms An object of class [bbm_sim][bbm_sim_class]; the result of a
#'   simulation with [bbm_sim()]. This must be 2-dimensional.
#' @param pixel_size The pixel size for the image. A grid of pixels is filled
#'   out over the simulation space, starting at the bottom-left (the origin).
#'   Any pixels crossing the boundary of the simulation are discarded.
#' @param method How shall the pixel values be calculated? `method = "count"`
#'   results in each pixel value being a count of the number of particles
#'   therein. `method = "poisson"` results in each particle being treated as a
#'   Poisson-emmitter of photons with rate specified in the `brightness`
#'   parameter.
#' @param brightness Required only if `method = poisson`. The poisson photon
#'   emmission rate of the diffusing particles in units of *per frame*. This is
#'   the brightness 'epsilon' referred to in the field of number and brightness,
#'   a method in fluorescence fluctuation spectroscopy.
#' @param check_duplicates Check for duplicate coordinates at each time point of the
#'   simulation, throwing an error if any are present. One reason to turn this
#'   off is for speed; the check slows things down slightly. If it's off and
#'   `df` does contain duplicate coordinates, the value associated with the last
#'   duplicate coordinate will count.
#'
#' @return An [ijtiff_img][ijtiff::ijtiff_img].
#'
#' @seealso [bbm_simulate_img]
#'
#' @examples
#' sim <- bbm_simulate(n_particles = 2, D = 1.5, dim = c(3, 5), end_time = 2)
#' bbm_simulation_to_img(sim, pixel_size = 1, method = "c")
#' bbm_simulation_to_img(sim, pixel_size = 1, method = "p", brightness = 9)
#'
#' @export
bbm_simulation_to_img <- function(bbms, pixel_size,
                                  method = c("count", "poisson"),
                                  brightness = NULL,
                                  check_duplicates = TRUE) {
  assert_class(bbms, "bbm_sim")
  if (ncol(bbms) != 4) {
    stop("This function only works for 2-dimensional simulations.", "\n",
         "* Yours is ", ncol(bbms) - 2, "-dimensional.")
  }
  assert_number(pixel_size, lower = 0)
  if (pixel_size == 0) {
    stop("pixel_size must be greater than zero.", "\n",
         "* You have pixel_size = ", pixel_size, ".")
  }
  if (filesstrings::all_equal(method, c("count", "poisson"))) {
    stop("You must select a method.", "\n",
         "* Choose either \"count\" or \"poisson\".")
  }
  assert_string(method)
  method %<>% RSAGA::match.arg.ext(c("count", "poisson"), ignore.case = TRUE)
  if (method == "poisson") {
    if (is.null(brightness)) {
      stop("When you select `method = \"poisson\"`, you must specify ",
           "brightness.", "\n",
           "* You have left `brightness = NULL`.")
    }
    assert_number(brightness, lower = 0)
  }
  sim_D <- attr(bbms, "sim_D")
  sim_dim <- attr(bbms, "sim_dim")
  bbms %<>%
    dplyr::mutate(
      px_vertical = assign_pixel_one_dim(x1, boundary = sim_dim[1],
                                           pixel_size = pixel_size),
      px_horizontal = assign_pixel_one_dim(x2, boundary = sim_dim[2],
                                         pixel_size = pixel_size)) %>%
    dplyr::group_by(px_horizontal, px_vertical, t) %>%
    dplyr::summarise(pixel_value = n()) %>%
    dplyr::ungroup()
  extreme_point <- tibble::tibble(
    px_vertical = assign_pixel_one_dim(sim_dim[1], boundary = sim_dim[1],
                                       pixel_size = pixel_size),
    px_horizontal = assign_pixel_one_dim(sim_dim[2], boundary = sim_dim[2],
                                         pixel_size = pixel_size),
    t = bbms$t[1], pixel_value = 0)
  if (nrow(plyr::match_df(bbms, extreme_point, on = c("px_horizontal",
                                                      "px_vertical"))) == 0) {
    bbms %<>% dplyr::bind_rows(extreme_point, .)
  }
  if (method == "poisson") {
    bbms %<>% dplyr::mutate(pixel_value = rpois(nrow(.),
                                                pixel_value * brightness))
  }
  img <- dplyr::rename(bbms, x1 = "px_vertical", x2 = "px_horizontal",
                       x3 = "t", value = "pixel_value") %>%
    dplyr::mutate(x3 = dplyr::dense_rank(x3)) %>%
    coords_to_arr(missing_value = 0, check_duplicates = check_duplicates)
  dim(img) %<>% {c(.[1:2], 1, .[3])}
  ijtiff::ijtiff_img(img)
}

one_time_to_count_matrix <- function(pos, pixel_size, dim,
                                     check_duplicates = TRUE) {
  assert_matrix(pos, ncols = 2)
  assert_number(pixel_size, lower = 0)
  assert_numeric(dim, lower = 0, len = 2)
  if (pixel_size == 0) {
    stop("pixel_size must be greater than zero.", "\n",
         "* You have pixel_size = ", pixel_size, ".")
  }
  pos %<>% magrittr::set_colnames(c("x1", "x2")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(x1 = assign_pixel_one_dim(x1, boundary = dim[1],
                                            pixel_size = pixel_size),
                  x2 = assign_pixel_one_dim(x2, boundary = dim[2],
                                            pixel_size = pixel_size)) %>%
    dplyr::group_by(x1, x2) %>%
    dplyr::summarise(value = n()) %>%
    dplyr::ungroup()
  extreme_point <- tibble::tibble(
    x1 = assign_pixel_one_dim(dim[1], boundary = dim[1],
                              pixel_size = pixel_size),
    x2 = assign_pixel_one_dim(dim[2], boundary = dim[2],
                              pixel_size = pixel_size),
    value = 0)
  if (nrow(dplyr::anti_join(extreme_point, pos, by = c("x1", "x2"))) != 0)
    pos %<>% dplyr::bind_rows(extreme_point, .)
  coords_to_arr(pos, missing_value = 0, check_duplicates = check_duplicates)
}

assign_pixel_one_dim <- function(pos, boundary, pixel_size) {
  assert_number(boundary, lower = 0)
  assert_atomic_vector(pos)
  assert_numeric(pos, lower = 0, upper = boundary)
  px <- pos %/% pixel_size + 1
  if (boundary %% pixel_size == 0)
    px[px == boundary %/% pixel_size + 1] %<>% {. - 1}
  px
}

#' Simulate an image series of particles undergoing bounded Brownian motion.
#'
#' Simulate an image series in the style of an [ijtiff_img][ijtiff::ijtiff_img]
#' where each frame represents a time-point in bounded Brownian motion. There is
#' the option for the pixel values to be counts of the number of molecules in
#' that pixel, or Poisson photon counts where each particle has a user-defined
#' brightness.
#'
#' The function [bbm_simulation_to_img()] converts an existing simulation into
#' an image series. This function does the simulation and creation of image
#' series simultaneously. This is more memory-efficient (but maybe not more
#' time-efficient). This means for some simulations which were impossible to run
#' via [bbm_simulate] (due to memory constraints), you may still be able to get
#' their associated image series with this function.
#'
#' @inheritParams bbm_simulate
#' @inheritParams bbm_simulation_to_img
#'
#' @return An [ijtiff_img][ijtiff::ijtiff_img].
#'
#' @seealso [bbm_simulation_to_img()]
#'
#' @examples
#' bbm_simulate_img(n_particles = 2, D = 1.5, dim = c(3, 5), end_time = 2,
#'                  pixel_size = 1, method = "p", brightness = 9)
#'
#' @export
bbm_simulate_img <- function(n_particles, D, dim, time_interval = 1, end_time,
                             pixel_size, method = c("count", "poisson"),
                             brightness = NULL, init_pos = NULL) {
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
  assert_number(pixel_size, lower = 0)
  if (pixel_size == 0) {
    stop("pixel_size must be greater than zero.", "\n",
         "* You have pixel_size = ", pixel_size, ".")
  }
  if (filesstrings::all_equal(method, c("count", "poisson"))) {
    stop("You must select a method.", "\n",
         "* Choose either \"count\" or \"poisson\".")
  }
  assert_string(method)
  method %<>% filesstrings::match_arg(c("count", "poisson"),
                                      ignore_case = TRUE)
  if (method == "poisson") {
    if (is.null(brightness)) {
      stop("When you select `method = \"poisson\"`, you must specify ",
           "brightness.", "\n",
           "* You have left `brightness = NULL`.")
    }
    assert_number(brightness, lower = 0)
  }
  if (length(dim) != 2) {
    stop("This function only works for 2-dimensional simulations.", "\n",
         "* Yours is ", length(dim), "-dimensional.")
  }
  frames <- list(one_time_to_count_matrix(init_pos, pixel_size = pixel_size,
                                          dim = dim, check_duplicates = FALSE))
  pos <- init_pos
  t <- time_interval
  i <- 2
  while (t <= end_time) {
    for (j in 1:2) {
      pos[, j] %<>% one_dim_simulate_one(boundary = dim[j], D = D,
                                         time_interval = time_interval)
    }
    frames[[i]] <- one_time_to_count_matrix(pos, pixel_size = pixel_size,
                                            dim = dim, check_duplicates = FALSE)
    t <- t + time_interval
    i <- i + 1
  }
  out <- unlist(frames)
  if (method == "poisson") out %<>% {rpois(length(.), . * brightness)}
  dim(out) <- c(dim(frames[[1]]), 1, length(frames))
  ijtiff::ijtiff_img(out)
}

