context("Class constructors")

test_that("bbm_sim_class() works", {
  tbl <- tibble::tibble(x = 1, y = 2)
  expect_error(bbm_sim_class(tbl, sim_dim = 3, sim_D = 1), "at least 3 columns")
  tbl <- tibble::tibble(t = 0, id = 1L, x1 = 0.5) %>%
    structure(sim_dim = 3, sim_D = 1)
  ans <- tbl
  class(ans) %<>% c("bbm_sim", .)
  expect_equal(bbm_sim_class(tbl, sim_dim = 3, sim_D = 1), ans)
  expect_error(bbm_sim_class(dplyr::rename(tbl, eyedee = "id"),
                             sim_dim = 3, sim_D = 1),
               "second column.*must be named 'id'")
  expect_error(bbm_sim_class(dplyr::rename(tbl, tee = "t"),
                             sim_dim = 3, sim_D = 1),
               "first column.*must be named 't'")
  expect_error(bbm_sim_class(dplyr::rename(tbl, x = "x1"),
                             sim_dim = 3, sim_D = 1),
               "Column 3.*must be named 'x1'")
  expect_error(bbm_sim_class(dplyr::mutate(tbl, id = id + 0.5),
                             sim_dim = 3, sim_D = 1),
               "The column 'id' should have type 'integer'.")
  expect_error(bbm_sim_class(dplyr::mutate(tbl, t = as.integer(t)),
                             sim_dim = 3, sim_D = 1),
               "The column 't' should have type 'double'.")
  expect_error(bbm_sim_class(dplyr::mutate(tbl, x1 = as.integer(x1)),
                             sim_dim = 3, sim_D = 1),
               "The column 'x1' should have type 'double'.")
  expect_error(bbm_sim_class(dplyr::mutate(tbl, id = -id),
                             sim_dim = 3, sim_D = 1),
               "non-negative")
})
