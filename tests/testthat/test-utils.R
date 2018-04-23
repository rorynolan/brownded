context("Utils")

test_that("coords_to_arr() works", {
  coords <- tibble::tribble(
    ~x1, ~x2, ~value,
    1, 1, 3,
    1, 2, 7,
    2, 2, 0
  )
  a <- array(NA, dim = c(2, 2))
  a[1, 1] <- 3
  a[1, 2] <- 7
  a[2, 2] <- 0
  expect_equal(coords_to_arr(coords) %>% {list(., dim(.))}, list(a, dim(a)),
               check.attributes = FALSE)
  coords <- tibble::tribble(
    ~x1, ~x2, ~x3, ~value,
    1, 1, 3, 3,
    1, 2, 2, 7,
    2, 2, 1, 0
  )
  a <- array(NA, dim = c(2, 2, 3))
  a[1, 1, 3] <- 3
  a[1, 2, 2] <- 7
  a[2, 2, 1] <- 0
  expect_equal(coords_to_arr(coords) %>% {list(., dim(.))}, list(a, dim(a)),
               check.attributes = FALSE)
  expect_error(coords_to_arr(tibble::tibble(x = 1)), "at least two columns")
  df <- data.frame(x = 1, y = 2)
  expect_error(coords_to_arr(df), "Your .* `df` must have a column .* 'value'.")
  names(df) <- c("x", "x")
  expect_error(coords_to_arr(df), "You have a duplicate name 'x'")
  df <- tibble::tibble(x1 = 1, value = list(1))
  expect_error(coords_to_arr(df), "The 'value' column .* must be atomic.")
  df <- tibble::tibble(x = 1, value = 1)
  expect_error(coords_to_arr(df), "non-compliant column name 'x'.$")
  df %<>% dplyr::rename(x0 = "x")
  expect_error(coords_to_arr(df), "column named 'x0'.")
  df %<>% dplyr::rename(x2 = "x0")
  expect_error(coords_to_arr(df), "must have a column named 'x1'.")
  df %<>%
    dplyr::rename(x1 = "x2") %>%
    dplyr::mutate(x3 = 5)
  expect_error(coords_to_arr(df), "missing dimension.*x1.*and.*x3.*but no.*x2")
  df %<>% dplyr::mutate(x2 = 2.5)
  expect_error(coords_to_arr(df), "column.*x2.*must be of type.*integer")
  df %<>% dplyr::mutate(x2 = -1)
  expect_error(coords_to_arr(df), "column.*x2.*must.*greater than zero")
  df %<>% dplyr::mutate(x2 = 1)
  df[2, ] <- df[1, ]
  expect_error(coords_to_arr(df), "row 2 is a coordinate duplicate")
})
