enlist_cols <- function(mat) purrr::map(seq_len(ncol(mat)), ~ mat[, .])

enlist_rows <- function(mat) purrr::map(seq_len(nrow(mat)), ~ mat[., ])


#' Convert a data frame of coordinates and values to an array.
#'
#' Given a data frame with columns `x1`, `x2`, `x3`, . . . , `xn` and a column
#' `value`, create an array `arr` of dimension `n` where `arr[x1, x2, . . ., xn]
#' = value`.
#'
#' @param df A data frame whose columns are named `x1`, `x2`, `x3`, . . . , `xn`
#'   and `value` (there can be no other columns). The `x` columns must be
#'   positive integers.
#' @param missing_value Coordinates of `arr` not specified in `df` are set to
#'   this value (default `NA`).
#' @param check_duplicates Check for duplicate coordinates in the data frame `df`,
#'   throwing an error if any are present. One reason to turn this off is for
#'   speed; the check slows things down slightly. If it's off and `df` does
#'   contain duplicate coordinates, the value associated with the last duplicate
#'   coordinate will count.
#'
#' @return An `n` dimensional array with the same type as `df$value`.
#'
#' @examples
#' coords <- tibble::tribble(~x1, ~x2, ~value,
#'                             1,   1,      3,
#'                             1,   2,      7,
#'                             2,   2,      0)
#' coords_to_arr(coords)
#' coords <- tibble::tribble(~x1, ~x2, ~ x3, ~value,
#'                             1,   1,    3,      3,
#'                             1,   2,    2,      7,
#'                             2,   2,    1,      0)
#' coords_to_arr(coords)
#' @export
coords_to_arr <- function(df, missing_value = NA, check_duplicates = TRUE) {
  checkmate::assert_data_frame(df)
  checkmate::assert_scalar(missing_value, na.ok = TRUE)
  if (ncol(df) < 2) {
    stop("Your data frame `df` must have at least two columns.", "\n",
         "* If it has exactly 2 columns, ", "
         these should be named 'x1' and 'value'.")
  }
  namez <- colnames(df)
  if (anyDuplicated(namez)) {
    stop("None of the names in your data frame `df` may be duplicated.", "\n",
         "* You have a duplicate name '", namez[anyDuplicated(namez)], "'.")
  }
  df %<>%
    janitor::clean_names() %>%
    dplyr::select(sort(dplyr::current_vars()))
  namez <- colnames(df)
  if (! "value" %in% namez)
    stop("Your data frame `df` must have a column named 'value'.")
  if (is.list(df$value)) {
    stop("The 'value' column of the data frame `df` must be atomic.", "\n",
         "* Your 'value' column is a list. This is not allowed.")
  }
  non_val_names <- setdiff(namez, "value")
  good_non_val_names <- stringr::str_detect(non_val_names, "^x\\d+$")
  if (!all(good_non_val_names)) {
    stop("Apart from the 'value' column, all of the columns in the data frame ",
         "`df` must be named as 'xi' where 'i' is a positive integer.", "\n",
         "* Your data frame has the non-compliant column name '",
         non_val_names[match(F, good_non_val_names)], "'.")
  }
  non_val_names %<>% filesstrings::nice_nums()
  names(df)[-1] <- non_val_names
  df %<>% dplyr::select(sort(dplyr::current_vars()))
  names(df)[-1] %<>% {paste0("x", filesstrings::first_number(.))}
  namez <- colnames(df)
  x_names <- namez[-1]
  if (filesstrings::first_number(x_names)[1] != 1) {
    if (filesstrings::first_number(x_names)[1] == 0) {
      stop("The data frame `df` must not have a column named 'x0'.", "\n",
       "* Yours has a column named 'x0'.")
    }
    stop("The data frame `df` must have a column named 'x1'. ", "\n",
         "* The lowest you have is ", x_names[1], ".")
  }
  for (i in seq_along(x_names)[-1]) {
    if (filesstrings::first_number(x_names[i]) != i) {
      stop("You must not have missing dimension coordinates in the data frame ",
           "`df`.", "\n",
           "* You have 'x", i - 1, "' and '", x_names[i], "' but no '",
           paste0("x", i), "'.")
    }
    if (!isTRUE(checkmate::check_integerish(df[[x_names[i]]]))) {
      stop("The column '", x_names[i], "' of the data frame `df` must be of ",
           "type 'integer'. ", "\n",
           "* Yours is of type '", typeof(df[[x_names[i]]]), "'.")
    }
    if (min(df[[x_names[i]]]) < 1) {
      stop("The column '", x_names[i], "' of the data frame `df` must have all",
           " values greater than zero. ", "\n",
           "* The minimum value in yours is ", min(df[[x_names[i]]]), ".")
    }
  }
  noval_df <- dplyr::select(df, -value)
  if (check_duplicates && anyDuplicated(noval_df)) {
    stop("Coordinate values in the data frame `df` may not be duplicated.\n",
         "* In your `df`, row ", anyDuplicated(noval_df),
         " is a coordinate duplicate.")
  }
  out <- array(missing_value, dim = purrr::map_dbl(noval_df, max))
  out[data.matrix(noval_df)] <- df$value
  out
}

#' Convert a bleaching fraction to a bleaching rate
#'
#' Given a bleaching simulation of `n_time_points` time-points, this function
#' tells you the bleaching rate that will result in the final time-point having
#' bleached by a fraction `frac` relative to the first time-point
#'
#' @param frac A number. The fraction of bleaching desired for the final
#'   time-point relative to the first. So if you want 20\% bleaching by the final
#'   time-point (i.e. 80\% of fluorescence remains), set `frac = 0.2`.
#' @param n_time_points A natural number. The number of time-points in the
#'   simulation.
#'
#' @return A number.
#'
#' @examples
#' bleach_fraction_to_rate(0.2, 1000)  # 20% bleaching over 1000 time-points
#'
#' @export
bleach_fraction_to_rate <- function(frac, n_time_points) {
  1 - (1 - frac) ^ (1 / n_time_points)
}
