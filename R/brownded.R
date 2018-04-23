#' @import checkmate
#' @importFrom magrittr '%>%' '%<>%' '%T>%'
#' @importFrom rlang '!!' ':='
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "x1", "x2", "x3", "value", "id", "n",
                           "px_horizontal", "px_vertical", "pixel_value"))
}
