#' Separate a column into multiple columns
#'
#' This is a wrapper around [tidyr::separate_wider_regex()] that allows to split
#' a column into multiple columns. The column contains so-called delimiter
#' separated values (DSV) and the values are extracted using regular
#' expressions.
#'
#' @param data A data frame.
#' @param col <[`tidy-select`][tidyr_tidy_select]> Column to separate.
#' @param names Names of the new columns. Use `NA` if you want a component not
#'   to be in the output.
#' @param ... Additional arguments passed to [tidyr::separate_wider_regex()].
#' @param patterns Regular expressions to extract the values from the column. If
#'   `NULL`, the pattern will match any character non-greedily. The length of
#'   the vector must be equal to the number of new columns.
#' @param delim Delimiter used in the column to separate different pieces of
#'   values.
#' @param prefix,suffix Prefix and suffix to be removed from the target column
#'   to retrieve the values.
#' @return A data frame with the separated columns.
#' @export
separate_wider_dsv <- function(data, col, names, ...,
                               patterns = NULL,
                               delim = "_",
                               prefix = NULL,
                               suffix = NULL) {
  check_dots_used()
  if (is.null(patterns)) {
    patterns <- rep(".*?", length(names))
  }
  patterns <- compose_patterns(names, patterns, delim)
  if (!is.null(prefix)) {
    patterns <- c(
      prefix,
      stringr::str_c(delim, "?+"),
      patterns
    )
  }
  if (!is.null(suffix)) {
    patterns <- c(
      patterns,
      stringr::str_c(delim, "?+"),
      suffix
    )
  }
  tidyr::separate_wider_regex(data, {{ col }}, patterns, ...) |>
    # workaround for https://github.com/tidyverse/tidyr/issues/1513
    dplyr::mutate(
      dplyr::pick(all_of(names[!is.na(names)])) |>
        utils::type.convert(as.is = TRUE)
    )
}

compose_patterns <- function(names, patterns, delim) {
  out <- rep(delim, 2 * length(names) - 1)
  out[seq(1, length(out), 2)] <- patterns
  out_names <- character(2 * length(names) - 1)
  out_names[seq(1, length(out), 2)] <- names
  set_names(out, out_names)
}
