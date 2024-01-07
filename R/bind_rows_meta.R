#' Combine multiple data frames by row with metadata from names
#'
#' This is a wrapper around [dplyr::bind_rows()] that allows to combine multiple
#' data frames with metadata. The metadata is extracted from the names of the
#' data frames. The names of the data frames should be of the form
#' `prefix_meta1_meta2_...` given the delimiter is set as `_`. The metadata
#' columns are then extracted and converted to the appropriate types.
#'
#' @param ... Data frames to combine. Typically, they are named appropriately to
#'   extract the metadata.
#' @param .names_meta,.patterns_meta Character vectors of names and patterns to
#'   extract the metadata from the names of the data frames. The `.names_meta`
#'   is used to name the metadata columns and `.patterns_meta` is used to
#'   extract the values. If `.names_meta` is `NULL`, the metadata parsing is
#'   skipped. If `.patterns_meta` is `NULL`, they are assumed to be `".*"`.
#' @param .delim Delimiter used in the names of the data frames to separate
#'   different pieces of metadata.
#' @param .prefix Prefix to be removed from the target names to retrieve the
#'   metadata.
#' @return A data frame with the combined data and metadata.
#' @export
bind_rows_meta <- function(...,
                           .names_meta = NULL,
                           .patterns_meta = NULL,
                           .delim = "_",
                           .prefix = NULL) {
  data <- dplyr::bind_rows(..., .id = ".id")
  if (is.null(.names_meta)) {
    return(dplyr::select(data, !".id"))
  }
  .patterns <- compose_patterns(.names_meta, .patterns_meta, .delim)
  if (!is.null(.prefix)) {
    .patterns <- c(
      .prefix,
      stringr::str_c(.delim, "?"),
      .patterns
    )
  }
  data |>
    tidyr::separate_wider_regex(".id", .patterns) |>
    # workaround for https://github.com/tidyverse/tidyr/issues/1513
    dplyr::mutate(
      dplyr::across(
        any_of(names(.patterns)),
        \(col) utils::type.convert(col, as.is = TRUE)
      )
    )
}

compose_patterns <- function(.names, .patterns, .delim) {
  if (is.null(.patterns)) {
    .patterns <- rep(".*", length(.names))
  }
  out <- rep(.delim, 2 * length(.names) - 1)
  out[seq(1, length(out), 2)] <- .patterns
  names <- character(2 * length(.names) - 1)
  names[seq(1, length(out), 2)] <- .names
  set_names(out, names)
}
