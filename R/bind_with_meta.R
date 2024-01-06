#' Combine data with metadata
#'
#' This is a wrapper around `dplyr::bind_rows()` that allows to combine
#' multiple data frames with metadata. The metadata is extracted from the
#' names of the data frames. The names of the data frames should be of the
#' form `prefix_meta1_meta2_...` given the delimiter is set as `_`. The
#' metadata columns are then extracted and converted to the appropriate
#' types.
#'
#' @param ... Data frames to combine. The names of the data frames should
#'   contain the metadata.
#' @param .names_meta Character vector of names of the metadata columns. If
#'   `NULL`, the metadata parsing is skipped.
#' @param .prefix Prefix to be removed from the target names to retrieve the
#'   metadata.
#' @param .fun_pre,.fun_post Functions to apply to the data before and after
#'   combining.
#' @param .delim_name Delimiter used in the names of the data frames to separate
#'   the metadata.
#' @return A data frame with the combined data and metadata.
#' @export
bind_with_meta <- function(...,
                           .names_meta = NULL,
                           .prefix = NULL,
                           .fun_pre = NULL,
                           .fun_post = NULL,
                           .delim_name = "_") {
  check_dots_used()
  x <- list(...)
  if (!is.null(.fun_pre)) {
    x <- lapply(x, as_function(.fun_pre))
  }
  if (!is.null(.names_meta)) {
    name_id <- ".id"
    data <- dplyr::bind_rows(x, .id = name_id)
    if (!is.null(.prefix)) {
      data[[name_id]] <- stringr::str_remove(
        data[[name_id]],
        paste0(.prefix, .delim_name)
      )
    }
    data <- data |>
      tidyr::separate(
        all_of(name_id),
        .names_meta,
        convert = TRUE
      )
  } else {
    data <- dplyr::bind_rows(x)
  }
  if (!is.null(.fun_post)) {
    data <- as_function(.fun_post)(data)
  }
  data
}
