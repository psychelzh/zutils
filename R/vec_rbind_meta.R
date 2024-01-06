#' Combine multiple data frames row-wise with metadata
#'
#' This is a wrapper around [vctrs::vec_rbind()] that allows to combine
#' multiple data frames with metadata. The metadata is extracted from the
#' names of the data frames. The names of the data frames should be of the
#' form `prefix_meta1_meta2_...` given the delimiter is set as `_`. The
#' metadata columns are then extracted and converted to the appropriate
#' types.
#'
#' @param ... Data frames to combine. Typically, they are named appropriately
#'   to extract the metadata.
#' @param .names_meta Character vector of names of the metadata columns. If
#'   `NULL`, the metadata parsing is skipped. If set, you should make sure
#'   that the data frames are named appropriately.
#' @param .prefix Prefix to be removed from the target names to retrieve the
#'   metadata. If `NULL`, the parsed columns with values that are the same
#'   across all data frames are removed.
#' @param .fun_pre,.fun_post Functions to apply to the data before and after
#'   combining. Note they should return values of [data.frame()] class.
#' @param .delim_name Delimiter used in the names of the data frames to separate
#'   the metadata.
#' @return A data frame with the combined data and metadata.
#' @export
vec_rbind_meta <- function(...,
                           .names_meta = NULL,
                           .prefix = NULL,
                           .fun_pre = NULL,
                           .fun_post = NULL,
                           .delim_name = "_") {
  x <- list2(...)
  if (!is.null(.fun_pre)) {
    x <- lapply(x, as_function(.fun_pre))
  }

  if (!is.null(.names_meta)) {
    name_id <- ".id"
    data <- vctrs::vec_rbind(!!!x, .names_to = name_id) |>
      dplyr::mutate(
        parse_meta(.data[[name_id]], .names_meta, .prefix, .delim_name),
        .keep = "unused",
        .before = 1
      )
  } else {
    data <- vctrs::vec_rbind(!!!x)
  }
  if (!is.null(.fun_post)) {
    data <- as_function(.fun_post)(data)
  }
  data
}

parse_meta <- function(meta, .names_meta, .prefix, .delim_name) {
  if (!is.null(.prefix)) {
    meta <- stringr::str_remove(meta, .prefix)
  }
  meta_parsed <- stringr::str_split(
    meta, .delim_name,
    simplify = TRUE
  )
  if (is.null(.prefix)) {
    meta_parsed <- meta_parsed[
      ,
      lengths(apply(meta_parsed, 2, unique, simplify = FALSE)) > 1
    ]
  }
  stopifnot(
    "Column number of metadata does not match input `.names_meta`." =
      ncol(meta_parsed) == length(.names_meta)
  )
  meta_parsed |>
    tibble::as_tibble(.name_repair = \(x) .names_meta) |>
    utils::type.convert(as.is = TRUE)
}
