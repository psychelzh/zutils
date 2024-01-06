#' Static aggregation with metadata
#'
#' Like [tarchetypes::tar_combine()] except it adds metadata columns to the
#' combined data frame.
#'
#' This function is useful for combining data frames. For [list()] targets,
#' you should pass a function to convert the list to a data frame in the
#' `fun_pre` argument. Metadata is retrieved from the names of the targets.
#'
#' @param name Name of the target. Could be a string or symbol.
#' @param cols_meta Character vector of column names to store the retrieved
#'   metadata.
#' @param ... One or more target objects or list of target objects. See
#'   [tarchetypes::tar_combine()].
#' @param prefix Prefix to be removed from the target names to retrieve the
#'   metadata. Defaults is the same as `name`.
#' @param fun_pre Function to apply to each target before combining.
#' @param fun_post Function to apply to the combined data frame.
#' @return A target object. See [tarchetypes::tar_combine()].
#' @export
tar_combine_with_meta <- function(name, cols_meta, ...,
                                  prefix = NULL,
                                  fun_pre = identity,
                                  fun_post = identity) {
  check_dots_used()
  ischar_name <- tryCatch(
    is.character(name) && length(name) == 1L,
    error = function(e) FALSE
  )
  if (!ischar_name) name <- deparse1(substitute(name))
  if (is.null(prefix)) prefix <- name
  tarchetypes::tar_combine_raw(
    name,
    ...,
    command = bquote(
      list(!!!.x) |>
        lapply(.(as_function(fun_pre))) |>
        bind_rows(.id = "id") |>
        # note there is delimiter after prefix should be removed too
        mutate(id = str_remove(id, str_c(.(prefix), "."))) |>
        separate(id, .(cols_meta), convert = TRUE) |>
        .(as_function(fun_post))()
    )
  )
}
