#' Tidy select for list
#'
#' A tidy select interface for lists. See [tidyselect::eval_select()]
#' for details.
#'
#' @param .l A [list()] object.
#' @param ... One or more unquoted expressions separated by commas.
#' @return A list with the selected elements.
#' @export
select_list <- function(.l, ...) {
  pos <- eval_select(expr(c(...)), .l)
  set_names(.l[pos], names(pos))
}
