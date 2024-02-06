#' Create a call to a function with its arguments
#'
#' This is basically a wrapper around [rlang::call2()] that allows you to
#' extract the arguments from a function and pass them to [rlang::call2()]
#' without having to type them out.
#'
#' @param .fn The function to call.
#' @param ... Arguments to pass to the function.
#' @return A call to the function with its arguments.
#' @export
call_full <- function(.fn, ...) {
  defaults <- syms_args(.fn)
  dots <- enexprs(...)
  dots <- utils::modifyList(defaults, dots)
  call2(.fn, !!!dots)
}

syms_args <- function(.fn) {
  args <- fn_fmls_names(as_function(.fn))
  set_names(syms(args), args)
}
