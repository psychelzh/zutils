#' Cautiously evaluate an expression
#'
#' This function is useful for when you want to evaluate an expression, but
#' you want to catch any errors and return a default value instead. Note the
#' error message will be printed as a warning as the name of the function
#' suggests.
#'
#' @param .f A function to modify. See [as_function()] for details.
#' @param otherwise A value to return if the expression throws an error.
#' @return A function that evaluates the expression and returns the result
#'   or the default value if an error is thrown.
#' @export
cautiously <- function(.f, otherwise = NULL) {
  .f <- as_function(.f)
  force(otherwise)
  function(...) {
    tryCatch(
      .f(...),
      error = function(e) {
        warning("Error: ", conditionMessage(e))
        otherwise
      }
    )
  }
}
