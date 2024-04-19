#' @name make_formula
#'
#' @title Generate a formula for the model
#'
#' @param attr Outcome of the model
#' @param nms Variable names
#' @returns A formula
#'
#' @importFrom stats as.formula
#' @author Pierre Roudier
#' @noRd
make_formula <- function(
    attr,
    nms
  ){
  fm <- as.formula(
    paste0(
      attr,
      " ~ ",
      paste0(
        "s(",
        nms,
        ")",
        collapse = " + "
      )
    )
  )
}
