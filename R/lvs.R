#' predict_pls
#'
#' @importFrom utils getFromNamespace
#' @noRd
predict_pls <- utils::getFromNamespace("predict.mvr", "pls")

#' @name get_lvs
#'
#' @title Get Latent Variables
#'
#' @param fit a \code{train} object
#' @param data a \code{data.frame}
#'
#' @returns a \code{data.frame} of latent variables
#'
#' @author Pierre Roudier
#'
#' @noRd
#'
get_lvs <- function(fit, data) {
  lvs <- predict_pls(
    fit$finalModel,
    newdata = data,
    type = "scores"
  )

  lvs <- data.frame(lvs)

  return(lvs)
}
