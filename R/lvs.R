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

  if (class(fit) != "train") stop("fit needs to be a train object.", call. = FALSE)

  model_type <- fit$modelInfo$label

  if (model_type == "Partial Least Squares") {
    fit_pls <- fit$finalModel
  } else if (model_type == "GAM-PLS") {
    fit_pls <- fit$finalModel$pls
  } else {
    stop("No GAM-PLS implementation for this type of model", call. = FALSE)
  }

  lvs <- predict_pls(
    fit_pls,
    newdata = data,
    type = "scores"
  )

  lvs <- data.frame(lvs)

  return(lvs)
}
