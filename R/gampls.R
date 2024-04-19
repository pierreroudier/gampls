#' fit_gampls
#'
#' @title GAM-PLS model using MGCV
#'
#' @param fit .
#' @param ... .
#'
#' @returns .
#'
#' @author Pierre Roudier
#'
#' @importFrom methods is
#' @importFrom mgcv gam
#' @include lvs.R formula.R
#' @export
#'
gampls <- function(
    fit,
    ...
) {

  if (!is(fit, "train"))
    stop("'fit' must be a 'train' object, typically an output of `caret::train`.", call. = FALSE)

  if (!is(fit$finalModel, "mvr"))
    stop("'fit' must be using a PLS model 'train'. \nUse `caret::train(..., method = 'pls')`.", call. = FALSE)

  # Extract the calibration set used for the PLS
  df_calib <- fit$trainingData

  # Extract the LVs from the model
  lvs <- get_lvs(fit = fit, data = df_calib)

  df_lvs <- data.frame(
    .outcome = df_calib$.outcome,
    lvs
  )

  # Covariable names
  nms <- names(df_lvs)[-1]

  # Assemble GAM formula
  fm <- make_formula(
    attr = ".outcome",
    nms
  )

  # Train GAM
  res <- gam(
    fm,
    data = df_lvs,
    ...
  )

  class(res) <- c("gampls", class(res))

  return(res)
}
