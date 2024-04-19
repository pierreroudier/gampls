#' simulate_gam
#'
#' @importFrom utils getFromNamespace
#' @noRd
simulate_gam <- utils::getFromNamespace("simulate.gam", "gratia")

#' @name simulate
#' @title Simulate from the posterior distribution using {gratia}
#' @description
#' Simulations from the posterior distribution of the GAM fitted from the latent variables of the original PLS regression model. This uses \code{gratia::simulate.gam}.
#'
#' @param fit A GAM fitted on the latent variables of a PLS
#' @param n numeric; the number of posterior simulations to return
#' @param data data.frame; observations for which the posterior draws from the models should be evaluated.
#' @param ... Fyurther arguments passed to \code{gratia::simulate.gam}
#'
#' @return A matrix with \code{nsim} columns.
#'
#' @details
#' This is a simple wrapper around \code{gratia::simulate.gam}, see the dedicated help page.
#'
#' @author Pierre Roudier
#' @export
#'
simulate <- function(
    fit,
    n,
    data,
    ...
) {

  # data needs to be LVs in this instance!
  # data <- get_lvs(fit, data)

  sim <- simulate_gam(
    fit,
    nsim = n,
    data = data,
    ...
  )

  return(sim)
}

#' bru_summarise
#'
#' @importFrom utils getFromNamespace
#' @noRd
bru_summarise <- utils::getFromNamespace("bru_summarise", "inlabru")

#' summarise_preds
#'
#' @title Summarise predictions from a GAM-PLS
#'
#' @param sim A \code{data.frame} of the posterior samples, generated using \code{simulate}
#' @param probs A numeric vector of probabilities with values in \code{[0, 1]}, passed to \code{stats::quantile}
#' @param ... Further arguments passed to \code{inlabru::bru_summarise}
#'
#' @returns A \code{data.frame} with summary statistics, \code{mean}, \code{sd}, the quantiles as per the \code{probs} argument, and the MC statistics \code{mean.mc_std_err} and \code{sd.mc_std_err}
#'
#' @details
#' The function is a simple wrapper around \code{inlabru::bru_summarise}
#'
#' @author Pierre Roudier
#' @importFrom inlabru bru_summarise
#' @export
#'
summarise_preds <- function(
    sim,
    probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975),
    ...
) {
  preds <- bru_summarise(
    sim,
    probs = probs,
    ...
  )

  return(preds)
}

#' predict
#'
#' @title Prediction from a fitted GAM-PLS model
#'
#' @param object a fitted GAM-PLS model, typically an output of \code{gampls}
#' @param newdata A \code{data.frame} or list containing the values of the model covariates at which predictions are required.
#' @param type See \code{mgcv::predict.gam}, defaults to \code{"response"}
#' @param ... Further arguments passed to \code{mgcv::predict.gam}
#'
#' @returns See \code{mgcv::predict.gam}
#'
#' @details
#' This is a simple wrapper around \code{mgcv::predict.gam}.
#'
#' @author Pierre Roudier
#'
#' @include formula.R
#' @importFrom stats predict
#' @importFrom mgcv predict.gam
#' @export predict.gampls
#' @export
#'
predict.gampls <- function(object, newdata, type = "response", ...) {

  # newdata needs to be LVs in this instance!

  res <- predict.gam(
    object,
    newdata,
    type = type,
    ...
  )

  return(res)
}
