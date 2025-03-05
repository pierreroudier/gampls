#' gamplsInfo
#'
#' @title a GAM-PLS model info to use in caret::train
#'
#' @author Pierre Roudier
#'
#' @include formula.R lvs.R
#' @importFrom pls plsr
#' @importFrom mgcv gam predict.gam
#' @export gamplsInfo
#' @export
#'
#'@examples
#'\dontrun{
#'
#'library(spectacles)
#'library(caret)
#' oz <- load_oz()
#'
#' fit <- train(
#'   x = spectra(oz),
#'   y = oz$carbon,
#'   method = gamplsInfo,
#'   tuneGrid = expand.grid(
#'     ncomp = 3:10,
#'     select = FALSE,
#'     method = "GCV.Cp"
#'   ),
#'   trControl = trainControl(
#'     method = "repeatedcv",
#'     number = 10,
#'     repeats = 5,
#'     # Here we can specify the summary function used during parametrisation
#'     summaryFunction = spectroSummary
#'  ),
#'   # Here we can specify which metric to use to optimise the model parameters
#'  metric = "RPIQ"
#' )
#'
#'}
#'
gamplsInfo <- list(

  label = "GAM-PLS",

  library = "gampls",

  type = c("Regression"),

  parameters = data.frame(
    parameter = c("ncomp", "select", "method"),
    class = c("numeric", "logical", "character"),
    label = c("Number of latent variables", "Feature Selection", "Method")
  ),

  grid = function (x, y, len = NULL, search = "grid") {

    if (search == "grid") {
      out <- expand.grid(
        ncomp = seq(
          1,
          min(ncol(x) - 1, len),
          by = 1
        ),
        select = c(TRUE, FALSE)[1:min(2, len)],
        method = "GCV.Cp"
      )
    }

    else {
      out <- data.frame(
        ncomp = unique(sample(1:ncol(x), replace = TRUE)),
        select = sample(c(TRUE, FALSE), size = len, replace = TRUE),
        method = sample(c("GCV.Cp", "ML"), size = len, replace = TRUE)
      )
    }

    return(out)
  },

  fit = function (x, y, wts, param, lev, last, classProbs, ...) {

    #
    # PLS part
    #

    require(pls)

    ncomp <- min(ncol(x), param$ncomp)

    if (is.factor(y)) stop("classsification is not supported by GAMPLS")

    if (is.data.frame(x)) {
      dat <- x
    } else {
      dat <- as.data.frame(x, stringsAsFactors = TRUE)
    }

    # Set outcome vector
    dat$.outcome <- y

    fit_pls <- pls::plsr(
      .outcome ~ .,
      data = dat,
      method = "oscorespls",
      ncomp = ncomp
    )

    #
    # EXTRACT LATENT VARIABLES
    #

    # Extract the LVs from the model
    lvs <- predict_pls(
      object = fit_pls,
      newdata = dat,
      ncomp = 1:ncomp,
      type = "scores"
    )

    dat_lvs <- data.frame(
      .outcome = dat$.outcome,
      lvs
    )

   #
    # TRAIN GAM
    #

    require(mgcv)

    # Assemble GAM formula
    modForm <-.mySmootherFormula(dat_lvs[, -1])

    # Set default family
    default_distr <- gaussian()

    modelArgs <- list(
      formula = modForm,
      data = dat_lvs,
      select = param$select,
      method = as.character(param$method)
    )

    theDots <- list(...)

    # Sets default family if not specified in the dots args
    if (!any(names(theDots) == "family")) {
      modelArgs$family <- default_distr
    }

    modelArgs <- c(modelArgs, theDots)

    fit_gam <- do.call(mgcv::gam, modelArgs)

    # class(out) <- c("gampls", class(res))

    out <- list(pls = fit_pls, gam = fit_gam)

    return(out)
  },

  predict = function (modelFit, newdata, submodels = NULL) {

    #
    # EXTRACT MODELS
    #

    fit_pls <- modelFit$pls
    fit_gam <- modelFit$gam

    #
    # PREDICT LATENT VARIABLES
    #

    lvs <- predict_pls(
      fit_pls,
      newdata = newdata,
      type = "scores"
    )

    lvs <- as.data.frame(lvs)
    names(lvs) <- sub(" ", ".", names(lvs))

    out <- predict.gam(fit_gam, lvs, type = "response")

    out
  },

  prob = NULL,

  # tags = c("Generalized Linear Model", "Generalized Additive Model"),

  # levels = function (x) x$obsLevels,

  sort = function(x) x
)
