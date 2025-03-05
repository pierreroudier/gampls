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

# Custom caret function to be able to pass somoothers to caret::train
.mySmootherFormula <- function (data, smoother = "s", cut = 10, df = 0, span = 0.5, degree = 1, y = ".outcome") {

  # nzv <- caret::nearZeroVar(data)
  # if (length(nzv) > 0) data <- data[, -nzv, drop = FALSE]

  # numValues <- sort(apply(data, 2, function(x) length(unique(x))))
  numValues <- sapply(data, is.numeric)

  prefix <- rep("", ncol(data))
  suffix <- rep("", ncol(data))

  # prefix[numValues > cut] <- paste(smoother, "(", sep = "")
  prefix[which(numValues)] <- paste(smoother, "(", sep = "")

  if (smoother == "s") {
    suffix[which(numValues)] <- if (df == 0)
      ")"
    else paste(", df=", df, ")", sep = "")
  }
  if (smoother == "lo") {
    suffix[which(numValues)] <- paste(", span=", span, ",degree=",
                                      degree, ")", sep = "")
  }
  if (smoother == "rcs") {
    suffix[which(numValues)] <- ")"
  }
  rhs <- paste(prefix, names(numValues), suffix, sep = "")
  rhs <- paste(rhs, collapse = "+")
  form <- as.formula(paste(y, rhs, sep = "~"))
  form
}
