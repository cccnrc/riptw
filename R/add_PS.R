#' Add propensity scores to a dataframe for a specific grouping variable
#'
#' This function add propensity score (PS) columns to the input data.frame object
#' you get a PS column for each factor of the passed outcome in the formula
#' with a specified prefix (default "PS_")
#'
#' @param data: the data.frame object for the analysis
#' @param formula: the regression model formula
#' @param prefix: the propensity score columns prefix to use (default: "PS_")
#' @param family: the family of regression to be used in vglm (default: "multinomial")
#' @return A data.frame object which is the original data.frame passed with a PS column added
add_PS <- function(data, formula, prefix = "PS_", family = VGAM::multinomial(parallel = FALSE)) {
  ## Fit multinomial logistic regression
  resVglm <- VGAM::vglm(formula = formula, data = data, family = family)
  ## Calculate PS
  psData <- as.data.frame(VGAM::predict(resVglm, type = "response"))
  names(psData) <- paste0(prefix, names(psData))
  ## Add to data
  DB_OUTPUT <- cbind(data, psData)
  return( DB_OUTPUT )
}
