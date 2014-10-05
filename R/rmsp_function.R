#' RMSP
#'
#' This function accepts a model object as input and returns
#' the 'RMSP' (root-mean-square-PRESS) statistic for that model.
#' @param model A model object.
#' @keywords rmsp model press
#' @export
#' @examples
#' rmsp()

rmsp <- function (model) {
  n <- length(model$residuals)
  press <- sum((resid(model)/(1-hatvalues(model)))^2)
  rmsp <- sqrt(press/n)
  rmsp
}