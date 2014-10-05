#' Criteria Table
#'
#' This function accepts a linear model as an input
#' and returns a bunch of useful values for model selection.
#' Included values: df, r.squared, SSE, RMSE, RMSP, AIC and BIC.
#' @param model The model you want to evaluate.
#' @keywords model df sse rmse rmsp aic bic
#' @export
#' @examples
#' criteriaTable()

criteriaTable <- function(model) {
  summ <- summary(model)
  anova <- anova(model)
  df <- data.frame(name = deparse(substitute(model)),
                   df = model$df,
                   r.squared = summ$r.squared,
                   SSE = last(last(anova[2])),
                   RMSE = summ$sigma,
                   RMSP = rmsp(model),
                   AIC = extractAIC(model)[2],
                   BIC = extractAIC(model, k = log(model$residuals))[2])
  return(df)
}